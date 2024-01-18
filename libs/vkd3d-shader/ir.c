/*
 * Copyright 2023 Conor McCarthy for CodeWeavers
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA
 */

#include "vkd3d_shader_private.h"

bool vsir_program_init(struct vsir_program *program, const struct vkd3d_shader_version *version, unsigned int reserve)
{
    program->shader_version = *version;
    return shader_instruction_array_init(&program->instructions, reserve);
}

void vsir_program_cleanup(struct vsir_program *program)
{
    size_t i;

    for (i = 0; i < program->block_name_count; ++i)
        vkd3d_free((void *)program->block_names[i]);
    vkd3d_free(program->block_names);
    shader_instruction_array_destroy(&program->instructions);
}

static inline bool shader_register_is_phase_instance_id(const struct vkd3d_shader_register *reg)
{
    return reg->type == VKD3DSPR_FORKINSTID || reg->type == VKD3DSPR_JOININSTID;
}

static bool shader_instruction_is_dcl(const struct vkd3d_shader_instruction *ins)
{
    return (VKD3DSIH_DCL <= ins->handler_idx && ins->handler_idx <= VKD3DSIH_DCL_VERTICES_OUT)
            || ins->handler_idx == VKD3DSIH_HS_DECLS;
}

static void vkd3d_shader_instruction_make_nop(struct vkd3d_shader_instruction *ins)
{
    struct vkd3d_shader_location location = ins->location;

    vsir_instruction_init(ins, &location, VKD3DSIH_NOP);
}

static void remove_dcl_temps(struct vsir_program *program)
{
    unsigned int i;

    for (i = 0; i < program->instructions.count; ++i)
    {
        struct vkd3d_shader_instruction *ins = &program->instructions.elements[i];

        if (ins->handler_idx == VKD3DSIH_DCL_TEMPS)
            vkd3d_shader_instruction_make_nop(ins);
    }
}

static void shader_register_eliminate_phase_addressing(struct vkd3d_shader_register *reg,
        unsigned int instance_id)
{
    unsigned int i;

    for (i = 0; i < reg->idx_count; ++i)
    {
        if (reg->idx[i].rel_addr && shader_register_is_phase_instance_id(&reg->idx[i].rel_addr->reg))
        {
            reg->idx[i].rel_addr = NULL;
            reg->idx[i].offset += instance_id;
        }
    }
}

static void shader_instruction_eliminate_phase_instance_id(struct vkd3d_shader_instruction *ins,
        unsigned int instance_id)
{
    struct vkd3d_shader_register *reg;
    unsigned int i;

    for (i = 0; i < ins->src_count; ++i)
    {
        reg = (struct vkd3d_shader_register *)&ins->src[i].reg;
        if (shader_register_is_phase_instance_id(reg))
        {
            vsir_register_init(reg, VKD3DSPR_IMMCONST, reg->data_type, 0);
            reg->u.immconst_u32[0] = instance_id;
            continue;
        }
        shader_register_eliminate_phase_addressing(reg, instance_id);
    }

    for (i = 0; i < ins->dst_count; ++i)
        shader_register_eliminate_phase_addressing((struct vkd3d_shader_register *)&ins->dst[i].reg, instance_id);
}

static const struct vkd3d_shader_varying_map *find_varying_map(
        const struct vkd3d_shader_varying_map_info *varying_map, unsigned int signature_idx)
{
    unsigned int i;

    for (i = 0; i < varying_map->varying_count; ++i)
    {
        if (varying_map->varying_map[i].output_signature_index == signature_idx)
            return &varying_map->varying_map[i];
    }

    return NULL;
}

static enum vkd3d_result remap_output_signature(struct vkd3d_shader_parser *parser,
        const struct vkd3d_shader_compile_info *compile_info)
{
    struct shader_signature *signature = &parser->shader_desc.output_signature;
    const struct vkd3d_shader_varying_map_info *varying_map;
    unsigned int i;

    if (!(varying_map = vkd3d_find_struct(compile_info->next, VARYING_MAP_INFO)))
        return VKD3D_OK;

    for (i = 0; i < signature->element_count; ++i)
    {
        const struct vkd3d_shader_varying_map *map = find_varying_map(varying_map, i);
        struct signature_element *e = &signature->elements[i];

        if (map)
        {
            unsigned int input_mask = map->input_mask;

            e->target_location = map->input_register_index;

            /* It is illegal in Vulkan if the next shader uses the same varying
             * location with a different mask. */
            if (input_mask && input_mask != e->mask)
            {
                vkd3d_shader_parser_error(parser, VKD3D_SHADER_ERROR_VSIR_NOT_IMPLEMENTED,
                        "Aborting due to not yet implemented feature: "
                        "Output mask %#x does not match input mask %#x.",
                        e->mask, input_mask);
                return VKD3D_ERROR_NOT_IMPLEMENTED;
            }
        }
        else
        {
            e->target_location = SIGNATURE_TARGET_LOCATION_UNUSED;
        }
    }

    for (i = 0; i < varying_map->varying_count; ++i)
    {
        if (varying_map->varying_map[i].output_signature_index >= signature->element_count)
        {
            vkd3d_shader_parser_error(parser, VKD3D_SHADER_ERROR_VSIR_NOT_IMPLEMENTED,
                    "Aborting due to not yet implemented feature: "
                    "The next stage consumes varyings not written by this stage.");
            return VKD3D_ERROR_NOT_IMPLEMENTED;
        }
    }

    return VKD3D_OK;
}

struct hull_flattener
{
    struct vkd3d_shader_instruction_array instructions;

    unsigned int instance_count;
    unsigned int phase_body_idx;
    enum vkd3d_shader_opcode phase;
    struct vkd3d_shader_location last_ret_location;
};

static bool flattener_is_in_fork_or_join_phase(const struct hull_flattener *flattener)
{
    return flattener->phase == VKD3DSIH_HS_FORK_PHASE || flattener->phase == VKD3DSIH_HS_JOIN_PHASE;
}

struct shader_phase_location
{
    unsigned int index;
    unsigned int instance_count;
    unsigned int instruction_count;
};

struct shader_phase_location_array
{
    /* Unlikely worst case: one phase for each component of each output register. */
    struct shader_phase_location locations[MAX_REG_OUTPUT * VKD3D_VEC4_SIZE];
    unsigned int count;
};

static void flattener_eliminate_phase_related_dcls(struct hull_flattener *normaliser,
        unsigned int index, struct shader_phase_location_array *locations)
{
    struct vkd3d_shader_instruction *ins = &normaliser->instructions.elements[index];
    struct shader_phase_location *loc;
    bool b;

    if (ins->handler_idx == VKD3DSIH_HS_FORK_PHASE || ins->handler_idx == VKD3DSIH_HS_JOIN_PHASE)
    {
        b = flattener_is_in_fork_or_join_phase(normaliser);
        /* Reset the phase info. */
        normaliser->phase_body_idx = ~0u;
        normaliser->phase = ins->handler_idx;
        normaliser->instance_count = 1;
        /* Leave the first occurrence and delete the rest. */
        if (b)
            vkd3d_shader_instruction_make_nop(ins);
        return;
    }
    else if (ins->handler_idx == VKD3DSIH_DCL_HS_FORK_PHASE_INSTANCE_COUNT
            || ins->handler_idx == VKD3DSIH_DCL_HS_JOIN_PHASE_INSTANCE_COUNT)
    {
        normaliser->instance_count = ins->declaration.count + !ins->declaration.count;
        vkd3d_shader_instruction_make_nop(ins);
        return;
    }
    else if (ins->handler_idx == VKD3DSIH_DCL_INPUT && shader_register_is_phase_instance_id(
            &ins->declaration.dst.reg))
    {
        vkd3d_shader_instruction_make_nop(ins);
        return;
    }

    if (normaliser->phase == VKD3DSIH_INVALID || shader_instruction_is_dcl(ins))
        return;

    if (normaliser->phase_body_idx == ~0u)
        normaliser->phase_body_idx = index;

    if (ins->handler_idx == VKD3DSIH_RET)
    {
        normaliser->last_ret_location = ins->location;
        vkd3d_shader_instruction_make_nop(ins);
        if (locations->count >= ARRAY_SIZE(locations->locations))
        {
            FIXME("Insufficient space for phase location.\n");
            return;
        }
        loc = &locations->locations[locations->count++];
        loc->index = normaliser->phase_body_idx;
        loc->instance_count = normaliser->instance_count;
        loc->instruction_count = index - normaliser->phase_body_idx;
    }
}

static enum vkd3d_result flattener_flatten_phases(struct hull_flattener *normaliser,
        struct shader_phase_location_array *locations)
{
    struct shader_phase_location *loc;
    unsigned int i, j, k, end, count;

    for (i = 0, count = 0; i < locations->count; ++i)
        count += (locations->locations[i].instance_count - 1) * locations->locations[i].instruction_count;

    if (!shader_instruction_array_reserve(&normaliser->instructions, normaliser->instructions.count + count))
        return VKD3D_ERROR_OUT_OF_MEMORY;
    end = normaliser->instructions.count;
    normaliser->instructions.count += count;

    for (i = locations->count; i > 0; --i)
    {
        loc = &locations->locations[i - 1];
        j = loc->index + loc->instruction_count;
        memmove(&normaliser->instructions.elements[j + count], &normaliser->instructions.elements[j],
                (end - j) * sizeof(*normaliser->instructions.elements));
        end = j;
        count -= (loc->instance_count - 1) * loc->instruction_count;
        loc->index += count;
    }

    for (i = 0, count = 0; i < locations->count; ++i)
    {
        loc = &locations->locations[i];
        /* Make a copy of the non-dcl instructions for each instance. */
        for (j = 1; j < loc->instance_count; ++j)
        {
            for (k = 0; k < loc->instruction_count; ++k)
            {
                if (!shader_instruction_array_clone_instruction(&normaliser->instructions,
                        loc->index + loc->instruction_count * j + k, loc->index + k))
                    return VKD3D_ERROR_OUT_OF_MEMORY;
            }
        }
        /* Replace each reference to the instance id with a constant instance id. */
        for (j = 0; j < loc->instance_count; ++j)
        {
            for (k = 0; k < loc->instruction_count; ++k)
                shader_instruction_eliminate_phase_instance_id(
                        &normaliser->instructions.elements[loc->index + loc->instruction_count * j + k], j);
        }
    }

    return VKD3D_OK;
}

void vsir_register_init(struct vkd3d_shader_register *reg, enum vkd3d_shader_register_type reg_type,
        enum vkd3d_data_type data_type, unsigned int idx_count)
{
    reg->type = reg_type;
    reg->precision = VKD3D_SHADER_REGISTER_PRECISION_DEFAULT;
    reg->non_uniform = false;
    reg->data_type = data_type;
    reg->idx[0].offset = ~0u;
    reg->idx[0].rel_addr = NULL;
    reg->idx[0].is_in_bounds = false;
    reg->idx[1].offset = ~0u;
    reg->idx[1].rel_addr = NULL;
    reg->idx[1].is_in_bounds = false;
    reg->idx[2].offset = ~0u;
    reg->idx[2].rel_addr = NULL;
    reg->idx[2].is_in_bounds = false;
    reg->idx_count = idx_count;
    reg->dimension = VSIR_DIMENSION_SCALAR;
    reg->alignment = 0;
}

void vsir_src_param_init(struct vkd3d_shader_src_param *param, enum vkd3d_shader_register_type reg_type,
        enum vkd3d_data_type data_type, unsigned int idx_count)
{
    vsir_register_init(&param->reg, reg_type, data_type, idx_count);
    param->swizzle = 0;
    param->modifiers = VKD3DSPSM_NONE;
}

void vsir_src_param_init_label(struct vkd3d_shader_src_param *param, unsigned int label_id)
{
    vsir_src_param_init(param, VKD3DSPR_LABEL, VKD3D_DATA_UINT, 1);
    param->reg.dimension = VSIR_DIMENSION_NONE;
    param->reg.idx[0].offset = label_id;
}

void vsir_instruction_init(struct vkd3d_shader_instruction *ins, const struct vkd3d_shader_location *location,
        enum vkd3d_shader_opcode handler_idx)
{
    memset(ins, 0, sizeof(*ins));
    ins->location = *location;
    ins->handler_idx = handler_idx;
}

static bool vsir_instruction_init_label(struct vkd3d_shader_instruction *ins, const struct vkd3d_shader_location *location,
        unsigned int label_id, void *parser)
{
    struct vkd3d_shader_src_param *src_param;

    if (!(src_param = shader_parser_get_src_params(parser, 1)))
        return false;

    vsir_src_param_init_label(src_param, label_id);

    vsir_instruction_init(ins, location, VKD3DSIH_LABEL);
    ins->src = src_param;
    ins->src_count = 1;

    return true;
}

static enum vkd3d_result instruction_array_flatten_hull_shader_phases(struct vkd3d_shader_instruction_array *src_instructions)
{
    struct hull_flattener flattener = {*src_instructions};
    struct vkd3d_shader_instruction_array *instructions;
    struct shader_phase_location_array locations;
    enum vkd3d_result result = VKD3D_OK;
    unsigned int i;

    instructions = &flattener.instructions;

    flattener.phase = VKD3DSIH_INVALID;
    for (i = 0, locations.count = 0; i < instructions->count; ++i)
        flattener_eliminate_phase_related_dcls(&flattener, i, &locations);

    if ((result = flattener_flatten_phases(&flattener, &locations)) < 0)
        return result;

    if (flattener.phase != VKD3DSIH_INVALID)
    {
        if (!shader_instruction_array_reserve(&flattener.instructions, flattener.instructions.count + 1))
            return VKD3D_ERROR_OUT_OF_MEMORY;
        vsir_instruction_init(&instructions->elements[instructions->count++], &flattener.last_ret_location, VKD3DSIH_RET);
    }

    *src_instructions = flattener.instructions;
    return result;
}

struct control_point_normaliser
{
    struct vkd3d_shader_instruction_array instructions;
    enum vkd3d_shader_opcode phase;
    struct vkd3d_shader_src_param *outpointid_param;
};

static bool control_point_normaliser_is_in_control_point_phase(const struct control_point_normaliser *normaliser)
{
    return normaliser->phase == VKD3DSIH_HS_CONTROL_POINT_PHASE;
}

static struct vkd3d_shader_src_param *instruction_array_create_outpointid_param(
        struct vkd3d_shader_instruction_array *instructions)
{
    struct vkd3d_shader_src_param *rel_addr;

    if (!(rel_addr = shader_src_param_allocator_get(&instructions->src_params, 1)))
        return NULL;

    vsir_register_init(&rel_addr->reg, VKD3DSPR_OUTPOINTID, VKD3D_DATA_UINT, 0);
    rel_addr->swizzle = 0;
    rel_addr->modifiers = 0;

    return rel_addr;
}

static void shader_dst_param_normalise_outpointid(struct vkd3d_shader_dst_param *dst_param,
        struct control_point_normaliser *normaliser)
{
    struct vkd3d_shader_register *reg = &dst_param->reg;

    if (control_point_normaliser_is_in_control_point_phase(normaliser) && reg->type == VKD3DSPR_OUTPUT)
    {
        /* The TPF reader validates idx_count. */
        assert(reg->idx_count == 1);
        reg->idx[1] = reg->idx[0];
        /* The control point id param is implicit here. Avoid later complications by inserting it. */
        reg->idx[0].offset = 0;
        reg->idx[0].rel_addr = normaliser->outpointid_param;
        ++reg->idx_count;
    }
}

static void shader_dst_param_io_init(struct vkd3d_shader_dst_param *param, const struct signature_element *e,
        enum vkd3d_shader_register_type reg_type, unsigned int idx_count)
{
    param->write_mask = e->mask;
    param->modifiers = 0;
    param->shift = 0;
    vsir_register_init(&param->reg, reg_type, vkd3d_data_type_from_component_type(e->component_type), idx_count);
}

static enum vkd3d_result control_point_normaliser_emit_hs_input(struct control_point_normaliser *normaliser,
        const struct shader_signature *s, unsigned int input_control_point_count, unsigned int dst,
        const struct vkd3d_shader_location *location)
{
    struct vkd3d_shader_instruction *ins;
    struct vkd3d_shader_dst_param *param;
    const struct signature_element *e;
    unsigned int i, count;

    for (i = 0, count = 1; i < s->element_count; ++i)
        count += !!s->elements[i].used_mask;

    if (!shader_instruction_array_reserve(&normaliser->instructions, normaliser->instructions.count + count))
        return VKD3D_ERROR_OUT_OF_MEMORY;

    memmove(&normaliser->instructions.elements[dst + count], &normaliser->instructions.elements[dst],
            (normaliser->instructions.count - dst) * sizeof(*normaliser->instructions.elements));
    normaliser->instructions.count += count;

    ins = &normaliser->instructions.elements[dst];
    vsir_instruction_init(ins, location, VKD3DSIH_HS_CONTROL_POINT_PHASE);
    ins->flags = 1;
    ++ins;

    for (i = 0; i < s->element_count; ++i)
    {
        e = &s->elements[i];
        if (!e->used_mask)
            continue;

        if (e->sysval_semantic != VKD3D_SHADER_SV_NONE)
        {
            vsir_instruction_init(ins, location, VKD3DSIH_DCL_INPUT_SIV);
            param = &ins->declaration.register_semantic.reg;
            ins->declaration.register_semantic.sysval_semantic = vkd3d_siv_from_sysval(e->sysval_semantic);
        }
        else
        {
            vsir_instruction_init(ins, location, VKD3DSIH_DCL_INPUT);
            param = &ins->declaration.dst;
        }

        shader_dst_param_io_init(param, e, VKD3DSPR_INPUT, 2);
        param->reg.idx[0].offset = input_control_point_count;
        param->reg.idx[1].offset = e->register_index;
        param->write_mask = e->mask;

        ++ins;
    }

    return VKD3D_OK;
}

static enum vkd3d_result instruction_array_normalise_hull_shader_control_point_io(
        struct vkd3d_shader_instruction_array *src_instructions, const struct shader_signature *input_signature)
{
    struct vkd3d_shader_instruction_array *instructions;
    struct control_point_normaliser normaliser;
    unsigned int input_control_point_count;
    struct vkd3d_shader_location location;
    struct vkd3d_shader_instruction *ins;
    enum vkd3d_result ret;
    unsigned int i, j;

    if (!(normaliser.outpointid_param = instruction_array_create_outpointid_param(src_instructions)))
    {
        ERR("Failed to allocate src param.\n");
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }
    normaliser.instructions = *src_instructions;
    instructions = &normaliser.instructions;
    normaliser.phase = VKD3DSIH_INVALID;

    for (i = 0; i < normaliser.instructions.count; ++i)
    {
        ins = &instructions->elements[i];

        switch (ins->handler_idx)
        {
            case VKD3DSIH_HS_CONTROL_POINT_PHASE:
            case VKD3DSIH_HS_FORK_PHASE:
            case VKD3DSIH_HS_JOIN_PHASE:
                normaliser.phase = ins->handler_idx;
                break;
            default:
                if (shader_instruction_is_dcl(ins))
                    break;
                for (j = 0; j < ins->dst_count; ++j)
                    shader_dst_param_normalise_outpointid(&ins->dst[j], &normaliser);
                break;
        }
    }

    normaliser.phase = VKD3DSIH_INVALID;
    input_control_point_count = 1;

    for (i = 0; i < instructions->count; ++i)
    {
        ins = &instructions->elements[i];

        switch (ins->handler_idx)
        {
            case VKD3DSIH_DCL_INPUT_CONTROL_POINT_COUNT:
                input_control_point_count = ins->declaration.count;
                break;
            case VKD3DSIH_HS_CONTROL_POINT_PHASE:
                *src_instructions = normaliser.instructions;
                return VKD3D_OK;
            case VKD3DSIH_HS_FORK_PHASE:
            case VKD3DSIH_HS_JOIN_PHASE:
                /* ins may be relocated if the instruction array expands. */
                location = ins->location;
                ret = control_point_normaliser_emit_hs_input(&normaliser, input_signature,
                        input_control_point_count, i, &location);
                *src_instructions = normaliser.instructions;
                return ret;
            default:
                break;
        }
    }

    *src_instructions = normaliser.instructions;
    return VKD3D_OK;
}

struct io_normaliser
{
    struct vkd3d_shader_instruction_array instructions;
    enum vkd3d_shader_type shader_type;
    uint8_t major;
    struct shader_signature *input_signature;
    struct shader_signature *output_signature;
    struct shader_signature *patch_constant_signature;

    unsigned int instance_count;
    unsigned int phase_body_idx;
    enum vkd3d_shader_opcode phase;
    unsigned int output_control_point_count;

    struct vkd3d_shader_src_param *outpointid_param;

    struct vkd3d_shader_dst_param *input_dcl_params[MAX_REG_OUTPUT];
    struct vkd3d_shader_dst_param *output_dcl_params[MAX_REG_OUTPUT];
    struct vkd3d_shader_dst_param *pc_dcl_params[MAX_REG_OUTPUT];
    uint8_t input_range_map[MAX_REG_OUTPUT][VKD3D_VEC4_SIZE];
    uint8_t output_range_map[MAX_REG_OUTPUT][VKD3D_VEC4_SIZE];
    uint8_t pc_range_map[MAX_REG_OUTPUT][VKD3D_VEC4_SIZE];

    bool use_vocp;
};

static bool io_normaliser_is_in_fork_or_join_phase(const struct io_normaliser *normaliser)
{
    return normaliser->phase == VKD3DSIH_HS_FORK_PHASE || normaliser->phase == VKD3DSIH_HS_JOIN_PHASE;
}

static bool io_normaliser_is_in_control_point_phase(const struct io_normaliser *normaliser)
{
    return normaliser->phase == VKD3DSIH_HS_CONTROL_POINT_PHASE;
}

static unsigned int shader_signature_find_element_for_reg(const struct shader_signature *signature,
        unsigned int reg_idx, unsigned int write_mask)
{
    unsigned int i, base_write_mask;

    for (i = 0; i < signature->element_count; ++i)
    {
        struct signature_element *e = &signature->elements[i];
        if (e->register_index <= reg_idx && e->register_index + e->register_count > reg_idx
                && (e->mask & write_mask) == write_mask)
        {
            return i;
        }
    }

    /* Validated in the TPF reader, but failure in signature_element_range_expand_mask()
     * can land us here on an unmatched vector mask. */
    FIXME("Failed to find signature element for register index %u, mask %#x; using scalar mask.\n",
            reg_idx, write_mask);
    base_write_mask = 1u << vsir_write_mask_get_component_idx(write_mask);
    if (base_write_mask != write_mask)
        return shader_signature_find_element_for_reg(signature, reg_idx, base_write_mask);

    vkd3d_unreachable();
}

struct signature_element *vsir_signature_find_element_for_reg(const struct shader_signature *signature,
        unsigned int reg_idx, unsigned int write_mask)
{
    return &signature->elements[shader_signature_find_element_for_reg(signature, reg_idx, write_mask)];
}

static unsigned int range_map_get_register_count(uint8_t range_map[][VKD3D_VEC4_SIZE],
        unsigned int register_idx, uint32_t write_mask)
{
    return range_map[register_idx][vsir_write_mask_get_component_idx(write_mask)];
}

static void range_map_set_register_range(uint8_t range_map[][VKD3D_VEC4_SIZE], unsigned int register_idx,
        unsigned int register_count, uint32_t write_mask, bool is_dcl_indexrange)
{
    unsigned int i, j, r, c, component_idx, component_count;

    assert(write_mask <= VKD3DSP_WRITEMASK_ALL);
    component_idx = vsir_write_mask_get_component_idx(write_mask);
    component_count = vsir_write_mask_component_count(write_mask);

    assert(register_idx < MAX_REG_OUTPUT && MAX_REG_OUTPUT - register_idx >= register_count);

    if (range_map[register_idx][component_idx] > register_count && is_dcl_indexrange)
    {
        /* Validated in the TPF reader. */
        assert(range_map[register_idx][component_idx] != UINT8_MAX);
        return;
    }
    if (range_map[register_idx][component_idx] == register_count)
    {
        /* Already done. This happens when fxc splits a register declaration by
         * component(s). The dcl_indexrange instructions are split too. */
        return;
    }
    range_map[register_idx][component_idx] = register_count;

    for (i = 0; i < register_count; ++i)
    {
        r = register_idx + i;
        for (j = !i; j < component_count; ++j)
        {
            c = component_idx + j;
            /* A synthetic patch constant range which overlaps an existing range can start upstream of it
             * for fork/join phase instancing, but ranges declared by dcl_indexrange should not overlap.
             * The latter is validated in the TPF reader. */
            assert(!range_map[r][c] || !is_dcl_indexrange);
            range_map[r][c] = UINT8_MAX;
        }
    }
}

static void io_normaliser_add_index_range(struct io_normaliser *normaliser,
        const struct vkd3d_shader_instruction *ins)
{
    const struct vkd3d_shader_index_range *range = &ins->declaration.index_range;
    const struct vkd3d_shader_register *reg = &range->dst.reg;
    unsigned int reg_idx, write_mask, element_idx;
    const struct shader_signature *signature;
    uint8_t (*range_map)[VKD3D_VEC4_SIZE];

    switch (reg->type)
    {
        case VKD3DSPR_INPUT:
        case VKD3DSPR_INCONTROLPOINT:
            range_map = normaliser->input_range_map;
            signature = normaliser->input_signature;
            break;
        case VKD3DSPR_OUTCONTROLPOINT:
            range_map = normaliser->output_range_map;
            signature = normaliser->output_signature;
            break;
        case VKD3DSPR_OUTPUT:
            if (!io_normaliser_is_in_fork_or_join_phase(normaliser))
            {
                range_map = normaliser->output_range_map;
                signature = normaliser->output_signature;
                break;
            }
            /* fall through */
        case VKD3DSPR_PATCHCONST:
            range_map = normaliser->pc_range_map;
            signature = normaliser->patch_constant_signature;
            break;
        default:
            /* Validated in the TPF reader. */
            vkd3d_unreachable();
    }

    reg_idx = reg->idx[reg->idx_count - 1].offset;
    write_mask = range->dst.write_mask;
    element_idx = shader_signature_find_element_for_reg(signature, reg_idx, write_mask);
    range_map_set_register_range(range_map, reg_idx, range->register_count,
            signature->elements[element_idx].mask, true);
}

static int signature_element_mask_compare(const void *a, const void *b)
{
    const struct signature_element *e = a, *f = b;
    int ret;

    return (ret = vkd3d_u32_compare(e->mask, f->mask)) ? ret : vkd3d_u32_compare(e->register_index, f->register_index);
}

static bool sysval_semantics_should_merge(const struct signature_element *e, const struct signature_element *f)
{
    if (e->sysval_semantic < VKD3D_SHADER_SV_TESS_FACTOR_QUADEDGE
            || e->sysval_semantic > VKD3D_SHADER_SV_TESS_FACTOR_LINEDEN)
        return false;

    return e->sysval_semantic == f->sysval_semantic
            /* Line detail and density must be merged together to match the SPIR-V array.
             * This deletes one of the two sysvals, but these are not used. */
            || (e->sysval_semantic == VKD3D_SHADER_SV_TESS_FACTOR_LINEDET
            && f->sysval_semantic == VKD3D_SHADER_SV_TESS_FACTOR_LINEDEN)
            || (e->sysval_semantic == VKD3D_SHADER_SV_TESS_FACTOR_LINEDEN
            && f->sysval_semantic == VKD3D_SHADER_SV_TESS_FACTOR_LINEDET);
}

/* Merge tess factor sysvals because they are an array in SPIR-V. */
static void shader_signature_map_patch_constant_index_ranges(struct shader_signature *s,
        uint8_t range_map[][VKD3D_VEC4_SIZE])
{
    struct signature_element *e, *f;
    unsigned int i, j, register_count;

    qsort(s->elements, s->element_count, sizeof(s->elements[0]), signature_element_mask_compare);

    for (i = 0; i < s->element_count; i += register_count)
    {
        e = &s->elements[i];
        register_count = 1;

        if (!e->sysval_semantic)
            continue;

        for (j = i + 1; j < s->element_count; ++j, ++register_count)
        {
            f = &s->elements[j];
            if (f->register_index != e->register_index + register_count || !sysval_semantics_should_merge(e, f))
                break;
        }
        if (register_count < 2)
            continue;

        range_map_set_register_range(range_map, e->register_index, register_count, e->mask, false);
    }
}

static int signature_element_register_compare(const void *a, const void *b)
{
    const struct signature_element *e = a, *f = b;

    return vkd3d_u32_compare(e->register_index, f->register_index);
}

static int signature_element_index_compare(const void *a, const void *b)
{
    const struct signature_element *e = a, *f = b;

    return vkd3d_u32_compare(e->sort_index, f->sort_index);
}

static unsigned int signature_element_range_expand_mask(struct signature_element *e, unsigned int register_count,
        uint8_t range_map[][VKD3D_VEC4_SIZE])
{
    unsigned int i, j, component_idx, component_count, merged_write_mask = e->mask;

    /* dcl_indexrange instructions can declare a subset of the full mask, and the masks of
     * the elements within the range may differ. TPF's handling of arrayed inputs with
     * dcl_indexrange is really just a hack. Here we create a mask which covers all element
     * masks, and check for collisions with other ranges. */

    for (i = 1; i < register_count; ++i)
        merged_write_mask |= e[i].mask;

    if (merged_write_mask == e->mask)
        return merged_write_mask;

    /* Reaching this point is very rare to begin with, and collisions are even rarer or
     * impossible. If the latter shows up, the fallback in shader_signature_find_element_for_reg()
     * may be sufficient. */

    component_idx = vsir_write_mask_get_component_idx(e->mask);
    component_count = vsir_write_mask_component_count(e->mask);

    for (i = e->register_index; i < e->register_index + register_count; ++i)
    {
        for (j = 0; j < component_idx; ++j)
            if (range_map[i][j])
                break;
        for (j = component_idx + component_count; j < VKD3D_VEC4_SIZE; ++j)
            if (range_map[i][j])
                break;
    }

    if (i == register_count)
    {
        WARN("Expanding mask %#x to %#x for %s, base reg %u, count %u.\n", e->mask, merged_write_mask,
                e->semantic_name, e->register_index, register_count);
        return merged_write_mask;
    }

    WARN("Cannot expand mask %#x to %#x for %s, base reg %u, count %u.\n", e->mask, merged_write_mask,
            e->semantic_name, e->register_index, register_count);
    return e->mask;
}

static bool shader_signature_merge(struct shader_signature *s, uint8_t range_map[][VKD3D_VEC4_SIZE],
        bool is_patch_constant)
{
    unsigned int i, j, element_count, new_count, register_count;
    struct signature_element *elements;
    struct signature_element *e, *f;
    bool used;

    element_count = s->element_count;
    if (!(elements = vkd3d_malloc(element_count * sizeof(*elements))))
        return false;
    memcpy(elements, s->elements, element_count * sizeof(*elements));

    qsort(elements, element_count, sizeof(elements[0]), signature_element_register_compare);

    for (i = 0, new_count = 0; i < element_count; i = j, elements[new_count++] = *e)
    {
        e = &elements[i];
        j = i + 1;

        if (e->register_index == ~0u)
            continue;

        /* Do not merge if the register index will be relative-addressed. */
        if (range_map_get_register_count(range_map, e->register_index, e->mask) > 1)
            continue;

        used = e->used_mask;

        for (; j < element_count; ++j)
        {
            f = &elements[j];

            /* Merge different components of the same register unless sysvals are different,
             * or it will be relative-addressed. */
            if (f->register_index != e->register_index || f->sysval_semantic != e->sysval_semantic
                    || range_map_get_register_count(range_map, f->register_index, f->mask) > 1)
                break;

            TRACE("Merging %s, reg %u, mask %#x, sysval %#x with %s, mask %#x, sysval %#x.\n", e->semantic_name,
                    e->register_index, e->mask, e->sysval_semantic, f->semantic_name, f->mask, f->sysval_semantic);
            assert(!(e->mask & f->mask));

            e->mask |= f->mask;
            e->used_mask |= f->used_mask;
            e->semantic_index = min(e->semantic_index, f->semantic_index);

            /* The first element may have no interpolation mode if it is unused. Elements which
             * actually have different interpolation modes are assigned different registers. */
            if (f->used_mask && !used)
            {
                if (e->interpolation_mode && e->interpolation_mode != f->interpolation_mode)
                    FIXME("Mismatching interpolation modes %u and %u.\n", e->interpolation_mode, f->interpolation_mode);
                else
                    e->interpolation_mode = f->interpolation_mode;
            }
        }
    }
    element_count = new_count;
    vkd3d_free(s->elements);
    s->elements = elements;
    s->element_count = element_count;

    if (is_patch_constant)
        shader_signature_map_patch_constant_index_ranges(s, range_map);

    for (i = 0, new_count = 0; i < element_count; i += register_count, elements[new_count++] = *e)
    {
        e = &elements[i];
        register_count = 1;

        if (e->register_index >= MAX_REG_OUTPUT)
            continue;

        register_count = range_map_get_register_count(range_map, e->register_index, e->mask);
        assert(register_count != UINT8_MAX);
        register_count += !register_count;

        if (register_count > 1)
        {
            TRACE("Merging %s, base reg %u, count %u.\n", e->semantic_name, e->register_index, register_count);
            e->register_count = register_count;
            e->mask = signature_element_range_expand_mask(e, register_count, range_map);
        }
    }
    element_count = new_count;

    /* Restoring the original order is required for sensible trace output. */
    qsort(elements, element_count, sizeof(elements[0]), signature_element_index_compare);

    s->element_count = element_count;

    return true;
}

static unsigned int shader_register_normalise_arrayed_addressing(struct vkd3d_shader_register *reg,
        unsigned int id_idx, unsigned int register_index)
{
    assert(id_idx < ARRAY_SIZE(reg->idx) - 1);

    /* For a relative-addressed register index, move the id up a slot to separate it from the address,
     * because rel_addr can be replaced with a constant offset in some cases. */
    if (reg->idx[id_idx].rel_addr)
    {
        reg->idx[id_idx + 1].rel_addr = NULL;
        reg->idx[id_idx + 1].offset = reg->idx[id_idx].offset;
        reg->idx[id_idx].offset -= register_index;
        if (id_idx)
        {
            /* idx[id_idx] now contains the array index, which must be moved below the control point id. */
            struct vkd3d_shader_register_index tmp = reg->idx[id_idx];
            reg->idx[id_idx] = reg->idx[id_idx - 1];
            reg->idx[id_idx - 1] = tmp;
        }
        ++id_idx;
    }
    /* Otherwise we have no address for the arrayed register, so insert one. This happens e.g. where
     * tessellation level registers are merged into an array because they're an array in SPIR-V. */
    else
    {
        ++id_idx;
        memmove(&reg->idx[1], &reg->idx[0], id_idx * sizeof(reg->idx[0]));
        reg->idx[0].rel_addr = NULL;
        reg->idx[0].offset = reg->idx[id_idx].offset - register_index;
    }

    return id_idx;
}

static bool shader_dst_param_io_normalise(struct vkd3d_shader_dst_param *dst_param, bool is_io_dcl,
         struct io_normaliser *normaliser)
 {
    unsigned int id_idx, reg_idx, write_mask, element_idx;
    struct vkd3d_shader_register *reg = &dst_param->reg;
    struct vkd3d_shader_dst_param **dcl_params;
    const struct shader_signature *signature;
    const struct signature_element *e;

    switch (reg->type)
    {
        case VKD3DSPR_OUTPUT:
            reg_idx = reg->idx[reg->idx_count - 1].offset;
            if (io_normaliser_is_in_fork_or_join_phase(normaliser))
            {
                signature = normaliser->patch_constant_signature;
                /* Convert patch constant outputs to the patch constant register type to avoid the need
                 * to convert compiler symbols when accessed as inputs in a later stage. */
                reg->type = VKD3DSPR_PATCHCONST;
                dcl_params = normaliser->pc_dcl_params;
            }
            else
            {
                signature = normaliser->output_signature;
                dcl_params = normaliser->output_dcl_params;
            }
            break;

        case VKD3DSPR_PATCHCONST:
            reg_idx = reg->idx[reg->idx_count - 1].offset;
            signature = normaliser->patch_constant_signature;
            dcl_params = normaliser->pc_dcl_params;
            break;

        case VKD3DSPR_COLOROUT:
            reg_idx = reg->idx[0].offset;
            signature = normaliser->output_signature;
            reg->type = VKD3DSPR_OUTPUT;
            dcl_params = normaliser->output_dcl_params;
            break;

        case VKD3DSPR_INCONTROLPOINT:
        case VKD3DSPR_INPUT:
            reg_idx = reg->idx[reg->idx_count - 1].offset;
            signature = normaliser->input_signature;
            reg->type = VKD3DSPR_INPUT;
            dcl_params = normaliser->input_dcl_params;
            break;

        case VKD3DSPR_ATTROUT:
            reg_idx = SM1_COLOR_REGISTER_OFFSET + reg->idx[0].offset;
            signature = normaliser->output_signature;
            reg->type = VKD3DSPR_OUTPUT;
            dcl_params = normaliser->output_dcl_params;
            break;

        case VKD3DSPR_RASTOUT:
            reg_idx = SM1_RASTOUT_REGISTER_OFFSET + reg->idx[0].offset;
            signature = normaliser->output_signature;
            reg->type = VKD3DSPR_OUTPUT;
            dcl_params = normaliser->output_dcl_params;
            break;

        default:
            return true;
    }

    id_idx = reg->idx_count - 1;
    write_mask = dst_param->write_mask;
    element_idx = shader_signature_find_element_for_reg(signature, reg_idx, write_mask);
    e = &signature->elements[element_idx];

    dst_param->write_mask >>= vsir_write_mask_get_component_idx(e->mask);
    if (is_io_dcl)
    {
        /* Validated in the TPF reader. */
        assert(element_idx < ARRAY_SIZE(normaliser->input_dcl_params));

        if (dcl_params[element_idx])
        {
            /* Merge split declarations into a single one. */
            dcl_params[element_idx]->write_mask |= dst_param->write_mask;
            /* Turn this into a nop. */
            return false;
        }
        else
        {
            dcl_params[element_idx] = dst_param;
        }
    }

    if (io_normaliser_is_in_control_point_phase(normaliser) && reg->type == VKD3DSPR_OUTPUT)
    {
        if (is_io_dcl)
        {
            /* Emit an array size for the control points for consistency with inputs. */
            reg->idx[0].offset = normaliser->output_control_point_count;
        }
        else
        {
            /* The control point id param. */
            assert(reg->idx[0].rel_addr);
        }
        id_idx = 1;
    }

    if ((e->register_count > 1 || vsir_sysval_semantic_is_tess_factor(e->sysval_semantic)))
    {
        if (is_io_dcl)
        {
            /* For control point I/O, idx 0 contains the control point count.
             * Ensure it is moved up to the next slot. */
            reg->idx[id_idx].offset = reg->idx[0].offset;
            reg->idx[0].offset = e->register_count;
            ++id_idx;
        }
        else
        {
            id_idx = shader_register_normalise_arrayed_addressing(reg, id_idx, e->register_index);
        }
    }

    /* Replace the register index with the signature element index */
    reg->idx[id_idx].offset = element_idx;
    reg->idx_count = id_idx + 1;

    return true;
}

static void shader_src_param_io_normalise(struct vkd3d_shader_src_param *src_param,
        struct io_normaliser *normaliser)
{
    unsigned int i, id_idx, reg_idx, write_mask, element_idx, component_idx;
    struct vkd3d_shader_register *reg = &src_param->reg;
    const struct shader_signature *signature;
    const struct signature_element *e;

    /* Input/output registers from one phase can be used as inputs in
     * subsequent phases. Specifically:
     *
     *   - Control phase inputs are available as "vicp" in fork and join
     *     phases.
     *   - Control phase outputs are available as "vocp" in fork and join
     *     phases.
     *   - Fork phase patch constants are available as "vpc" in join
     *     phases.
     *
     * We handle "vicp" here by converting INCONTROLPOINT src registers to
     * type INPUT so they match the control phase declarations. We handle
     * "vocp" by converting OUTCONTROLPOINT registers to type OUTPUT.
     * Merging fork and join phases handles "vpc". */

    switch (reg->type)
    {
        case VKD3DSPR_PATCHCONST:
            reg_idx = reg->idx[reg->idx_count - 1].offset;
            signature = normaliser->patch_constant_signature;
            break;

        case VKD3DSPR_INCONTROLPOINT:
            reg->type = VKD3DSPR_INPUT;
            /* fall through */
        case VKD3DSPR_INPUT:
            if (normaliser->major < 3 && normaliser->shader_type == VKD3D_SHADER_TYPE_PIXEL)
                reg_idx = SM1_COLOR_REGISTER_OFFSET + reg->idx[0].offset;
            else
                reg_idx = reg->idx[reg->idx_count - 1].offset;
            signature = normaliser->input_signature;
            break;

        case VKD3DSPR_OUTCONTROLPOINT:
            reg->type = VKD3DSPR_OUTPUT;
            /* fall through */
        case VKD3DSPR_OUTPUT:
            reg_idx = reg->idx[reg->idx_count - 1].offset;
            signature = normaliser->output_signature;
            break;

        case VKD3DSPR_TEXTURE:
            if (normaliser->shader_type != VKD3D_SHADER_TYPE_PIXEL)
                return;
            reg->type = VKD3DSPR_INPUT;
            reg_idx = reg->idx[0].offset;
            signature = normaliser->input_signature;
            break;

        default:
            return;
    }

    id_idx = reg->idx_count - 1;
    write_mask = VKD3DSP_WRITEMASK_0 << vsir_swizzle_get_component(src_param->swizzle, 0);
    element_idx = shader_signature_find_element_for_reg(signature, reg_idx, write_mask);

    e = &signature->elements[element_idx];
    if ((e->register_count > 1 || vsir_sysval_semantic_is_tess_factor(e->sysval_semantic)))
        id_idx = shader_register_normalise_arrayed_addressing(reg, id_idx, e->register_index);
    reg->idx[id_idx].offset = element_idx;
    reg->idx_count = id_idx + 1;

    if ((component_idx = vsir_write_mask_get_component_idx(e->mask)))
    {
        for (i = 0; i < VKD3D_VEC4_SIZE; ++i)
            if (vsir_swizzle_get_component(src_param->swizzle, i))
                src_param->swizzle -= component_idx << VKD3D_SHADER_SWIZZLE_SHIFT(i);
    }
}

static void shader_instruction_normalise_io_params(struct vkd3d_shader_instruction *ins,
        struct io_normaliser *normaliser)
{
    struct vkd3d_shader_register *reg;
    unsigned int i;

    switch (ins->handler_idx)
    {
        case VKD3DSIH_DCL_INPUT:
            if (normaliser->shader_type == VKD3D_SHADER_TYPE_HULL)
            {
                reg = &ins->declaration.dst.reg;

                if (reg->type == VKD3DSPR_OUTCONTROLPOINT)
                    normaliser->use_vocp = true;

                /* We don't need to keep OUTCONTROLPOINT or PATCHCONST input declarations since their
                * equivalents were declared earlier, but INCONTROLPOINT may be the first occurrence. */
                if (reg->type == VKD3DSPR_OUTCONTROLPOINT || reg->type == VKD3DSPR_PATCHCONST)
                    vkd3d_shader_instruction_make_nop(ins);
                else if (reg->type == VKD3DSPR_INCONTROLPOINT)
                    reg->type = VKD3DSPR_INPUT;
            }
            /* fall through */
        case VKD3DSIH_DCL_INPUT_PS:
        case VKD3DSIH_DCL_OUTPUT:
            if (!shader_dst_param_io_normalise(&ins->declaration.dst, true, normaliser))
                vkd3d_shader_instruction_make_nop(ins);
            break;
        case VKD3DSIH_DCL_INPUT_SGV:
        case VKD3DSIH_DCL_INPUT_SIV:
        case VKD3DSIH_DCL_INPUT_PS_SGV:
        case VKD3DSIH_DCL_INPUT_PS_SIV:
        case VKD3DSIH_DCL_OUTPUT_SIV:
            if (!shader_dst_param_io_normalise(&ins->declaration.register_semantic.reg, true, normaliser))
                vkd3d_shader_instruction_make_nop(ins);
            break;
        case VKD3DSIH_HS_CONTROL_POINT_PHASE:
        case VKD3DSIH_HS_FORK_PHASE:
        case VKD3DSIH_HS_JOIN_PHASE:
            normaliser->phase = ins->handler_idx;
            memset(normaliser->input_dcl_params, 0, sizeof(normaliser->input_dcl_params));
            memset(normaliser->output_dcl_params, 0, sizeof(normaliser->output_dcl_params));
            memset(normaliser->pc_dcl_params, 0, sizeof(normaliser->pc_dcl_params));
            break;
        default:
            if (shader_instruction_is_dcl(ins))
                break;
            for (i = 0; i < ins->dst_count; ++i)
                shader_dst_param_io_normalise(&ins->dst[i], false, normaliser);
            for (i = 0; i < ins->src_count; ++i)
                shader_src_param_io_normalise(&ins->src[i], normaliser);
            break;
    }
}

static enum vkd3d_result shader_normalise_io_registers(struct vkd3d_shader_parser *parser)
{
    struct io_normaliser normaliser = {parser->program.instructions};
    struct vsir_program *program = &parser->program;
    struct vkd3d_shader_instruction *ins;
    bool has_control_point_phase;
    unsigned int i, j;

    normaliser.phase = VKD3DSIH_INVALID;
    normaliser.shader_type = program->shader_version.type;
    normaliser.major = program->shader_version.major;
    normaliser.input_signature = &parser->shader_desc.input_signature;
    normaliser.output_signature = &parser->shader_desc.output_signature;
    normaliser.patch_constant_signature = &parser->shader_desc.patch_constant_signature;

    for (i = 0, has_control_point_phase = false; i < program->instructions.count; ++i)
    {
        ins = &program->instructions.elements[i];

        switch (ins->handler_idx)
        {
            case VKD3DSIH_DCL_OUTPUT_CONTROL_POINT_COUNT:
                normaliser.output_control_point_count = ins->declaration.count;
                break;
            case VKD3DSIH_DCL_INDEX_RANGE:
                io_normaliser_add_index_range(&normaliser, ins);
                vkd3d_shader_instruction_make_nop(ins);
                break;
            case VKD3DSIH_HS_CONTROL_POINT_PHASE:
                has_control_point_phase = true;
                /* fall through */
            case VKD3DSIH_HS_FORK_PHASE:
            case VKD3DSIH_HS_JOIN_PHASE:
                normaliser.phase = ins->handler_idx;
                break;
            default:
                break;
        }
    }

    if (normaliser.shader_type == VKD3D_SHADER_TYPE_HULL && !has_control_point_phase)
    {
        /* Inputs and outputs must match for the default phase, so merge ranges must match too. */
        for (i = 0; i < MAX_REG_OUTPUT; ++i)
        {
            for (j = 0; j < VKD3D_VEC4_SIZE; ++j)
            {
                if (!normaliser.input_range_map[i][j] && normaliser.output_range_map[i][j])
                    normaliser.input_range_map[i][j] = normaliser.output_range_map[i][j];
                else if (normaliser.input_range_map[i][j] && !normaliser.output_range_map[i][j])
                    normaliser.output_range_map[i][j] = normaliser.input_range_map[i][j];
                else assert(normaliser.input_range_map[i][j] == normaliser.output_range_map[i][j]);
            }
        }
    }

    if (!shader_signature_merge(&parser->shader_desc.input_signature, normaliser.input_range_map, false)
            || !shader_signature_merge(&parser->shader_desc.output_signature, normaliser.output_range_map, false)
            || !shader_signature_merge(&parser->shader_desc.patch_constant_signature, normaliser.pc_range_map, true))
    {
        program->instructions = normaliser.instructions;
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    normaliser.phase = VKD3DSIH_INVALID;
    for (i = 0; i < normaliser.instructions.count; ++i)
        shader_instruction_normalise_io_params(&normaliser.instructions.elements[i], &normaliser);

    program->instructions = normaliser.instructions;
    program->use_vocp = normaliser.use_vocp;
    return VKD3D_OK;
}

struct flat_constant_def
{
    enum vkd3d_shader_d3dbc_constant_register set;
    uint32_t index;
    uint32_t value[4];
};

struct flat_constants_normaliser
{
    struct flat_constant_def *defs;
    size_t def_count, defs_capacity;
};

static bool get_flat_constant_register_type(const struct vkd3d_shader_register *reg,
        enum vkd3d_shader_d3dbc_constant_register *set, uint32_t *index)
{
    static const struct
    {
        enum vkd3d_shader_register_type type;
        enum vkd3d_shader_d3dbc_constant_register set;
        uint32_t offset;
    }
    regs[] =
    {
        {VKD3DSPR_CONST, VKD3D_SHADER_D3DBC_FLOAT_CONSTANT_REGISTER, 0},
        {VKD3DSPR_CONST2, VKD3D_SHADER_D3DBC_FLOAT_CONSTANT_REGISTER, 2048},
        {VKD3DSPR_CONST3, VKD3D_SHADER_D3DBC_FLOAT_CONSTANT_REGISTER, 4096},
        {VKD3DSPR_CONST4, VKD3D_SHADER_D3DBC_FLOAT_CONSTANT_REGISTER, 6144},
        {VKD3DSPR_CONSTINT, VKD3D_SHADER_D3DBC_INT_CONSTANT_REGISTER, 0},
        {VKD3DSPR_CONSTBOOL, VKD3D_SHADER_D3DBC_BOOL_CONSTANT_REGISTER, 0},
    };

    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(regs); ++i)
    {
        if (reg->type == regs[i].type)
        {
            if (reg->idx[0].rel_addr)
            {
                FIXME("Unhandled relative address.\n");
                return false;
            }

            *set = regs[i].set;
            *index = regs[i].offset + reg->idx[0].offset;
            return true;
        }
    }

    return false;
}

static void shader_register_normalise_flat_constants(struct vkd3d_shader_src_param *param,
        const struct flat_constants_normaliser *normaliser)
{
    enum vkd3d_shader_d3dbc_constant_register set;
    uint32_t index;
    size_t i, j;

    if (!get_flat_constant_register_type(&param->reg, &set, &index))
        return;

    for (i = 0; i < normaliser->def_count; ++i)
    {
        if (normaliser->defs[i].set == set && normaliser->defs[i].index == index)
        {
            param->reg.type = VKD3DSPR_IMMCONST;
            param->reg.idx_count = 0;
            param->reg.dimension = VSIR_DIMENSION_VEC4;
            for (j = 0; j < 4; ++j)
                param->reg.u.immconst_u32[j] = normaliser->defs[i].value[j];
            return;
        }
    }

    param->reg.type = VKD3DSPR_CONSTBUFFER;
    param->reg.idx[0].offset = set; /* register ID */
    param->reg.idx[1].offset = set; /* register index */
    param->reg.idx[2].offset = index; /* buffer index */
    param->reg.idx_count = 3;
}

static enum vkd3d_result instruction_array_normalise_flat_constants(struct vsir_program *program)
{
    struct flat_constants_normaliser normaliser = {0};
    unsigned int i, j;

    for (i = 0; i < program->instructions.count; ++i)
    {
        struct vkd3d_shader_instruction *ins = &program->instructions.elements[i];

        if (ins->handler_idx == VKD3DSIH_DEF || ins->handler_idx == VKD3DSIH_DEFI || ins->handler_idx == VKD3DSIH_DEFB)
        {
            struct flat_constant_def *def;

            if (!vkd3d_array_reserve((void **)&normaliser.defs, &normaliser.defs_capacity,
                    normaliser.def_count + 1, sizeof(*normaliser.defs)))
            {
                vkd3d_free(normaliser.defs);
                return VKD3D_ERROR_OUT_OF_MEMORY;
            }

            def = &normaliser.defs[normaliser.def_count++];

            get_flat_constant_register_type((struct vkd3d_shader_register *)&ins->dst[0].reg, &def->set, &def->index);
            for (j = 0; j < 4; ++j)
                def->value[j] = ins->src[0].reg.u.immconst_u32[j];

            vkd3d_shader_instruction_make_nop(ins);
        }
        else
        {
            for (j = 0; j < ins->src_count; ++j)
                shader_register_normalise_flat_constants(&ins->src[j], &normaliser);
        }
    }

    vkd3d_free(normaliser.defs);
    return VKD3D_OK;
}

static void remove_dead_code(struct vsir_program *program)
{
    size_t i, depth = 0;
    bool dead = false;

    for (i = 0; i < program->instructions.count; ++i)
    {
        struct vkd3d_shader_instruction *ins = &program->instructions.elements[i];

        switch (ins->handler_idx)
        {
            case VKD3DSIH_IF:
            case VKD3DSIH_LOOP:
            case VKD3DSIH_SWITCH:
                if (dead)
                {
                    vkd3d_shader_instruction_make_nop(ins);
                    ++depth;
                }
                break;

            case VKD3DSIH_ENDIF:
            case VKD3DSIH_ENDLOOP:
            case VKD3DSIH_ENDSWITCH:
            case VKD3DSIH_ELSE:
                if (dead)
                {
                    if (depth > 0)
                    {
                        if (ins->handler_idx != VKD3DSIH_ELSE)
                            --depth;
                        vkd3d_shader_instruction_make_nop(ins);
                    }
                    else
                    {
                        dead = false;
                    }
                }
                break;

            /* `depth' is counted with respect to where the dead code
             * segment began. So it starts at zero and it signals the
             * termination of the dead code segment when it would
             * become negative. */
            case VKD3DSIH_BREAK:
            case VKD3DSIH_RET:
            case VKD3DSIH_CONTINUE:
                if (dead)
                {
                    vkd3d_shader_instruction_make_nop(ins);
                }
                else
                {
                    dead = true;
                    depth = 0;
                }
                break;

            /* If `case' or `default' appears at zero depth, it means
             * that they are a possible target for the corresponding
             * switch, so the code is live again. */
            case VKD3DSIH_CASE:
            case VKD3DSIH_DEFAULT:
                if (dead)
                {
                    if (depth == 0)
                        dead = false;
                    else
                        vkd3d_shader_instruction_make_nop(ins);
                }
                break;

            /* Phase instructions can only appear in hull shaders and
             * outside of any block. When a phase returns, control is
             * moved to the following phase, so they make code live
             * again. */
            case VKD3DSIH_HS_CONTROL_POINT_PHASE:
            case VKD3DSIH_HS_FORK_PHASE:
            case VKD3DSIH_HS_JOIN_PHASE:
                dead = false;
                break;

            default:
                if (dead)
                    vkd3d_shader_instruction_make_nop(ins);
                break;
        }
    }
}

static enum vkd3d_result normalise_combined_samplers(struct vkd3d_shader_parser *parser)
{
    unsigned int i;

    for (i = 0; i < parser->program.instructions.count; ++i)
    {
        struct vkd3d_shader_instruction *ins = &parser->program.instructions.elements[i];
        struct vkd3d_shader_src_param *srcs;

        switch (ins->handler_idx)
        {
            case VKD3DSIH_TEX:
                if (!(srcs = shader_src_param_allocator_get(&parser->program.instructions.src_params, 3)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                memset(srcs, 0, sizeof(*srcs) * 3);

                ins->handler_idx = VKD3DSIH_SAMPLE;

                srcs[0] = ins->src[0];

                srcs[1].reg.type = VKD3DSPR_RESOURCE;
                srcs[1].reg.idx[0] = ins->src[1].reg.idx[0];
                srcs[1].reg.idx[1] = ins->src[1].reg.idx[0];
                srcs[1].reg.idx_count = 2;
                srcs[1].reg.data_type = VKD3D_DATA_RESOURCE;
                srcs[1].swizzle = VKD3D_SHADER_NO_SWIZZLE;

                srcs[2].reg.type = VKD3DSPR_SAMPLER;
                srcs[2].reg.idx[0] = ins->src[1].reg.idx[0];
                srcs[2].reg.idx[1] = ins->src[1].reg.idx[0];
                srcs[2].reg.idx_count = 2;
                srcs[2].reg.data_type = VKD3D_DATA_SAMPLER;

                ins->src = srcs;
                ins->src_count = 3;
                break;

            case VKD3DSIH_TEXBEM:
            case VKD3DSIH_TEXBEML:
            case VKD3DSIH_TEXCOORD:
            case VKD3DSIH_TEXDEPTH:
            case VKD3DSIH_TEXDP3:
            case VKD3DSIH_TEXDP3TEX:
            case VKD3DSIH_TEXLDD:
            case VKD3DSIH_TEXLDL:
            case VKD3DSIH_TEXM3x2PAD:
            case VKD3DSIH_TEXM3x2TEX:
            case VKD3DSIH_TEXM3x3DIFF:
            case VKD3DSIH_TEXM3x3PAD:
            case VKD3DSIH_TEXM3x3SPEC:
            case VKD3DSIH_TEXM3x3TEX:
            case VKD3DSIH_TEXM3x3VSPEC:
            case VKD3DSIH_TEXREG2AR:
            case VKD3DSIH_TEXREG2GB:
            case VKD3DSIH_TEXREG2RGB:
                vkd3d_shader_parser_error(parser, VKD3D_SHADER_ERROR_VSIR_NOT_IMPLEMENTED,
                        "Aborting due to not yet implemented feature: "
                        "Combined sampler instruction %#x.", ins->handler_idx);
                return VKD3D_ERROR_NOT_IMPLEMENTED;

            default:
                break;
        }
    }

    return VKD3D_OK;
}

struct cf_flattener_if_info
{
    struct vkd3d_shader_src_param *false_param;
    unsigned int id;
    uint32_t merge_block_id;
    unsigned int else_block_id;
};

struct cf_flattener_loop_info
{
    unsigned int header_block_id;
    unsigned int continue_block_id;
    uint32_t merge_block_id;
};

struct cf_flattener_switch_case
{
    unsigned int value;
    unsigned int block_id;
};

struct cf_flattener_switch_info
{
    size_t ins_location;
    const struct vkd3d_shader_src_param *condition;
    unsigned int id;
    unsigned int merge_block_id;
    unsigned int default_block_id;
    struct cf_flattener_switch_case *cases;
    size_t cases_size;
    unsigned int cases_count;
};

struct cf_flattener_info
{
    union
    {
        struct cf_flattener_if_info if_;
        struct cf_flattener_loop_info loop;
        struct cf_flattener_switch_info switch_;
    } u;

    enum
    {
        VKD3D_BLOCK_IF,
        VKD3D_BLOCK_LOOP,
        VKD3D_BLOCK_SWITCH,
    } current_block;
    bool inside_block;
};

struct cf_flattener
{
    struct vkd3d_shader_parser *parser;

    struct vkd3d_shader_location location;
    bool allocation_failed;

    struct vkd3d_shader_instruction *instructions;
    size_t instruction_capacity;
    size_t instruction_count;

    unsigned int block_id;
    const char **block_names;
    size_t block_name_capacity;
    size_t block_name_count;

    unsigned int branch_id;
    unsigned int loop_id;
    unsigned int switch_id;

    unsigned int control_flow_depth;
    struct cf_flattener_info *control_flow_info;
    size_t control_flow_info_size;
};

static struct vkd3d_shader_instruction *cf_flattener_require_space(struct cf_flattener *flattener, size_t count)
{
    if (!vkd3d_array_reserve((void **)&flattener->instructions, &flattener->instruction_capacity,
            flattener->instruction_count + count, sizeof(*flattener->instructions)))
    {
        ERR("Failed to allocate instructions.\n");
        flattener->allocation_failed = true;
        return NULL;
    }
    return &flattener->instructions[flattener->instruction_count];
}

static bool cf_flattener_copy_instruction(struct cf_flattener *flattener,
        const struct vkd3d_shader_instruction *instruction)
{
    struct vkd3d_shader_instruction *dst_ins;

    if (instruction->handler_idx == VKD3DSIH_NOP)
        return true;

    if (!(dst_ins = cf_flattener_require_space(flattener, 1)))
        return false;

    *dst_ins = *instruction;
    ++flattener->instruction_count;
    return true;
}

static unsigned int cf_flattener_alloc_block_id(struct cf_flattener *flattener)
{
    return ++flattener->block_id;
}

static struct vkd3d_shader_src_param *instruction_src_params_alloc(struct vkd3d_shader_instruction *ins,
        unsigned int count, struct cf_flattener *flattener)
{
    struct vkd3d_shader_src_param *params = shader_parser_get_src_params(flattener->parser, count);
    if (!params)
    {
        flattener->allocation_failed = true;
        return NULL;
    }
    ins->src = params;
    ins->src_count = count;
    return params;
}

static void cf_flattener_emit_label(struct cf_flattener *flattener, unsigned int label_id)
{
    struct vkd3d_shader_instruction *ins;

    if (!(ins = cf_flattener_require_space(flattener, 1)))
        return;
    if (vsir_instruction_init_label(ins, &flattener->location, label_id, flattener->parser))
        ++flattener->instruction_count;
    else
        flattener->allocation_failed = true;
}

/* For conditional branches, this returns the false target branch parameter. */
static struct vkd3d_shader_src_param *cf_flattener_emit_branch(struct cf_flattener *flattener,
        unsigned int merge_block_id, unsigned int continue_block_id,
        const struct vkd3d_shader_src_param *condition, unsigned int true_id, unsigned int false_id,
        unsigned int flags)
{
    struct vkd3d_shader_src_param *src_params, *false_branch_param;
    struct vkd3d_shader_instruction *ins;

    if (!(ins = cf_flattener_require_space(flattener, 1)))
        return NULL;
    vsir_instruction_init(ins, &flattener->location, VKD3DSIH_BRANCH);

    if (condition)
    {
        if (!(src_params = instruction_src_params_alloc(ins, 4 + !!continue_block_id, flattener)))
            return NULL;
        src_params[0] = *condition;
        if (flags == VKD3D_SHADER_CONDITIONAL_OP_Z)
        {
            vsir_src_param_init_label(&src_params[1], false_id);
            vsir_src_param_init_label(&src_params[2], true_id);
            false_branch_param = &src_params[1];
        }
        else
        {
            vsir_src_param_init_label(&src_params[1], true_id);
            vsir_src_param_init_label(&src_params[2], false_id);
            false_branch_param = &src_params[2];
        }
        vsir_src_param_init_label(&src_params[3], merge_block_id);
        if (continue_block_id)
            vsir_src_param_init_label(&src_params[4], continue_block_id);
    }
    else
    {
        if (!(src_params = instruction_src_params_alloc(ins, merge_block_id ? 3 : 1, flattener)))
            return NULL;
        vsir_src_param_init_label(&src_params[0], true_id);
        if (merge_block_id)
        {
            /* An unconditional branch may only have merge information for a loop, which
             * must have both a merge block and continue block. */
            vsir_src_param_init_label(&src_params[1], merge_block_id);
            vsir_src_param_init_label(&src_params[2], continue_block_id);
        }
        false_branch_param = NULL;
    }

    ++flattener->instruction_count;

    return false_branch_param;
}

static void cf_flattener_emit_conditional_branch_and_merge(struct cf_flattener *flattener,
        const struct vkd3d_shader_src_param *condition, unsigned int true_id, unsigned int flags)
{
    unsigned int merge_block_id;

    merge_block_id = cf_flattener_alloc_block_id(flattener);
    cf_flattener_emit_branch(flattener, merge_block_id, 0, condition, true_id, merge_block_id, flags);
    cf_flattener_emit_label(flattener, merge_block_id);
}

static void cf_flattener_emit_unconditional_branch(struct cf_flattener *flattener, unsigned int target_block_id)
{
    cf_flattener_emit_branch(flattener, 0, 0, NULL, target_block_id, 0, 0);
}

static struct cf_flattener_info *cf_flattener_push_control_flow_level(struct cf_flattener *flattener)
{
    if (!vkd3d_array_reserve((void **)&flattener->control_flow_info, &flattener->control_flow_info_size,
            flattener->control_flow_depth + 1, sizeof(*flattener->control_flow_info)))
    {
        ERR("Failed to allocate control flow info structure.\n");
        flattener->allocation_failed = true;
        return NULL;
    }

    return &flattener->control_flow_info[flattener->control_flow_depth++];
}

static void cf_flattener_pop_control_flow_level(struct cf_flattener *flattener)
{
    struct cf_flattener_info *cf_info;

    cf_info = &flattener->control_flow_info[--flattener->control_flow_depth];
    memset(cf_info, 0, sizeof(*cf_info));
}

static struct cf_flattener_info *cf_flattener_find_innermost_loop(struct cf_flattener *flattener)
{
    int depth;

    for (depth = flattener->control_flow_depth - 1; depth >= 0; --depth)
    {
        if (flattener->control_flow_info[depth].current_block == VKD3D_BLOCK_LOOP)
            return &flattener->control_flow_info[depth];
    }

    return NULL;
}

static struct cf_flattener_info *cf_flattener_find_innermost_breakable_cf_construct(struct cf_flattener *flattener)
{
    int depth;

    for (depth = flattener->control_flow_depth - 1; depth >= 0; --depth)
    {
        if (flattener->control_flow_info[depth].current_block == VKD3D_BLOCK_LOOP
                || flattener->control_flow_info[depth].current_block == VKD3D_BLOCK_SWITCH)
            return &flattener->control_flow_info[depth];
    }

    return NULL;
}

static void VKD3D_PRINTF_FUNC(3, 4) cf_flattener_create_block_name(struct cf_flattener *flattener,
        unsigned int block_id, const char *fmt, ...)
{
    struct vkd3d_string_buffer buffer;
    size_t block_name_count;
    va_list args;

    --block_id;

    block_name_count = max(flattener->block_name_count, block_id + 1);
    if (!vkd3d_array_reserve((void **)&flattener->block_names, &flattener->block_name_capacity,
            block_name_count, sizeof(*flattener->block_names)))
        return;
    memset(&flattener->block_names[flattener->block_name_count], 0,
            (block_name_count - flattener->block_name_count) * sizeof(*flattener->block_names));
    flattener->block_name_count = block_name_count;

    vkd3d_string_buffer_init(&buffer);
    va_start(args, fmt);
    vkd3d_string_buffer_vprintf(&buffer, fmt, args);
    va_end(args);

    flattener->block_names[block_id] = buffer.buffer;
}

static bool vsir_instruction_is_dcl(const struct vkd3d_shader_instruction *instruction)
{
    enum vkd3d_shader_opcode handler_idx = instruction->handler_idx;
    return (VKD3DSIH_DCL <= handler_idx && handler_idx <= VKD3DSIH_DCL_VERTICES_OUT)
            || handler_idx == VKD3DSIH_HS_DECLS;
}

static enum vkd3d_result cf_flattener_iterate_instruction_array(struct cf_flattener *flattener)
{
    bool main_block_open, is_hull_shader, after_declarations_section;
    struct vkd3d_shader_parser *parser = flattener->parser;
    struct vkd3d_shader_instruction_array *instructions;
    struct vsir_program *program = &parser->program;
    struct vkd3d_shader_instruction *dst_ins;
    size_t i;

    instructions = &program->instructions;
    is_hull_shader = program->shader_version.type == VKD3D_SHADER_TYPE_HULL;
    main_block_open = !is_hull_shader;
    after_declarations_section = is_hull_shader;

    if (!cf_flattener_require_space(flattener, instructions->count + 1))
        return VKD3D_ERROR_OUT_OF_MEMORY;

    for (i = 0; i < instructions->count; ++i)
    {
        unsigned int loop_header_block_id, loop_body_block_id, continue_block_id, merge_block_id, true_block_id;
        const struct vkd3d_shader_instruction *instruction = &instructions->elements[i];
        const struct vkd3d_shader_src_param *src = instruction->src;
        struct cf_flattener_info *cf_info;

        flattener->location = instruction->location;

        /* Declarations should occur before the first code block, which in hull shaders is marked by the first
         * phase instruction, and in all other shader types begins with the first label instruction. */
        if (!after_declarations_section && !vsir_instruction_is_dcl(instruction)
                && instruction->handler_idx != VKD3DSIH_NOP)
        {
            after_declarations_section = true;
            cf_flattener_emit_label(flattener, cf_flattener_alloc_block_id(flattener));
        }

        cf_info = flattener->control_flow_depth
                ? &flattener->control_flow_info[flattener->control_flow_depth - 1] : NULL;

        switch (instruction->handler_idx)
        {
            case VKD3DSIH_HS_CONTROL_POINT_PHASE:
            case VKD3DSIH_HS_FORK_PHASE:
            case VKD3DSIH_HS_JOIN_PHASE:
                if (!cf_flattener_copy_instruction(flattener, instruction))
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                if (instruction->handler_idx != VKD3DSIH_HS_CONTROL_POINT_PHASE || !instruction->flags)
                    after_declarations_section = false;
                break;

            case VKD3DSIH_LABEL:
                vkd3d_shader_parser_error(parser, VKD3D_SHADER_ERROR_VSIR_NOT_IMPLEMENTED,
                        "Aborting due to not yet implemented feature: Label instruction.");
                return VKD3D_ERROR_NOT_IMPLEMENTED;

            case VKD3DSIH_IF:
                if (!(cf_info = cf_flattener_push_control_flow_level(flattener)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                true_block_id = cf_flattener_alloc_block_id(flattener);
                merge_block_id = cf_flattener_alloc_block_id(flattener);
                cf_info->u.if_.false_param = cf_flattener_emit_branch(flattener, merge_block_id, 0,
                        src, true_block_id, merge_block_id, instruction->flags);
                if (!cf_info->u.if_.false_param)
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                cf_flattener_emit_label(flattener, true_block_id);

                cf_info->u.if_.id = flattener->branch_id;
                cf_info->u.if_.merge_block_id = merge_block_id;
                cf_info->u.if_.else_block_id = 0;
                cf_info->inside_block = true;
                cf_info->current_block = VKD3D_BLOCK_IF;

                cf_flattener_create_block_name(flattener, merge_block_id, "branch%u_merge", flattener->branch_id);
                cf_flattener_create_block_name(flattener, true_block_id, "branch%u_true", flattener->branch_id);
                ++flattener->branch_id;
                break;

            case VKD3DSIH_ELSE:
                if (cf_info->inside_block)
                    cf_flattener_emit_unconditional_branch(flattener, cf_info->u.if_.merge_block_id);

                cf_info->u.if_.else_block_id = cf_flattener_alloc_block_id(flattener);
                cf_info->u.if_.false_param->reg.idx[0].offset = cf_info->u.if_.else_block_id;

                cf_flattener_create_block_name(flattener,
                        cf_info->u.if_.else_block_id, "branch%u_false", cf_info->u.if_.id);
                cf_flattener_emit_label(flattener, cf_info->u.if_.else_block_id);

                cf_info->inside_block = true;
                break;

            case VKD3DSIH_ENDIF:
                if (cf_info->inside_block)
                    cf_flattener_emit_unconditional_branch(flattener, cf_info->u.if_.merge_block_id);

                cf_flattener_emit_label(flattener, cf_info->u.if_.merge_block_id);

                cf_flattener_pop_control_flow_level(flattener);
                break;

            case VKD3DSIH_LOOP:
                if (!(cf_info = cf_flattener_push_control_flow_level(flattener)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                loop_header_block_id = cf_flattener_alloc_block_id(flattener);
                loop_body_block_id = cf_flattener_alloc_block_id(flattener);
                continue_block_id = cf_flattener_alloc_block_id(flattener);
                merge_block_id = cf_flattener_alloc_block_id(flattener);

                cf_flattener_emit_unconditional_branch(flattener, loop_header_block_id);
                cf_flattener_emit_label(flattener, loop_header_block_id);
                cf_flattener_emit_branch(flattener, merge_block_id, continue_block_id,
                        NULL, loop_body_block_id, 0, 0);

                cf_flattener_emit_label(flattener, loop_body_block_id);

                cf_info->u.loop.header_block_id = loop_header_block_id;
                cf_info->u.loop.continue_block_id = continue_block_id;
                cf_info->u.loop.merge_block_id = merge_block_id;
                cf_info->current_block = VKD3D_BLOCK_LOOP;
                cf_info->inside_block = true;

                cf_flattener_create_block_name(flattener, loop_header_block_id, "loop%u_header", flattener->loop_id);
                cf_flattener_create_block_name(flattener, loop_body_block_id, "loop%u_body", flattener->loop_id);
                cf_flattener_create_block_name(flattener, continue_block_id, "loop%u_continue", flattener->loop_id);
                cf_flattener_create_block_name(flattener, merge_block_id, "loop%u_merge", flattener->loop_id);
                ++flattener->loop_id;
                break;

            case VKD3DSIH_ENDLOOP:
                if (cf_info->inside_block)
                    cf_flattener_emit_unconditional_branch(flattener, cf_info->u.loop.continue_block_id);

                cf_flattener_emit_label(flattener, cf_info->u.loop.continue_block_id);
                cf_flattener_emit_unconditional_branch(flattener, cf_info->u.loop.header_block_id);
                cf_flattener_emit_label(flattener, cf_info->u.loop.merge_block_id);

                cf_flattener_pop_control_flow_level(flattener);
                break;

            case VKD3DSIH_SWITCH:
                if (!(cf_info = cf_flattener_push_control_flow_level(flattener)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                merge_block_id = cf_flattener_alloc_block_id(flattener);

                cf_info->u.switch_.ins_location = flattener->instruction_count;
                cf_info->u.switch_.condition = src;

                if (!(dst_ins = cf_flattener_require_space(flattener, 1)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                vsir_instruction_init(dst_ins, &instruction->location, VKD3DSIH_SWITCH_MONOLITHIC);
                ++flattener->instruction_count;

                cf_info->u.switch_.id = flattener->switch_id;
                cf_info->u.switch_.merge_block_id = merge_block_id;
                cf_info->u.switch_.cases = NULL;
                cf_info->u.switch_.cases_size = 0;
                cf_info->u.switch_.cases_count = 0;
                cf_info->u.switch_.default_block_id = 0;
                cf_info->inside_block = false;
                cf_info->current_block = VKD3D_BLOCK_SWITCH;

                cf_flattener_create_block_name(flattener, merge_block_id, "switch%u_merge", flattener->switch_id);
                ++flattener->switch_id;

                if (!vkd3d_array_reserve((void **)&cf_info->u.switch_.cases, &cf_info->u.switch_.cases_size,
                        10, sizeof(*cf_info->u.switch_.cases)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                break;

            case VKD3DSIH_ENDSWITCH:
            {
                struct vkd3d_shader_src_param *src_params;
                unsigned int j;

                if (!cf_info->u.switch_.default_block_id)
                    cf_info->u.switch_.default_block_id = cf_info->u.switch_.merge_block_id;

                cf_flattener_emit_label(flattener, cf_info->u.switch_.merge_block_id);

                /* The SWITCH instruction is completed when the endswitch
                 * instruction is processed because we do not know the number
                 * of case statements or the default block id in advance.*/
                dst_ins = &flattener->instructions[cf_info->u.switch_.ins_location];
                if (!(src_params = instruction_src_params_alloc(dst_ins, cf_info->u.switch_.cases_count * 2 + 3, flattener)))
                {
                    vkd3d_free(cf_info->u.switch_.cases);
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                }
                src_params[0] = *cf_info->u.switch_.condition;
                vsir_src_param_init_label(&src_params[1], cf_info->u.switch_.default_block_id);
                vsir_src_param_init_label(&src_params[2], cf_info->u.switch_.merge_block_id);
                for (j = 0; j < cf_info->u.switch_.cases_count; ++j)
                {
                    unsigned int index = j * 2 + 3;
                    vsir_src_param_init(&src_params[index], VKD3DSPR_IMMCONST, VKD3D_DATA_UINT, 0);
                    src_params[index].reg.u.immconst_u32[0] = cf_info->u.switch_.cases[j].value;
                    vsir_src_param_init_label(&src_params[index + 1], cf_info->u.switch_.cases[j].block_id);
                }
                vkd3d_free(cf_info->u.switch_.cases);

                cf_flattener_pop_control_flow_level(flattener);
                break;
            }

            case VKD3DSIH_CASE:
            {
                unsigned int label_id, value;

                if (src->swizzle != VKD3D_SHADER_SWIZZLE(X, X, X, X))
                {
                    WARN("Unexpected src swizzle %#x.\n", src->swizzle);
                    vkd3d_shader_parser_error(parser, VKD3D_SHADER_ERROR_VSIR_INVALID_SWIZZLE,
                            "The swizzle for a switch case value is not scalar X.");
                }
                value = *src->reg.u.immconst_u32;

                if (!vkd3d_array_reserve((void **)&cf_info->u.switch_.cases, &cf_info->u.switch_.cases_size,
                        cf_info->u.switch_.cases_count + 1, sizeof(*cf_info->u.switch_.cases)))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                label_id = cf_flattener_alloc_block_id(flattener);
                if (cf_info->inside_block) /* fall-through */
                    cf_flattener_emit_unconditional_branch(flattener, label_id);

                cf_info->u.switch_.cases[cf_info->u.switch_.cases_count].value = value;
                cf_info->u.switch_.cases[cf_info->u.switch_.cases_count].block_id = label_id;
                ++cf_info->u.switch_.cases_count;

                cf_flattener_emit_label(flattener, label_id);
                cf_flattener_create_block_name(flattener, label_id, "switch%u_case%u", cf_info->u.switch_.id, value);
                cf_info->inside_block = true;
                break;
            }

            case VKD3DSIH_DEFAULT:
                cf_info->u.switch_.default_block_id = cf_flattener_alloc_block_id(flattener);
                if (cf_info->inside_block) /* fall-through */
                    cf_flattener_emit_unconditional_branch(flattener, cf_info->u.switch_.default_block_id);

                cf_flattener_emit_label(flattener, cf_info->u.switch_.default_block_id);

                cf_flattener_create_block_name(flattener, cf_info->u.switch_.default_block_id,
                        "switch%u_default", cf_info->u.switch_.id);
                cf_info->inside_block = true;
                break;

            case VKD3DSIH_BREAK:
            {
                struct cf_flattener_info *breakable_cf_info;

                if (!(breakable_cf_info = cf_flattener_find_innermost_breakable_cf_construct(flattener)))
                {
                    FIXME("Unhandled break instruction.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                if (breakable_cf_info->current_block == VKD3D_BLOCK_LOOP)
                {
                    cf_flattener_emit_unconditional_branch(flattener, breakable_cf_info->u.loop.merge_block_id);
                }
                else if (breakable_cf_info->current_block == VKD3D_BLOCK_SWITCH)
                {
                    cf_flattener_emit_unconditional_branch(flattener, breakable_cf_info->u.switch_.merge_block_id);
                }

                cf_info->inside_block = false;
                break;
            }

            case VKD3DSIH_BREAKP:
            {
                struct cf_flattener_info *loop_cf_info;

                if (!(loop_cf_info = cf_flattener_find_innermost_loop(flattener)))
                {
                    ERR("Invalid 'breakc' instruction outside loop.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                cf_flattener_emit_conditional_branch_and_merge(flattener,
                        src, loop_cf_info->u.loop.merge_block_id, instruction->flags);
                break;
            }

            case VKD3DSIH_CONTINUE:
            {
                struct cf_flattener_info *loop_cf_info;

                if (!(loop_cf_info = cf_flattener_find_innermost_loop(flattener)))
                {
                    ERR("Invalid 'continue' instruction outside loop.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                cf_flattener_emit_unconditional_branch(flattener, loop_cf_info->u.loop.continue_block_id);

                cf_info->inside_block = false;
                break;
            }

            case VKD3DSIH_CONTINUEP:
            {
                struct cf_flattener_info *loop_cf_info;

                if (!(loop_cf_info = cf_flattener_find_innermost_loop(flattener)))
                {
                    ERR("Invalid 'continuec' instruction outside loop.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                cf_flattener_emit_conditional_branch_and_merge(flattener,
                        src, loop_cf_info->u.loop.continue_block_id, instruction->flags);
                break;
            }

            case VKD3DSIH_RET:
                if (!cf_flattener_copy_instruction(flattener, instruction))
                    return VKD3D_ERROR_OUT_OF_MEMORY;

                if (cf_info)
                    cf_info->inside_block = false;
                else
                    main_block_open = false;
                break;

            default:
                if (!cf_flattener_copy_instruction(flattener, instruction))
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                break;
        }
    }

    if (main_block_open)
    {
        if (!(dst_ins = cf_flattener_require_space(flattener, 1)))
            return VKD3D_ERROR_OUT_OF_MEMORY;
        vsir_instruction_init(dst_ins, &flattener->location, VKD3DSIH_RET);
        ++flattener->instruction_count;
    }

    return flattener->allocation_failed ? VKD3D_ERROR_OUT_OF_MEMORY : VKD3D_OK;
}

static enum vkd3d_result flatten_control_flow_constructs(struct vkd3d_shader_parser *parser)
{
    struct vsir_program *program = &parser->program;
    struct cf_flattener flattener = {0};
    enum vkd3d_result result;

    flattener.parser = parser;
    result = cf_flattener_iterate_instruction_array(&flattener);

    if (result >= 0)
    {
        vkd3d_free(parser->program.instructions.elements);
        program->instructions.elements = flattener.instructions;
        program->instructions.capacity = flattener.instruction_capacity;
        program->instructions.count = flattener.instruction_count;
        program->block_count = flattener.block_id;
    }
    else
    {
        vkd3d_free(flattener.instructions);
    }

    vkd3d_free(flattener.control_flow_info);
    /* Simpler to always free these in free_shader_desc(). */
    program->block_names = flattener.block_names;
    program->block_name_count = flattener.block_name_count;

    return result;
}

enum vkd3d_result vkd3d_shader_normalise(struct vkd3d_shader_parser *parser,
        const struct vkd3d_shader_compile_info *compile_info)
{
    struct vkd3d_shader_instruction_array *instructions = &parser->program.instructions;
    enum vkd3d_result result = VKD3D_OK;

    remove_dcl_temps(&parser->program);

    if (!parser->shader_desc.is_dxil)
    {
        if (parser->program.shader_version.type != VKD3D_SHADER_TYPE_PIXEL)
        {
            if ((result = remap_output_signature(parser, compile_info)) < 0)
                return result;
        }

        if (parser->program.shader_version.type == VKD3D_SHADER_TYPE_HULL)
        {
            if ((result = instruction_array_flatten_hull_shader_phases(instructions)) < 0)
                return result;

            if ((result = instruction_array_normalise_hull_shader_control_point_io(instructions,
                    &parser->shader_desc.input_signature)) < 0)
                return result;
        }

        if ((result = shader_normalise_io_registers(parser)) < 0)
            return result;

        if ((result = instruction_array_normalise_flat_constants(&parser->program)) < 0)
            return result;

        remove_dead_code(&parser->program);

        if ((result = flatten_control_flow_constructs(parser)) < 0)
            return result;

        if ((result = normalise_combined_samplers(parser)) < 0)
            return result;
    }

    if (TRACE_ON())
        vkd3d_shader_trace(&parser->program);

    if (!parser->failed && (result = vsir_validate(parser)) < 0)
        return result;

    if (parser->failed)
        result = VKD3D_ERROR_INVALID_SHADER;

    return result;
}

struct validation_context
{
    struct vkd3d_shader_parser *parser;
    const struct vsir_program *program;
    size_t instruction_idx;
    bool invalid_instruction_idx;
    bool dcl_temps_found;
    enum vkd3d_shader_opcode phase;
    enum cf_type
    {
        CF_TYPE_UNKNOWN = 0,
        CF_TYPE_STRUCTURED,
        CF_TYPE_BLOCKS,
    } cf_type;
    bool inside_block;

    struct validation_context_temp_data
    {
        enum vsir_dimension dimension;
        size_t first_seen;
    } *temps;

    struct validation_context_ssa_data
    {
        enum vsir_dimension dimension;
        size_t first_seen;
        uint32_t write_mask;
        uint32_t read_mask;
        size_t first_assigned;
    } *ssas;

    enum vkd3d_shader_opcode *blocks;
    size_t depth;
    size_t blocks_capacity;
};

static void VKD3D_PRINTF_FUNC(3, 4) validator_error(struct validation_context *ctx,
        enum vkd3d_shader_error error, const char *format, ...)
{
    struct vkd3d_string_buffer buf;
    va_list args;

    vkd3d_string_buffer_init(&buf);

    va_start(args, format);
    vkd3d_string_buffer_vprintf(&buf, format, args);
    va_end(args);

    if (ctx->invalid_instruction_idx)
    {
        vkd3d_shader_parser_error(ctx->parser, error, "%s", buf.buffer);
        ERR("VSIR validation error: %s\n", buf.buffer);
    }
    else
    {
        vkd3d_shader_parser_error(ctx->parser, error, "instruction %zu: %s", ctx->instruction_idx + 1, buf.buffer);
        ERR("VSIR validation error: instruction %zu: %s\n", ctx->instruction_idx + 1, buf.buffer);
    }

    vkd3d_string_buffer_cleanup(&buf);
}

static void vsir_validate_src_param(struct validation_context *ctx,
        const struct vkd3d_shader_src_param *src);

static void vsir_validate_register(struct validation_context *ctx,
        const struct vkd3d_shader_register *reg)
{
    unsigned int i;

    if (reg->type >= VKD3DSPR_COUNT)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE, "Invalid register type %#x.",
                reg->type);

    if (reg->precision >= VKD3D_SHADER_REGISTER_PRECISION_COUNT)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_PRECISION, "Invalid register precision %#x.",
                reg->precision);

    if (reg->data_type >= VKD3D_DATA_COUNT)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DATA_TYPE, "Invalid register data type %#x.",
                reg->data_type);

    if (reg->dimension >= VSIR_DIMENSION_COUNT)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid register dimension %#x.",
                reg->dimension);

    if (reg->idx_count > ARRAY_SIZE(reg->idx))
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid register index count %u.",
                reg->idx_count);

    for (i = 0; i < min(reg->idx_count, ARRAY_SIZE(reg->idx)); ++i)
    {
        const struct vkd3d_shader_src_param *param = reg->idx[i].rel_addr;
        if (reg->idx[i].rel_addr)
            vsir_validate_src_param(ctx, param);
    }

    switch (reg->type)
    {
        case VKD3DSPR_TEMP:
        {
            struct validation_context_temp_data *data;

            if (reg->idx_count != 1)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a TEMP register.",
                        reg->idx_count);
                break;
            }

            if (reg->idx[0].rel_addr)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX, "Non-NULL relative address for a TEMP register.");

            if (reg->idx[0].offset >= ctx->parser->program.temp_count)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX, "TEMP register index %u exceeds the maximum count %u.",
                        reg->idx[0].offset, ctx->parser->program.temp_count);
                break;
            }

            data = &ctx->temps[reg->idx[0].offset];

            if (reg->dimension == VSIR_DIMENSION_NONE)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid dimension NONE for a TEMP register.");
                break;
            }

            /* TEMP registers can be scalar or vec4, provided that
             * each individual register always appears with the same
             * dimension. */
            if (data->dimension == VSIR_DIMENSION_NONE)
            {
                data->dimension = reg->dimension;
                data->first_seen = ctx->instruction_idx;
            }
            else if (data->dimension != reg->dimension)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid dimension %#x for a TEMP register: "
                        "it has already been seen with dimension %#x at instruction %zu.",
                        reg->dimension, data->dimension, data->first_seen);
            }
            break;
        }

        case VKD3DSPR_SSA:
        {
            struct validation_context_ssa_data *data;

            if (reg->idx_count != 1)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a SSA register.",
                        reg->idx_count);
                break;
            }

            if (reg->idx[0].rel_addr)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX, "Non-NULL relative address for a SSA register.");

            if (reg->idx[0].offset >= ctx->program->ssa_count)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX,
                        "SSA register index %u exceeds the maximum count %u.",
                        reg->idx[0].offset, ctx->program->ssa_count);
                break;
            }

            data = &ctx->ssas[reg->idx[0].offset];

            if (reg->dimension == VSIR_DIMENSION_NONE)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid dimension NONE for a SSA register.");
                break;
            }

            /* SSA registers can be scalar or vec4, provided that each
             * individual register always appears with the same
             * dimension. */
            if (data->dimension == VSIR_DIMENSION_NONE)
            {
                data->dimension = reg->dimension;
                data->first_seen = ctx->instruction_idx;
            }
            else if (data->dimension != reg->dimension)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid dimension %#x for a SSA register: "
                        "it has already been seen with dimension %#x at instruction %zu.",
                        reg->dimension, data->dimension, data->first_seen);
            }
            break;
        }

        case VKD3DSPR_LABEL:
            if (reg->precision != VKD3D_SHADER_REGISTER_PRECISION_DEFAULT)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_PRECISION, "Invalid precision %#x for a LABEL register.",
                        reg->precision);

            if (reg->data_type != VKD3D_DATA_UINT)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DATA_TYPE, "Invalid data type %#x for a LABEL register.",
                        reg->data_type);

            if (reg->dimension != VSIR_DIMENSION_NONE)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION, "Invalid dimension %#x for a LABEL register.",
                        reg->dimension);

            if (reg->idx_count != 1)
            {
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a LABEL register.",
                        reg->idx_count);
                break;
            }

            if (reg->idx[0].rel_addr)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX, "Non-NULL relative address for a LABEL register.");

            /* Index == 0 is invalid, but it is temporarily allowed
             * for intermediate stages. Once we support validation
             * dialects we can selectively check for that. */
            if (reg->idx[0].offset > ctx->program->block_count)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX,
                        "LABEL register index %u exceeds the maximum count %u.",
                        reg->idx[0].offset, ctx->program->block_count);
            break;

        case VKD3DSPR_NULL:
            if (reg->idx_count != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a NULL register.",
                        reg->idx_count);
            break;

        case VKD3DSPR_IMMCONST:
            if (reg->idx_count != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a IMMCONST register.",
                        reg->idx_count);
            break;

        case VKD3DSPR_IMMCONST64:
            if (reg->idx_count != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_INDEX_COUNT, "Invalid index count %u for a IMMCONST64 register.",
                        reg->idx_count);
            break;

        default:
            break;
    }
}

static void vsir_validate_dst_param(struct validation_context *ctx,
        const struct vkd3d_shader_dst_param *dst)
{
    vsir_validate_register(ctx, &dst->reg);

    if (dst->write_mask & ~VKD3DSP_WRITEMASK_ALL)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_WRITE_MASK, "Destination has invalid write mask %#x.",
                dst->write_mask);

    switch (dst->reg.dimension)
    {
        case VSIR_DIMENSION_SCALAR:
            if (dst->write_mask != VKD3DSP_WRITEMASK_0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_WRITE_MASK, "Scalar destination has invalid write mask %#x.",
                    dst->write_mask);
            break;

        case VSIR_DIMENSION_VEC4:
            if (dst->write_mask == 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_WRITE_MASK, "Vec4 destination has empty write mask.");
            break;

        default:
            if (dst->write_mask != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_WRITE_MASK, "Destination of dimension %u has invalid write mask %#x.",
                    dst->reg.dimension, dst->write_mask);
            break;
    }

    if (dst->modifiers & ~VKD3DSPDM_MASK)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_MODIFIERS, "Destination has invalid modifiers %#x.",
                dst->modifiers);

    switch (dst->shift)
    {
        case 0:
        case 1:
        case 2:
        case 3:
        case 13:
        case 14:
        case 15:
            break;

        default:
            validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SHIFT, "Destination has invalid shift %#x.",
                    dst->shift);
    }

    switch (dst->reg.type)
    {
        case VKD3DSPR_SSA:
            if (dst->reg.idx[0].offset < ctx->parser->program.ssa_count)
            {
                struct validation_context_ssa_data *data = &ctx->ssas[dst->reg.idx[0].offset];

                if (data->write_mask == 0)
                {
                    data->write_mask = dst->write_mask;
                    data->first_assigned = ctx->instruction_idx;
                }
                else
                {
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SSA_USAGE,
                            "SSA register is already assigned at instruction %zu.",
                            data->first_assigned);
                }
            }
            break;

        case VKD3DSPR_IMMCONST:
            validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                    "Invalid IMMCONST register used as destination parameter.");
            break;

        case VKD3DSPR_IMMCONST64:
            validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                    "Invalid IMMCONST64 register used as destination parameter.");
            break;

        default:
            break;
    }
}

static void vsir_validate_src_param(struct validation_context *ctx,
        const struct vkd3d_shader_src_param *src)
{
    vsir_validate_register(ctx, &src->reg);

    if (src->swizzle & ~0x03030303u)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SWIZZLE, "Source has invalid swizzle %#x.",
                src->swizzle);

    if (src->reg.dimension != VSIR_DIMENSION_VEC4 && src->swizzle != 0)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SWIZZLE, "Source of dimension %u has invalid swizzle %#x.",
                src->reg.dimension, src->swizzle);

    if (src->modifiers >= VKD3DSPSM_COUNT)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_MODIFIERS, "Source has invalid modifiers %#x.",
                src->modifiers);

    switch (src->reg.type)
    {
        case VKD3DSPR_SSA:
            if (src->reg.idx[0].offset < ctx->parser->program.ssa_count)
            {
                struct validation_context_ssa_data *data = &ctx->ssas[src->reg.idx[0].offset];
                unsigned int i;

                for (i = 0; i < VKD3D_VEC4_SIZE; ++i)
                    data->read_mask |= (1u << vsir_swizzle_get_component(src->swizzle, i));
            }
            break;

        default:
            break;
    }
}

static void vsir_validate_dst_count(struct validation_context *ctx,
        const struct vkd3d_shader_instruction *instruction, unsigned int count)
{
    if (instruction->dst_count != count)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DEST_COUNT,
                "Invalid destination count %u for an instruction of type %#x, expected %u.",
                        instruction->dst_count, instruction->handler_idx, count);
}

static void vsir_validate_src_count(struct validation_context *ctx,
        const struct vkd3d_shader_instruction *instruction, unsigned int count)
{
    if (instruction->src_count != count)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SOURCE_COUNT,
                "Invalid source count %u for an instruction of type %#x, expected %u.",
                instruction->src_count, instruction->handler_idx, count);
}

static bool vsir_validate_src_min_count(struct validation_context *ctx,
        const struct vkd3d_shader_instruction *instruction, unsigned int count)
{
    if (instruction->src_count < count)
    {
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SOURCE_COUNT,
                "Invalid source count %u for an instruction of type %#x, expected at least %u.",
                instruction->src_count, instruction->handler_idx, count);
        return false;
    }

    return true;
}

static bool vsir_validate_src_max_count(struct validation_context *ctx,
        const struct vkd3d_shader_instruction *instruction, unsigned int count)
{
    if (instruction->src_count > count)
    {
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SOURCE_COUNT,
                "Invalid source count %u for an instruction of type %#x, expected at most %u.",
                instruction->src_count, instruction->handler_idx, count);
        return false;
    }

    return true;
}

static const char *name_from_cf_type(enum cf_type type)
{
    switch (type)
    {
        case CF_TYPE_STRUCTURED:
            return "structured";
        case CF_TYPE_BLOCKS:
            return "block-based";
        default:
            vkd3d_unreachable();
    }
}

static void vsir_validate_cf_type(struct validation_context *ctx,
        const struct vkd3d_shader_instruction *instruction, enum cf_type expected_type)
{
    assert(ctx->cf_type != CF_TYPE_UNKNOWN);
    assert(expected_type != CF_TYPE_UNKNOWN);
    if (ctx->cf_type != expected_type)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Invalid instruction %#x in %s shader.",
                instruction->handler_idx, name_from_cf_type(ctx->cf_type));
}

static void vsir_validate_instruction(struct validation_context *ctx)
{
    const struct vkd3d_shader_version *version = &ctx->program->shader_version;
    const struct vkd3d_shader_instruction *instruction;
    size_t i;

    instruction = &ctx->program->instructions.elements[ctx->instruction_idx];
    ctx->parser->location = instruction->location;

    for (i = 0; i < instruction->dst_count; ++i)
        vsir_validate_dst_param(ctx, &instruction->dst[i]);

    for (i = 0; i < instruction->src_count; ++i)
        vsir_validate_src_param(ctx, &instruction->src[i]);

    if (instruction->handler_idx >= VKD3DSIH_INVALID)
    {
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_HANDLER, "Invalid instruction handler %#x.",
                instruction->handler_idx);
    }

    switch (instruction->handler_idx)
    {
        case VKD3DSIH_HS_DECLS:
        case VKD3DSIH_HS_CONTROL_POINT_PHASE:
        case VKD3DSIH_HS_FORK_PHASE:
        case VKD3DSIH_HS_JOIN_PHASE:
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (version->type != VKD3D_SHADER_TYPE_HULL)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_HANDLER, "Phase instruction %#x is only valid in a hull shader.",
                        instruction->handler_idx);
            if (ctx->depth != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Phase instruction %#x must appear to top level.",
                        instruction->handler_idx);
            ctx->phase = instruction->handler_idx;
            ctx->dcl_temps_found = false;
            return;

        default:
            break;
    }

    if (version->type == VKD3D_SHADER_TYPE_HULL && ctx->phase == VKD3DSIH_INVALID)
        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_HANDLER,
                "Instruction %#x appear before any phase instruction in a hull shader.",
                instruction->handler_idx);

    /* We support two different control flow types in shaders:
     * block-based, like DXIL and SPIR-V, and structured, like D3DBC
     * and TPF. The shader is detected as block-based when its first
     * instruction, except for DCL_* and phases, is a LABEL. Currently
     * we mandate that each shader is either purely block-based or
     * purely structured. In principle we could allow structured
     * constructs in a block, provided they are confined in a single
     * block, but need for that hasn't arisen yet, so we don't. */
    if (ctx->cf_type == CF_TYPE_UNKNOWN && !(instruction->handler_idx >= VKD3DSIH_DCL
            && instruction->handler_idx <= VKD3DSIH_DCL_VERTICES_OUT))
    {
        if (instruction->handler_idx == VKD3DSIH_LABEL)
            ctx->cf_type = CF_TYPE_BLOCKS;
        else
            ctx->cf_type = CF_TYPE_STRUCTURED;
    }

    if (ctx->cf_type == CF_TYPE_BLOCKS && !vsir_instruction_is_dcl(instruction))
    {
        switch (instruction->handler_idx)
        {
            case VKD3DSIH_LABEL:
                if (ctx->inside_block)
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Invalid LABEL instruction inside a block.");
                ctx->inside_block = true;
                break;

            case VKD3DSIH_RET:
            case VKD3DSIH_BRANCH:
            case VKD3DSIH_SWITCH_MONOLITHIC:
                if (!ctx->inside_block)
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Invalid instruction %#x outside any block.",
                            instruction->handler_idx);
                ctx->inside_block = false;
                break;

            default:
                if (!ctx->inside_block)
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Invalid instruction %#x outside any block.",
                            instruction->handler_idx);
                break;
        }
    }

    switch (instruction->handler_idx)
    {
        case VKD3DSIH_DCL_TEMPS:
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->dcl_temps_found)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_DUPLICATE_DCL_TEMPS, "Duplicate DCL_TEMPS instruction.");
            if (instruction->declaration.count > ctx->program->temp_count)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DCL_TEMPS,
                        "Invalid DCL_TEMPS count %u, expected at most %u.",
                        instruction->declaration.count, ctx->program->temp_count);
            ctx->dcl_temps_found = true;
            break;

        case VKD3DSIH_IF:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 1);
            if (!vkd3d_array_reserve((void **)&ctx->blocks, &ctx->blocks_capacity, ctx->depth + 1, sizeof(*ctx->blocks)))
                return;
            ctx->blocks[ctx->depth++] = instruction->handler_idx;
            break;

        case VKD3DSIH_IFC:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 2);
            if (!vkd3d_array_reserve((void **)&ctx->blocks, &ctx->blocks_capacity, ctx->depth + 1, sizeof(*ctx->blocks)))
                return;
            ctx->blocks[ctx->depth++] = VKD3DSIH_IF;
            break;

        case VKD3DSIH_ELSE:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->depth == 0 || ctx->blocks[ctx->depth - 1] != VKD3DSIH_IF)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "ELSE instruction doesn't terminate IF block.");
            else
                ctx->blocks[ctx->depth - 1] = instruction->handler_idx;
            break;

        case VKD3DSIH_ENDIF:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->depth == 0 || (ctx->blocks[ctx->depth - 1] != VKD3DSIH_IF && ctx->blocks[ctx->depth - 1] != VKD3DSIH_ELSE))
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "ENDIF instruction doesn't terminate IF/ELSE block.");
            else
                --ctx->depth;
            break;

        case VKD3DSIH_LOOP:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, version->major <= 3 ? 2 : 0);
            if (!vkd3d_array_reserve((void **)&ctx->blocks, &ctx->blocks_capacity, ctx->depth + 1, sizeof(*ctx->blocks)))
                return;
            ctx->blocks[ctx->depth++] = instruction->handler_idx;
            break;

        case VKD3DSIH_ENDLOOP:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->depth == 0 || ctx->blocks[ctx->depth - 1] != VKD3DSIH_LOOP)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "ENDLOOP instruction doesn't terminate LOOP block.");
            else
                --ctx->depth;
            break;

        case VKD3DSIH_REP:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 1);
            if (!vkd3d_array_reserve((void **)&ctx->blocks, &ctx->blocks_capacity, ctx->depth + 1, sizeof(*ctx->blocks)))
                return;
            ctx->blocks[ctx->depth++] = instruction->handler_idx;
            break;

        case VKD3DSIH_ENDREP:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->depth == 0 || ctx->blocks[ctx->depth - 1] != VKD3DSIH_REP)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "ENDREP instruction doesn't terminate REP block.");
            else
                --ctx->depth;
            break;

        case VKD3DSIH_SWITCH:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 1);
            if (!vkd3d_array_reserve((void **)&ctx->blocks, &ctx->blocks_capacity, ctx->depth + 1, sizeof(*ctx->blocks)))
                return;
            ctx->blocks[ctx->depth++] = instruction->handler_idx;
            break;

        case VKD3DSIH_ENDSWITCH:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_STRUCTURED);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            if (ctx->depth == 0 || ctx->blocks[ctx->depth - 1] != VKD3DSIH_SWITCH)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "ENDSWITCH instruction doesn't terminate SWITCH block.");
            else
                --ctx->depth;
            break;

        case VKD3DSIH_RET:
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 0);
            break;

        case VKD3DSIH_LABEL:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_BLOCKS);
            vsir_validate_dst_count(ctx, instruction, 0);
            vsir_validate_src_count(ctx, instruction, 1);
            if (instruction->src_count >= 1 && !vsir_register_is_label(&instruction->src[0].reg))
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                        "Invalid register of type %#x in a LABEL instruction, expected LABEL.",
                        instruction->src[0].reg.type);
            break;

        case VKD3DSIH_BRANCH:
            vsir_validate_cf_type(ctx, instruction, CF_TYPE_BLOCKS);
            vsir_validate_dst_count(ctx, instruction, 0);
            if (!vsir_validate_src_min_count(ctx, instruction, 1))
                break;
            if (vsir_register_is_label(&instruction->src[0].reg))
            {
                /* Unconditional branch: parameters are jump label,
                 * optional merge label, optional continue label. */
                vsir_validate_src_max_count(ctx, instruction, 3);

                for (i = 0; i < instruction->src_count; ++i)
                {
                    if (!vsir_register_is_label(&instruction->src[i].reg))
                        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                                "Invalid register of type %#x in unconditional BRANCH instruction, expected LABEL.",
                                instruction->src[i].reg.type);
                }
            }
            else
            {
                /* Conditional branch: parameters are condition, true
                 * jump label, false jump label, optional merge label,
                 * optional continue label. */
                vsir_validate_src_min_count(ctx, instruction, 3);
                vsir_validate_src_max_count(ctx, instruction, 5);

                for (i = 1; i < instruction->src_count; ++i)
                {
                    if (!vsir_register_is_label(&instruction->src[i].reg))
                        validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                                "Invalid register of type %#x in conditional BRANCH instruction, expected LABEL.",
                                instruction->src[i].reg.type);
                }
            }
            break;

        case VKD3DSIH_SWITCH_MONOLITHIC:
        {
            unsigned int case_count;

            vsir_validate_cf_type(ctx, instruction, CF_TYPE_BLOCKS);
            vsir_validate_dst_count(ctx, instruction, 0);
            /* Parameters are source, default label, merge label and
             * then pairs of constant value and case label. */
            if (!vsir_validate_src_min_count(ctx, instruction, 3))
                break;
            if (instruction->src_count % 2 != 1)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SOURCE_COUNT,
                        "Invalid source count %u for a monolithic SWITCH instruction, it must be an odd number.",
                        instruction->src_count);

            if (!vsir_register_is_label(&instruction->src[1].reg))
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                        "Invalid default label register of type %#x in monolithic SWITCH instruction, expected LABEL.",
                        instruction->src[1].reg.type);

            if (!vsir_register_is_label(&instruction->src[2].reg))
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                        "Invalid merge label register of type %#x in monolithic SWITCH instruction, expected LABEL.",
                        instruction->src[2].reg.type);

            case_count = (instruction->src_count - 3) / 2;

            for (i = 0; i < case_count; ++i)
            {
                unsigned int value_idx = 3 + 2 * i;
                unsigned int label_idx = 3 + 2 * i + 1;

                if (!register_is_constant(&instruction->src[value_idx].reg))
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                            "Invalid value register for case %zu of type %#x in monolithic SWITCH instruction, "
                            "expected IMMCONST or IMMCONST64.", i, instruction->src[value_idx].reg.type);

                if (!vsir_register_is_label(&instruction->src[label_idx].reg))
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                            "Invalid label register for case %zu of type %#x in monolithic SWITCH instruction, "
                            "expected LABEL.", i, instruction->src[value_idx].reg.type);
            }
            break;
        }

        case VKD3DSIH_PHI:
        {
            unsigned int incoming_count;

            vsir_validate_cf_type(ctx, instruction, CF_TYPE_BLOCKS);
            vsir_validate_dst_count(ctx, instruction, 1);
            vsir_validate_src_min_count(ctx, instruction, 2);
            if (instruction->src_count % 2 != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SOURCE_COUNT,
                        "Invalid source count %u for a PHI instruction, it must be an even number.",
                        instruction->src_count);
            incoming_count = instruction->src_count / 2;

            if (!register_is_ssa(&instruction->dst[0].reg))
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                        "Invalid destination of type %#x in PHI instruction, expected SSA.",
                        instruction->dst[0].reg.type);

            if (instruction->dst[0].reg.dimension != VSIR_DIMENSION_SCALAR)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION,
                        "Invalid destination dimension %#x in PHI instruction, expected scalar.",
                        instruction->dst[0].reg.dimension);

            if (instruction->dst[0].modifiers != VKD3DSPDM_NONE)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_MODIFIERS,
                        "Invalid modifiers %#x for the destination of a PHI instruction, expected none.",
                        instruction->dst[0].modifiers);

            if (instruction->dst[0].shift != 0)
                validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SHIFT,
                        "Invalid shift %#x for the destination of a PHI instruction, expected none.",
                        instruction->dst[0].shift);

            for (i = 0; i < incoming_count; ++i)
            {
                unsigned int value_idx = 2 * i;
                unsigned int label_idx = 2 * i + 1;

                if (!register_is_constant(&instruction->src[value_idx].reg) && !register_is_ssa(&instruction->src[value_idx].reg))
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                            "Invalid value register for incoming %zu of type %#x in PHI instruction, "
                            "expected SSA, IMMCONST or IMMCONST64.", i, instruction->src[value_idx].reg.type);

                if (instruction->src[value_idx].reg.dimension != VSIR_DIMENSION_SCALAR)
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_DIMENSION,
                            "Invalid value dimension %#x for incoming %zu in PHI instruction, expected scalar.",
                            instruction->src[value_idx].reg.dimension, i);

                if (!vsir_register_is_label(&instruction->src[label_idx].reg))
                    validator_error(ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_REGISTER_TYPE,
                            "Invalid label register for case %zu of type %#x in PHI instruction, "
                            "expected LABEL.", i, instruction->src[value_idx].reg.type);
            }
            break;
        }

        default:
            break;
    }
}

enum vkd3d_result vsir_validate(struct vkd3d_shader_parser *parser)
{
    struct validation_context ctx =
    {
        .parser = parser,
        .program = &parser->program,
        .phase = VKD3DSIH_INVALID,
    };
    unsigned int i;

    if (!(parser->config_flags & VKD3D_SHADER_CONFIG_FLAG_FORCE_VALIDATION))
        return VKD3D_OK;

    if (!(ctx.temps = vkd3d_calloc(ctx.program->temp_count, sizeof(*ctx.temps))))
        goto fail;

    if (!(ctx.ssas = vkd3d_calloc(ctx.program->ssa_count, sizeof(*ctx.ssas))))
        goto fail;

    for (ctx.instruction_idx = 0; ctx.instruction_idx < parser->program.instructions.count; ++ctx.instruction_idx)
        vsir_validate_instruction(&ctx);

    ctx.invalid_instruction_idx = true;

    if (ctx.depth != 0)
        validator_error(&ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "%zu nested blocks were not closed.", ctx.depth);

    if (ctx.inside_block)
        validator_error(&ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_CONTROL_FLOW, "Last block was not closed.");

    for (i = 0; i < ctx.program->ssa_count; ++i)
    {
        struct validation_context_ssa_data *data = &ctx.ssas[i];

        if ((data->write_mask | data->read_mask) != data->write_mask)
            validator_error(&ctx, VKD3D_SHADER_ERROR_VSIR_INVALID_SSA_USAGE,
                    "SSA register %u has invalid read mask %#x, which is not a subset of the write mask %#x "
                    "at the point of definition.", i, data->read_mask, data->write_mask);
    }

    vkd3d_free(ctx.blocks);
    vkd3d_free(ctx.temps);
    vkd3d_free(ctx.ssas);

    return VKD3D_OK;

fail:
    vkd3d_free(ctx.blocks);
    vkd3d_free(ctx.temps);
    vkd3d_free(ctx.ssas);

    return VKD3D_ERROR_OUT_OF_MEMORY;
}
