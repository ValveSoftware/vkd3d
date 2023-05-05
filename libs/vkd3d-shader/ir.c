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
    ins->handler_idx = VKD3DSIH_NOP;
    ins->dst_count = 0;
    ins->src_count = 0;
    ins->dst = NULL;
    ins->src = NULL;
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
            reg->type = VKD3DSPR_IMMCONST;
            reg->precision = VKD3D_SHADER_REGISTER_PRECISION_DEFAULT;
            reg->non_uniform = false;
            reg->idx[0].offset = ~0u;
            reg->idx[0].rel_addr = NULL;
            reg->idx[1].offset = ~0u;
            reg->idx[1].rel_addr = NULL;
            reg->idx[2].offset = ~0u;
            reg->idx[2].rel_addr = NULL;
            reg->idx_count = 0;
            reg->immconst_type = VKD3D_IMMCONST_SCALAR;
            reg->u.immconst_uint[0] = instance_id;
            continue;
        }
        shader_register_eliminate_phase_addressing(reg, instance_id);
    }

    for (i = 0; i < ins->dst_count; ++i)
        shader_register_eliminate_phase_addressing((struct vkd3d_shader_register *)&ins->dst[i].reg, instance_id);
}

struct hull_flattener
{
    struct vkd3d_shader_instruction_array instructions;

    unsigned int max_temp_count;
    unsigned int temp_dcl_idx;

    unsigned int instance_count;
    unsigned int phase_body_idx;
    enum vkd3d_shader_opcode phase;
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
    else if (ins->handler_idx == VKD3DSIH_DCL_TEMPS && normaliser->phase != VKD3DSIH_INVALID)
    {
        /* Leave only the first temp declaration and set it to the max count later. */
        if (!normaliser->max_temp_count)
            normaliser->temp_dcl_idx = index;
        else
            vkd3d_shader_instruction_make_nop(ins);
        normaliser->max_temp_count = max(normaliser->max_temp_count, ins->declaration.count);
        return;
    }

    if (normaliser->phase == VKD3DSIH_INVALID || shader_instruction_is_dcl(ins))
        return;

    if (normaliser->phase_body_idx == ~0u)
        normaliser->phase_body_idx = index;

    if (ins->handler_idx == VKD3DSIH_RET)
    {
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

static void shader_register_init(struct vkd3d_shader_register *reg, enum vkd3d_shader_register_type reg_type,
        enum vkd3d_data_type data_type, unsigned int idx_count)
{
    reg->type = reg_type;
    reg->precision = VKD3D_SHADER_REGISTER_PRECISION_DEFAULT;
    reg->non_uniform = false;
    reg->data_type = data_type;
    reg->idx[0].offset = ~0u;
    reg->idx[0].rel_addr = NULL;
    reg->idx[1].offset = ~0u;
    reg->idx[1].rel_addr = NULL;
    reg->idx[2].offset = ~0u;
    reg->idx[2].rel_addr = NULL;
    reg->idx_count = idx_count;
    reg->immconst_type = VKD3D_IMMCONST_SCALAR;
}

static void shader_instruction_init(struct vkd3d_shader_instruction *ins, enum vkd3d_shader_opcode handler_idx)
{
    memset(ins, 0, sizeof(*ins));
    ins->handler_idx = handler_idx;
}

enum vkd3d_result instruction_array_flatten_hull_shader_phases(struct vkd3d_shader_instruction_array *src_instructions)
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
        if (flattener.temp_dcl_idx)
            instructions->elements[flattener.temp_dcl_idx].declaration.count = flattener.max_temp_count;

        if (!shader_instruction_array_reserve(&flattener.instructions, flattener.instructions.count + 1))
            return VKD3D_ERROR_OUT_OF_MEMORY;
        shader_instruction_init(&instructions->elements[instructions->count++], VKD3DSIH_RET);
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

    shader_register_init(&rel_addr->reg, VKD3DSPR_OUTPOINTID, VKD3D_DATA_UINT, 0);
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
    shader_register_init(&param->reg, reg_type, vkd3d_data_type_from_component_type(e->component_type), idx_count);
}

static enum vkd3d_result control_point_normaliser_emit_hs_input(struct control_point_normaliser *normaliser,
        const struct shader_signature *s, unsigned int input_control_point_count, unsigned int dst)
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
    shader_instruction_init(ins, VKD3DSIH_HS_CONTROL_POINT_PHASE);
    ins->flags = 1;
    ++ins;

    for (i = 0; i < s->element_count; ++i)
    {
        e = &s->elements[i];
        if (!e->used_mask)
            continue;

        if (e->sysval_semantic != VKD3D_SHADER_SV_NONE)
        {
            shader_instruction_init(ins, VKD3DSIH_DCL_INPUT_SIV);
            param = &ins->declaration.register_semantic.reg;
            ins->declaration.register_semantic.sysval_semantic = vkd3d_siv_from_sysval(e->sysval_semantic);
        }
        else
        {
            shader_instruction_init(ins, VKD3DSIH_DCL_INPUT);
            param = &ins->declaration.dst;
        }

        shader_dst_param_io_init(param, e, VKD3DSPR_INPUT, 2);
        param->reg.idx[0].offset = input_control_point_count;
        param->reg.idx[1].offset = i;

        ++ins;
    }

    return VKD3D_OK;
}

enum vkd3d_result instruction_array_normalise_hull_shader_control_point_io(
        struct vkd3d_shader_instruction_array *src_instructions, const struct shader_signature *input_signature)
{
    struct vkd3d_shader_instruction_array *instructions;
    struct control_point_normaliser normaliser;
    unsigned int input_control_point_count;
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
                    shader_dst_param_normalise_outpointid((struct vkd3d_shader_dst_param *)&ins->dst[j], &normaliser);
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
                ret = control_point_normaliser_emit_hs_input(&normaliser, input_signature,
                        input_control_point_count, i);
                *src_instructions = normaliser.instructions;
                return ret;
            default:
                break;
        }
    }

    *src_instructions = normaliser.instructions;
    return VKD3D_OK;
}
