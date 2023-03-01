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

    for (i = 0; i < ARRAY_SIZE(reg->idx) && reg->idx[i].offset != ~0u; ++i)
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
            reg->immconst_type = VKD3D_IMMCONST_SCALAR;
            reg->u.immconst_uint[0] = instance_id;
            continue;
        }
        shader_register_eliminate_phase_addressing(reg, instance_id);
    }

    for (i = 0; i < ins->dst_count; ++i)
        shader_register_eliminate_phase_addressing((struct vkd3d_shader_register *)&ins->dst[i].reg, instance_id);
}

static bool normaliser_is_in_fork_or_join_phase(const struct vkd3d_shader_normaliser *normaliser)
{
    return normaliser->phase == VKD3DSIH_HS_FORK_PHASE || normaliser->phase == VKD3DSIH_HS_JOIN_PHASE;
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

static void shader_normaliser_eliminate_phase_related_dcls(struct vkd3d_shader_normaliser *normaliser,
        unsigned int index, struct shader_phase_location_array *locations)
{
    struct vkd3d_shader_instruction *ins = &normaliser->instructions.elements[index];
    struct shader_phase_location *loc;
    bool b;

    if (ins->handler_idx == VKD3DSIH_HS_FORK_PHASE || ins->handler_idx == VKD3DSIH_HS_JOIN_PHASE)
    {
        b = normaliser_is_in_fork_or_join_phase(normaliser);
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

static enum vkd3d_result shader_normaliser_flatten_phases(struct vkd3d_shader_normaliser *normaliser,
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

static void shader_instruction_init(struct vkd3d_shader_instruction *ins, enum vkd3d_shader_opcode handler_idx)
{
    memset(ins, 0, sizeof(*ins));
    ins->handler_idx = handler_idx;
}

void shader_normaliser_init(struct vkd3d_shader_normaliser *normaliser,
        struct vkd3d_shader_instruction_array *instructions)
{
    memset(normaliser, 0, sizeof(*normaliser));
    normaliser->phase = VKD3DSIH_INVALID;
    normaliser->instructions = *instructions;
    memset(instructions, 0, sizeof(*instructions));
}

enum vkd3d_result shader_normaliser_flatten_hull_shader_phases(struct vkd3d_shader_normaliser *normaliser)
{
    struct vkd3d_shader_instruction_array *instructions = &normaliser->instructions;
    struct shader_phase_location_array locations;
    enum vkd3d_result result = VKD3D_OK;
    unsigned int i;

    for (i = 0, locations.count = 0; i < instructions->count; ++i)
        shader_normaliser_eliminate_phase_related_dcls(normaliser, i, &locations);

    if ((result = shader_normaliser_flatten_phases(normaliser, &locations)) < 0)
        return result;

    if (normaliser->phase != VKD3DSIH_INVALID)
    {
        if (normaliser->temp_dcl_idx)
            instructions->elements[normaliser->temp_dcl_idx].declaration.count = normaliser->max_temp_count;

        if (!shader_instruction_array_reserve(&normaliser->instructions, normaliser->instructions.count + 1))
            return VKD3D_ERROR_OUT_OF_MEMORY;
        shader_instruction_init(&instructions->elements[instructions->count++], VKD3DSIH_RET);
    }

    return result;
}

void shader_normaliser_destroy(struct vkd3d_shader_normaliser *normaliser)
{
    shader_instruction_array_destroy(&normaliser->instructions);
}
