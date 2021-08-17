/*
 * HLSL code generation for DXBC shader models 4-5
 *
 * Copyright 2019-2020 Zebediah Figura for CodeWeavers
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

#include "hlsl.h"
#include <stdio.h>
#include "vkd3d_d3dcommon.h"
#include "sm4.h"

static void write_sm4_rdef(struct hlsl_ctx *ctx, struct dxbc_writer *dxbc)
{
    size_t cbuffers_offset, resources_offset, creator_offset, string_offset;
    size_t cbuffer_position, resource_position, creator_position;
    const struct hlsl_profile_info *profile = ctx->profile;
    struct vkd3d_bytecode_buffer buffer = {0};
    const struct hlsl_buffer *cbuffer;
    unsigned int cbuffer_count = 0, i;

    static const uint16_t target_types[] =
    {
        0xffff, /* PIXEL */
        0xfffe, /* VERTEX */
        0x4753, /* GEOMETRY */
        0x4853, /* HULL */
        0x4453, /* DOMAIN */
        0x4353, /* COMPUTE */
    };

    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        if (cbuffer->reg.allocated)
            ++cbuffer_count;
    }

    put_u32(&buffer, cbuffer_count);
    cbuffer_position = put_u32(&buffer, 0);
    put_u32(&buffer, cbuffer_count); /* bound resource count */
    resource_position = put_u32(&buffer, 0);
    put_u32(&buffer, (target_types[profile->type] << 16) | (profile->major_version << 8) | profile->minor_version);
    put_u32(&buffer, 0); /* FIXME: compilation flags */
    creator_position = put_u32(&buffer, 0);

    /* Bound resources. */

    resources_offset = bytecode_get_size(&buffer);
    set_u32(&buffer, resource_position, resources_offset);
    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        uint32_t flags = 0;

        if (!cbuffer->reg.allocated)
            continue;

        if (cbuffer->reservation.type)
            flags |= D3D_SIF_USERPACKED;

        put_u32(&buffer, 0); /* name */
        put_u32(&buffer, cbuffer->type == HLSL_BUFFER_CONSTANT ? D3D_SIT_CBUFFER : D3D_SIT_TBUFFER);
        put_u32(&buffer, 0); /* return type */
        put_u32(&buffer, 0); /* dimension */
        put_u32(&buffer, 0); /* multisample count */
        put_u32(&buffer, cbuffer->reg.id); /* bind point */
        put_u32(&buffer, 1); /* bind count */
        put_u32(&buffer, flags); /* flags */
    }

    i = 0;
    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        if (!cbuffer->reg.allocated)
            continue;

        string_offset = put_string(&buffer, cbuffer->name);
        set_u32(&buffer, resources_offset + i++ * 8 * sizeof(uint32_t), string_offset);
    }

    /* Buffers. */

    cbuffers_offset = bytecode_get_size(&buffer);
    set_u32(&buffer, cbuffer_position, cbuffers_offset);
    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        const struct hlsl_ir_var *var;
        unsigned int var_count = 0;

        if (!cbuffer->reg.allocated)
            continue;

        LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
        {
            if (var->is_uniform && var->buffer == cbuffer)
                ++var_count;
        }

        put_u32(&buffer, 0); /* name */
        put_u32(&buffer, var_count);
        put_u32(&buffer, 0); /* variable offset */
        put_u32(&buffer, align(cbuffer->size, 4) * sizeof(float));
        put_u32(&buffer, 0); /* FIXME: flags */
        put_u32(&buffer, cbuffer->type == HLSL_BUFFER_CONSTANT ? D3D_CT_CBUFFER : D3D_CT_TBUFFER);
    }

    i = 0;
    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        if (!cbuffer->reg.allocated)
            continue;

        string_offset = put_string(&buffer, cbuffer->name);
        set_u32(&buffer, cbuffers_offset + i++ * 6 * sizeof(uint32_t), string_offset);
    }

    i = 0;
    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        size_t vars_start = bytecode_get_size(&buffer);
        const struct hlsl_ir_var *var;

        if (!cbuffer->reg.allocated)
            continue;

        set_u32(&buffer, cbuffers_offset + (i++ * 6 + 2) * sizeof(uint32_t), vars_start);

        LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
        {
            if (var->is_uniform && var->buffer == cbuffer)
            {
                uint32_t flags = 0;

                if (var->last_read)
                    flags |= D3D_SVF_USED;

                put_u32(&buffer, 0); /* name */
                put_u32(&buffer, var->buffer_offset);
                put_u32(&buffer, var->data_type->reg_size * sizeof(float));
                put_u32(&buffer, flags);
                put_u32(&buffer, 0); /* FIXME: type */
                put_u32(&buffer, 0); /* FIXME: default value */
            }
        }
    }

    creator_offset = put_string(&buffer, vkd3d_shader_get_version(NULL, NULL));
    set_u32(&buffer, creator_position, creator_offset);

    dxbc_writer_add_section(dxbc, TAG_RDEF, buffer.data, buffer.size);
}

static void write_sm4_shdr(struct hlsl_ctx *ctx, struct dxbc_writer *dxbc)
{
    const struct hlsl_profile_info *profile = ctx->profile;
    struct vkd3d_bytecode_buffer buffer = {0};

    static const uint16_t shader_types[VKD3D_SHADER_TYPE_COUNT] =
    {
        VKD3D_SM4_PS,
        VKD3D_SM4_VS,
        VKD3D_SM4_GS,
        VKD3D_SM5_HS,
        VKD3D_SM5_DS,
        VKD3D_SM5_CS,
        0, /* EFFECT */
        0, /* TEXTURE */
        VKD3D_SM4_LIB,
    };

    put_u32(&buffer, (shader_types[profile->type] << 16) | (profile->major_version << 4) | profile->minor_version);
    put_u32(&buffer, 0); /* FIXME: instruction token count */

    dxbc_writer_add_section(dxbc, TAG_SHDR, buffer.data, buffer.size);
}

int hlsl_sm4_write(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out)
{
    struct dxbc_writer dxbc;
    size_t i;
    int ret;

    dxbc_writer_init(&dxbc);

    write_sm4_rdef(ctx, &dxbc);
    write_sm4_shdr(ctx, &dxbc);

    ret = dxbc_writer_write(&dxbc, out);
    for (i = 0; i < dxbc.section_count; ++i)
        vkd3d_free((void *)dxbc.sections[i].data);
    return ret;
}
