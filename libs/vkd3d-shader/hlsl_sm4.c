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

static const struct hlsl_type *get_array_type(const struct hlsl_type *type)
{
    if (type->type == HLSL_CLASS_ARRAY)
        return get_array_type(type->e.array.type);
    return type;
}

static unsigned int get_array_size(const struct hlsl_type *type)
{
    if (type->type == HLSL_CLASS_ARRAY)
        return get_array_size(type->e.array.type) * type->e.array.elements_count;
    return 1;
}

static D3D_SHADER_VARIABLE_CLASS sm4_class(const struct hlsl_type *type)
{
    switch (type->type)
    {
        case HLSL_CLASS_ARRAY:
            return sm4_class(type->e.array.type);
        case HLSL_CLASS_MATRIX:
            assert(type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK);
            if (type->modifiers & HLSL_MODIFIER_COLUMN_MAJOR)
                return D3D_SVC_MATRIX_COLUMNS;
            else
                return D3D_SVC_MATRIX_ROWS;
        case HLSL_CLASS_OBJECT:
            return D3D_SVC_OBJECT;
        case HLSL_CLASS_SCALAR:
            return D3D_SVC_SCALAR;
        case HLSL_CLASS_STRUCT:
            return D3D_SVC_STRUCT;
        case HLSL_CLASS_VECTOR:
            return D3D_SVC_VECTOR;
        default:
            ERR("Invalid class %#x.\n", type->type);
            assert(0);
            return 0;
    }
}

static D3D_SHADER_VARIABLE_TYPE sm4_base_type(const struct hlsl_type *type)
{
    switch (type->base_type)
    {
        case HLSL_TYPE_BOOL:
            return D3D_SVT_BOOL;
        case HLSL_TYPE_FLOAT:
        case HLSL_TYPE_HALF:
            return D3D_SVT_FLOAT;
        case HLSL_TYPE_INT:
            return D3D_SVT_INT;
        case HLSL_TYPE_PIXELSHADER:
            return D3D_SVT_PIXELSHADER;
        case HLSL_TYPE_SAMPLER:
            switch (type->sampler_dim)
            {
                case HLSL_SAMPLER_DIM_1D:
                    return D3D_SVT_SAMPLER1D;
                case HLSL_SAMPLER_DIM_2D:
                    return D3D_SVT_SAMPLER2D;
                case HLSL_SAMPLER_DIM_3D:
                    return D3D_SVT_SAMPLER3D;
                case HLSL_SAMPLER_DIM_CUBE:
                    return D3D_SVT_SAMPLERCUBE;
                case HLSL_SAMPLER_DIM_GENERIC:
                    return D3D_SVT_SAMPLER;
                default:
                    assert(0);
            }
            break;
        case HLSL_TYPE_STRING:
            return D3D_SVT_STRING;
        case HLSL_TYPE_TEXTURE:
            switch (type->sampler_dim)
            {
                case HLSL_SAMPLER_DIM_1D:
                    return D3D_SVT_TEXTURE1D;
                case HLSL_SAMPLER_DIM_2D:
                    return D3D_SVT_TEXTURE2D;
                case HLSL_SAMPLER_DIM_3D:
                    return D3D_SVT_TEXTURE3D;
                case HLSL_SAMPLER_DIM_CUBE:
                    return D3D_SVT_TEXTURECUBE;
                case HLSL_SAMPLER_DIM_GENERIC:
                    return D3D_SVT_TEXTURE;
                default:
                    assert(0);
            }
            break;
        case HLSL_TYPE_UINT:
            return D3D_SVT_UINT;
        case HLSL_TYPE_VERTEXSHADER:
            return D3D_SVT_VERTEXSHADER;
        case HLSL_TYPE_VOID:
            return D3D_SVT_VOID;
        default:
            assert(0);
    }
    assert(0);
    return 0;
}

static void write_sm4_type(struct hlsl_ctx *ctx, struct vkd3d_bytecode_buffer *buffer, struct hlsl_type *type)
{
    const struct hlsl_type *array_type = get_array_type(type);
    const char *name = array_type->name ? array_type->name : "<unnamed>";
    const struct hlsl_profile_info *profile = ctx->profile;
    unsigned int field_count = 0, array_size = 0;
    size_t fields_offset = 0, name_offset = 0;
    struct hlsl_struct_field *field;

    if (type->bytecode_offset)
        return;

    if (profile->major_version >= 5)
        name_offset = put_string(buffer, name);

    if (type->type == HLSL_CLASS_ARRAY)
        array_size = get_array_size(type);

    if (array_type->type == HLSL_CLASS_STRUCT)
    {
        LIST_FOR_EACH_ENTRY(field, array_type->e.elements, struct hlsl_struct_field, entry)
        {
            field->name_bytecode_offset = put_string(buffer, field->name);
            write_sm4_type(ctx, buffer, field->type);
        }

        fields_offset = bytecode_get_size(buffer);

        LIST_FOR_EACH_ENTRY(field, array_type->e.elements, struct hlsl_struct_field, entry)
        {
            put_u32(buffer, field->name_bytecode_offset);
            put_u32(buffer, field->type->bytecode_offset);
            put_u32(buffer, field->reg_offset);
            ++field_count;
        }
    }

    type->bytecode_offset = put_u32(buffer, vkd3d_make_u32(sm4_class(type), sm4_base_type(type)));
    put_u32(buffer, vkd3d_make_u32(type->dimy, type->dimx));
    put_u32(buffer, vkd3d_make_u32(array_size, field_count));
    put_u32(buffer, fields_offset);

    if (profile->major_version >= 5)
    {
        put_u32(buffer, 0); /* FIXME: unknown */
        put_u32(buffer, 0); /* FIXME: unknown */
        put_u32(buffer, 0); /* FIXME: unknown */
        put_u32(buffer, 0); /* FIXME: unknown */
        put_u32(buffer, name_offset);
    }
}

static void write_sm4_rdef(struct hlsl_ctx *ctx, struct dxbc_writer *dxbc)
{
    size_t cbuffers_offset, resources_offset, creator_offset, string_offset;
    size_t cbuffer_position, resource_position, creator_position;
    const struct hlsl_profile_info *profile = ctx->profile;
    struct vkd3d_bytecode_buffer buffer = {0};
    unsigned int cbuffer_count = 0, i, j;
    const struct hlsl_buffer *cbuffer;

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
    put_u32(&buffer, vkd3d_make_u32(vkd3d_make_u16(profile->minor_version, profile->major_version),
            target_types[profile->type]));
    put_u32(&buffer, 0); /* FIXME: compilation flags */
    creator_position = put_u32(&buffer, 0);

    if (profile->major_version >= 5)
    {
        put_u32(&buffer, TAG_RD11);
        put_u32(&buffer, 15 * sizeof(uint32_t)); /* size of RDEF header including this header */
        put_u32(&buffer, 6 * sizeof(uint32_t)); /* size of buffer desc */
        put_u32(&buffer, 8 * sizeof(uint32_t)); /* size of binding desc */
        put_u32(&buffer, 10 * sizeof(uint32_t)); /* size of variable desc */
        put_u32(&buffer, 9 * sizeof(uint32_t)); /* size of type desc */
        put_u32(&buffer, 3 * sizeof(uint32_t)); /* size of member desc */
        put_u32(&buffer, 0); /* unknown; possibly a null terminator */
    }

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
                put_u32(&buffer, 0); /* type */
                put_u32(&buffer, 0); /* FIXME: default value */

                if (profile->major_version >= 5)
                {
                    put_u32(&buffer, 0); /* texture start */
                    put_u32(&buffer, 0); /* texture count */
                    put_u32(&buffer, 0); /* sampler start */
                    put_u32(&buffer, 0); /* sampler count */
                }
            }
        }

        j = 0;
        LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
        {
            if (var->is_uniform && var->buffer == cbuffer)
            {
                const unsigned int var_size = (profile->major_version >= 5 ? 10 : 6);
                size_t var_offset = vars_start + j * var_size * sizeof(uint32_t);
                size_t string_offset = put_string(&buffer, var->name);

                set_u32(&buffer, var_offset, string_offset);
                write_sm4_type(ctx, &buffer, var->data_type);
                set_u32(&buffer, var_offset + 4 * sizeof(uint32_t), var->data_type->bytecode_offset);
                ++j;
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

    put_u32(&buffer, vkd3d_make_u32((profile->major_version << 4) | profile->minor_version, shader_types[profile->type]));
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
