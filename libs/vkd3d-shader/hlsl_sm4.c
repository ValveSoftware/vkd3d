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

bool hlsl_sm4_register_from_semantic(struct hlsl_ctx *ctx, const struct hlsl_semantic *semantic,
        bool output, enum vkd3d_sm4_register_type *type, bool *has_idx)
{
    unsigned int i;

    static const struct
    {
        const char *semantic;
        bool output;
        enum vkd3d_shader_type shader_type;
        enum vkd3d_sm4_register_type type;
        bool has_idx;
    }
    register_table[] =
    {
        {"sv_primitiveid",  false, VKD3D_SHADER_TYPE_GEOMETRY, VKD3D_SM4_RT_PRIMID, false},

        /* Put sv_target in this table, instead of letting it fall through to
         * default varying allocation, so that the register index matches the
         * usage index. */
        {"color",           true, VKD3D_SHADER_TYPE_PIXEL, VKD3D_SM4_RT_OUTPUT,     true},
        {"depth",           true, VKD3D_SHADER_TYPE_PIXEL, VKD3D_SM4_RT_DEPTHOUT,   false},
        {"sv_depth",        true, VKD3D_SHADER_TYPE_PIXEL, VKD3D_SM4_RT_DEPTHOUT,   false},
        {"sv_target",       true, VKD3D_SHADER_TYPE_PIXEL, VKD3D_SM4_RT_OUTPUT,     true},
    };

    for (i = 0; i < ARRAY_SIZE(register_table); ++i)
    {
        if (!ascii_strcasecmp(semantic->name, register_table[i].semantic)
                && output == register_table[i].output
                && ctx->profile->type == register_table[i].shader_type)
        {
            *type = register_table[i].type;
            *has_idx = register_table[i].has_idx;
            return true;
        }
    }

    return false;
}

bool hlsl_sm4_usage_from_semantic(struct hlsl_ctx *ctx, const struct hlsl_semantic *semantic,
        bool output, D3D_NAME *usage)
{
    unsigned int i;

    static const struct
    {
        const char *name;
        bool output;
        enum vkd3d_shader_type shader_type;
        D3DDECLUSAGE usage;
    }
    semantics[] =
    {
        {"position",                    false, VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_POSITION},
        {"sv_position",                 false, VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_POSITION},
        {"sv_primitiveid",              false, VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_PRIMITIVE_ID},

        {"position",                    true,  VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_POSITION},
        {"sv_position",                 true,  VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_POSITION},
        {"sv_primitiveid",              true,  VKD3D_SHADER_TYPE_GEOMETRY,  D3D_NAME_PRIMITIVE_ID},

        {"position",                    false, VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_POSITION},
        {"sv_position",                 false, VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_POSITION},

        {"color",                       true,  VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_TARGET},
        {"depth",                       true,  VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_DEPTH},
        {"sv_target",                   true,  VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_TARGET},
        {"sv_depth",                    true,  VKD3D_SHADER_TYPE_PIXEL,     D3D_NAME_DEPTH},

        {"sv_position",                 false, VKD3D_SHADER_TYPE_VERTEX,    D3D_NAME_UNDEFINED},

        {"position",                    true,  VKD3D_SHADER_TYPE_VERTEX,    D3D_NAME_POSITION},
        {"sv_position",                 true,  VKD3D_SHADER_TYPE_VERTEX,    D3D_NAME_POSITION},
    };

    for (i = 0; i < ARRAY_SIZE(semantics); ++i)
    {
        if (!ascii_strcasecmp(semantic->name, semantics[i].name)
                && output == semantics[i].output
                && ctx->profile->type == semantics[i].shader_type
                && !ascii_strncasecmp(semantic->name, "sv_", 3))
        {
            *usage = semantics[i].usage;
            return true;
        }
    }

    if (!ascii_strncasecmp(semantic->name, "sv_", 3))
        return false;

    *usage = D3D_NAME_UNDEFINED;
    return true;
}

static void write_sm4_signature(struct hlsl_ctx *ctx, struct dxbc_writer *dxbc, bool output)
{
    struct vkd3d_bytecode_buffer buffer = {0};
    struct vkd3d_string_buffer *string;
    const struct hlsl_ir_var *var;
    size_t count_position;
    unsigned int i;
    bool ret;

    count_position = put_u32(&buffer, 0);
    put_u32(&buffer, 8); /* unknown */

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        unsigned int width = (1u << var->data_type->dimx) - 1, use_mask;
        enum vkd3d_sm4_register_type type;
        uint32_t usage_idx, reg_idx;
        D3D_NAME usage;
        bool has_idx;

        if ((output && !var->is_output_semantic) || (!output && !var->is_input_semantic))
            continue;

        ret = hlsl_sm4_usage_from_semantic(ctx, &var->semantic, output, &usage);
        assert(ret);
        usage_idx = var->semantic.index;

        if (hlsl_sm4_register_from_semantic(ctx, &var->semantic, output, &type, &has_idx))
        {
            reg_idx = has_idx ? var->semantic.index : ~0u;
        }
        else
        {
            assert(var->reg.allocated);
            type = VKD3D_SM4_RT_INPUT;
            reg_idx = var->reg.id;
        }

        use_mask = width; /* FIXME: accurately report use mask */
        if (output)
            use_mask = 0xf ^ use_mask;

        /* Special pixel shader semantics (TARGET, DEPTH, COVERAGE). */
        if (usage >= 64)
            usage = 0;

        put_u32(&buffer, 0); /* name */
        put_u32(&buffer, usage_idx);
        put_u32(&buffer, usage);
        switch (var->data_type->base_type)
        {
            case HLSL_TYPE_FLOAT:
            case HLSL_TYPE_HALF:
                put_u32(&buffer, D3D_REGISTER_COMPONENT_FLOAT32);
                break;

            case HLSL_TYPE_INT:
                put_u32(&buffer, D3D_REGISTER_COMPONENT_SINT32);
                break;

            case HLSL_TYPE_BOOL:
            case HLSL_TYPE_UINT:
                put_u32(&buffer, D3D_REGISTER_COMPONENT_UINT32);
                break;

            default:
                if ((string = hlsl_type_to_string(ctx, var->data_type)))
                    hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                            "Invalid data type %s for semantic variable %s.", string->buffer, var->name);
                hlsl_release_string_buffer(ctx, string);
                put_u32(&buffer, D3D_REGISTER_COMPONENT_UNKNOWN);
        }
        put_u32(&buffer, reg_idx);
        put_u32(&buffer, vkd3d_make_u16(width, use_mask));
    }

    i = 0;
    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        const char *semantic = var->semantic.name;
        size_t string_offset;
        D3D_NAME usage;

        if ((output && !var->is_output_semantic) || (!output && !var->is_input_semantic))
            continue;

        hlsl_sm4_usage_from_semantic(ctx, &var->semantic, output, &usage);

        if (usage == D3D_NAME_TARGET && !ascii_strcasecmp(semantic, "color"))
            string_offset = put_string(&buffer, "SV_Target");
        else if (usage == D3D_NAME_DEPTH && !ascii_strcasecmp(semantic, "depth"))
            string_offset = put_string(&buffer, "SV_Depth");
        else if (usage == D3D_NAME_POSITION && !ascii_strcasecmp(semantic, "position"))
            string_offset = put_string(&buffer, "SV_Position");
        else
            string_offset = put_string(&buffer, semantic);
        set_u32(&buffer, (2 + i++ * 6) * sizeof(uint32_t), string_offset);
    }

    set_u32(&buffer, count_position, i);

    dxbc_writer_add_section(dxbc, output ? TAG_OSGN : TAG_ISGN, buffer.data, buffer.size);
}

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
                put_u32(&buffer, var->buffer_offset * sizeof(float));
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

struct sm4_register
{
    enum vkd3d_sm4_register_type type;
    uint32_t idx[2];
    unsigned int idx_count;
    enum vkd3d_sm4_dimension dim;
    uint32_t immconst_uint[4];
    unsigned int mod;
};

struct sm4_instruction
{
    enum vkd3d_sm4_opcode opcode;

    struct
    {
        struct sm4_register reg;
        unsigned int writemask;
    } dst;

    struct
    {
        struct sm4_register reg;
        unsigned int swizzle;
    } srcs[2];
    unsigned int src_count;

    uint32_t idx[2];
    unsigned int idx_count;

    unsigned int has_dst;
};

static unsigned int sm4_swizzle_type(enum vkd3d_sm4_register_type type)
{
    switch (type)
    {
        case VKD3D_SM4_RT_IMMCONST:
            return VKD3D_SM4_SWIZZLE_NONE;

        case VKD3D_SM4_RT_CONSTBUFFER:
        case VKD3D_SM4_RT_INPUT:
        case VKD3D_SM4_RT_TEMP:
            return VKD3D_SM4_SWIZZLE_VEC4;

        default:
            FIXME("Unhandled register type %#x.\n", type);
            return VKD3D_SM4_SWIZZLE_VEC4;
    }
}

static void sm4_register_from_deref(struct hlsl_ctx *ctx, struct sm4_register *reg,
        unsigned int *writemask, const struct hlsl_deref *deref, const struct hlsl_type *data_type)
{
    const struct hlsl_ir_var *var = deref->var;

    if (var->is_uniform)
    {
        reg->type = VKD3D_SM4_RT_CONSTBUFFER;
        reg->dim = VKD3D_SM4_DIMENSION_VEC4;
        reg->idx[0] = var->buffer->reg.id;
        reg->idx[1] = var->buffer_offset / 4;
        reg->idx_count = 2;
        *writemask = ((1u << data_type->dimx) - 1) << (var->buffer_offset & 3);
    }
    else if (var->is_input_semantic)
    {
        bool has_idx;

        if (hlsl_sm4_register_from_semantic(ctx, &var->semantic, false, &reg->type, &has_idx))
        {
            if (has_idx)
            {
                reg->idx[0] = var->semantic.index;
                reg->idx_count = 1;
            }

            reg->dim = VKD3D_SM4_DIMENSION_VEC4;
            *writemask = (1u << data_type->dimx) - 1;
        }
        else
        {
            struct hlsl_reg hlsl_reg = hlsl_reg_from_deref(deref, data_type);

            assert(hlsl_reg.allocated);
            reg->type = VKD3D_SM4_RT_INPUT;
            reg->dim = VKD3D_SM4_DIMENSION_VEC4;
            reg->idx[0] = hlsl_reg.id;
            reg->idx_count = 1;
            *writemask = hlsl_reg.writemask;
        }
    }
    else if (var->is_output_semantic)
    {
        bool has_idx;

        if (hlsl_sm4_register_from_semantic(ctx, &var->semantic, true, &reg->type, &has_idx))
        {
            if (has_idx)
            {
                reg->idx[0] = var->semantic.index;
                reg->idx_count = 1;
            }

            if (reg->type == VKD3D_SM4_RT_DEPTHOUT)
                reg->dim = VKD3D_SM4_DIMENSION_SCALAR;
            else
                reg->dim = VKD3D_SM4_DIMENSION_VEC4;
            *writemask = (1u << data_type->dimx) - 1;
        }
        else
        {
            struct hlsl_reg hlsl_reg = hlsl_reg_from_deref(deref, data_type);

            assert(hlsl_reg.allocated);
            reg->type = VKD3D_SM4_RT_OUTPUT;
            reg->dim = VKD3D_SM4_DIMENSION_VEC4;
            reg->idx[0] = hlsl_reg.id;
            reg->idx_count = 1;
            *writemask = hlsl_reg.writemask;
        }
    }
    else
    {
        struct hlsl_reg hlsl_reg = hlsl_reg_from_deref(deref, data_type);

        assert(hlsl_reg.allocated);
        reg->type = VKD3D_SM4_RT_TEMP;
        reg->dim = VKD3D_SM4_DIMENSION_VEC4;
        reg->idx[0] = hlsl_reg.id;
        reg->idx_count = 1;
        *writemask = hlsl_reg.writemask;
    }
}

static void sm4_register_from_node(struct sm4_register *reg, unsigned int *writemask, const struct hlsl_ir_node *instr)
{
    assert(instr->reg.allocated);
    reg->type = VKD3D_SM4_RT_TEMP;
    reg->dim = VKD3D_SM4_DIMENSION_VEC4;
    reg->idx[0] = instr->reg.id;
    reg->idx_count = 1;
    *writemask = instr->reg.writemask;
}

static uint32_t sm4_encode_register(const struct sm4_register *reg)
{
    return (reg->type << VKD3D_SM4_REGISTER_TYPE_SHIFT)
            | (reg->idx_count << VKD3D_SM4_REGISTER_ORDER_SHIFT)
            | (reg->dim << VKD3D_SM4_DIMENSION_SHIFT);
}

static uint32_t sm4_register_order(const struct sm4_register *reg)
{
    uint32_t order = 1;
    if (reg->type == VKD3D_SM4_RT_IMMCONST)
        order += reg->dim == VKD3D_SM4_DIMENSION_VEC4 ? 4 : 1;
    order += reg->idx_count;
    if (reg->mod)
        ++order;
    return order;
}

static void write_sm4_instruction(struct vkd3d_bytecode_buffer *buffer, const struct sm4_instruction *instr)
{
    uint32_t token = instr->opcode;
    unsigned int size = 1, i, j;

    if (instr->has_dst)
        size += sm4_register_order(&instr->dst.reg);
    for (i = 0; i < instr->src_count; ++i)
        size += sm4_register_order(&instr->srcs[i].reg);
    size += instr->idx_count;

    token |= (size << VKD3D_SM4_INSTRUCTION_LENGTH_SHIFT);
    put_u32(buffer, token);

    if (instr->has_dst)
    {
        token = sm4_encode_register(&instr->dst.reg);
        if (instr->dst.reg.dim == VKD3D_SM4_DIMENSION_VEC4)
            token |= instr->dst.writemask << VKD3D_SM4_WRITEMASK_SHIFT;
        put_u32(buffer, token);

        for (j = 0; j < instr->dst.reg.idx_count; ++j)
            put_u32(buffer, instr->dst.reg.idx[j]);
    }

    for (i = 0; i < instr->src_count; ++i)
    {
        token = sm4_encode_register(&instr->srcs[i].reg);
        token |= sm4_swizzle_type(instr->srcs[i].reg.type) << VKD3D_SM4_SWIZZLE_TYPE_SHIFT;
        token |= instr->srcs[i].swizzle << VKD3D_SM4_SWIZZLE_SHIFT;
        if (instr->srcs[i].reg.mod)
            token |= VKD3D_SM4_REGISTER_MODIFIER;
        put_u32(buffer, token);

        if (instr->srcs[i].reg.mod)
            put_u32(buffer, instr->srcs[i].reg.mod);

        for (j = 0; j < instr->srcs[i].reg.idx_count; ++j)
            put_u32(buffer, instr->srcs[i].reg.idx[j]);

        if (instr->srcs[i].reg.type == VKD3D_SM4_RT_IMMCONST)
        {
            put_u32(buffer, instr->srcs[i].reg.immconst_uint[0]);
            if (instr->srcs[i].reg.dim == VKD3D_SM4_DIMENSION_VEC4)
            {
                put_u32(buffer, instr->srcs[i].reg.immconst_uint[1]);
                put_u32(buffer, instr->srcs[i].reg.immconst_uint[2]);
                put_u32(buffer, instr->srcs[i].reg.immconst_uint[3]);
            }
        }
    }

    for (j = 0; j < instr->idx_count; ++j)
        put_u32(buffer, instr->idx[j]);
}

static void write_sm4_dcl_constant_buffer(struct vkd3d_bytecode_buffer *buffer, const struct hlsl_buffer *cbuffer)
{
    const struct sm4_instruction instr =
    {
        .opcode = VKD3D_SM4_OP_DCL_CONSTANT_BUFFER,

        .srcs[0].reg.dim = VKD3D_SM4_DIMENSION_VEC4,
        .srcs[0].reg.type = VKD3D_SM4_RT_CONSTBUFFER,
        .srcs[0].reg.idx = {cbuffer->reg.id, (cbuffer->used_size + 3) / 4},
        .srcs[0].reg.idx_count = 2,
        .srcs[0].swizzle = HLSL_SWIZZLE(X, Y, Z, W),
        .src_count = 1,
    };
    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_dcl_semantic(struct hlsl_ctx *ctx, struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_var *var)
{
    const struct hlsl_profile_info *profile = ctx->profile;
    const bool output = var->is_output_semantic;
    D3D_NAME usage;
    bool has_idx;

    struct sm4_instruction instr =
    {
        .dst.reg.dim = VKD3D_SM4_DIMENSION_VEC4,
        .has_dst = 1,
    };

    if (hlsl_sm4_register_from_semantic(ctx, &var->semantic, output, &instr.dst.reg.type, &has_idx))
    {
        if (has_idx)
        {
            instr.dst.reg.idx[0] = var->semantic.index;
            instr.dst.reg.idx_count = 1;
        }
        else
        {
            instr.dst.reg.idx_count = 0;
        }
        instr.dst.writemask = (1 << var->data_type->dimx) - 1;
    }
    else
    {
        instr.dst.reg.type = output ? VKD3D_SM4_RT_OUTPUT : VKD3D_SM4_RT_INPUT;
        instr.dst.reg.idx[0] = var->reg.id;
        instr.dst.reg.idx_count = 1;
        instr.dst.writemask = var->reg.writemask;
    }

    if (instr.dst.reg.type == VKD3D_SM4_RT_DEPTHOUT)
        instr.dst.reg.dim = VKD3D_SM4_DIMENSION_SCALAR;

    hlsl_sm4_usage_from_semantic(ctx, &var->semantic, output, &usage);

    if (var->is_input_semantic)
    {
        switch (usage)
        {
            case D3D_NAME_UNDEFINED:
                instr.opcode = (profile->type == VKD3D_SHADER_TYPE_PIXEL)
                        ? VKD3D_SM4_OP_DCL_INPUT_PS : VKD3D_SM4_OP_DCL_INPUT;
                break;

            case D3D_NAME_INSTANCE_ID:
            case D3D_NAME_PRIMITIVE_ID:
            case D3D_NAME_VERTEX_ID:
                instr.opcode = (profile->type == VKD3D_SHADER_TYPE_PIXEL)
                        ? VKD3D_SM4_OP_DCL_INPUT_PS_SGV : VKD3D_SM4_OP_DCL_INPUT_SGV;
                break;

            default:
                instr.opcode = (profile->type == VKD3D_SHADER_TYPE_PIXEL)
                        ? VKD3D_SM4_OP_DCL_INPUT_PS_SIV : VKD3D_SM4_OP_DCL_INPUT_SIV;
                break;
        }

        if (profile->type == VKD3D_SHADER_TYPE_PIXEL)
            instr.opcode |= VKD3DSIM_LINEAR << VKD3D_SM4_INTERPOLATION_MODE_SHIFT;
    }
    else
    {
        if (usage == D3D_NAME_UNDEFINED || profile->type == VKD3D_SHADER_TYPE_PIXEL)
            instr.opcode = VKD3D_SM4_OP_DCL_OUTPUT;
        else
            instr.opcode = VKD3D_SM4_OP_DCL_OUTPUT_SIV;
    }

    switch (usage)
    {
        case D3D_NAME_COVERAGE:
        case D3D_NAME_DEPTH:
        case D3D_NAME_DEPTH_GREATER_EQUAL:
        case D3D_NAME_DEPTH_LESS_EQUAL:
        case D3D_NAME_TARGET:
        case D3D_NAME_UNDEFINED:
            break;

        default:
            instr.idx_count = 1;
            instr.idx[0] = usage;
            break;
    }

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_dcl_temps(struct vkd3d_bytecode_buffer *buffer, uint32_t temp_count)
{
    struct sm4_instruction instr =
    {
        .opcode = VKD3D_SM4_OP_DCL_TEMPS,

        .idx = {temp_count},
        .idx_count = 1,
    };

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_ret(struct vkd3d_bytecode_buffer *buffer)
{
    struct sm4_instruction instr =
    {
        .opcode = VKD3D_SM4_OP_RET,
    };

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_unary_op(struct vkd3d_bytecode_buffer *buffer, enum vkd3d_sm4_opcode opcode,
        const struct hlsl_ir_node *dst, const struct hlsl_ir_node *src, unsigned int src_mod)
{
    struct sm4_instruction instr;
    unsigned int writemask;

    memset(&instr, 0, sizeof(instr));
    instr.opcode = opcode;

    sm4_register_from_node(&instr.dst.reg, &instr.dst.writemask, dst);
    instr.has_dst = 1;

    sm4_register_from_node(&instr.srcs[0].reg, &writemask, src);
    instr.srcs[0].swizzle = hlsl_map_swizzle(hlsl_swizzle_from_writemask(writemask), instr.dst.writemask);
    instr.srcs[0].reg.mod = src_mod;
    instr.src_count = 1;

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_binary_op(struct vkd3d_bytecode_buffer *buffer, enum vkd3d_sm4_opcode opcode,
        const struct hlsl_ir_node *dst, const struct hlsl_ir_node *src1, const struct hlsl_ir_node *src2)
{
    struct sm4_instruction instr;
    unsigned int writemask;

    memset(&instr, 0, sizeof(instr));
    instr.opcode = opcode;

    sm4_register_from_node(&instr.dst.reg, &instr.dst.writemask, dst);
    instr.has_dst = 1;

    sm4_register_from_node(&instr.srcs[0].reg, &writemask, src1);
    instr.srcs[0].swizzle = hlsl_map_swizzle(hlsl_swizzle_from_writemask(writemask), instr.dst.writemask);
    sm4_register_from_node(&instr.srcs[1].reg, &writemask, src2);
    instr.srcs[1].swizzle = hlsl_map_swizzle(hlsl_swizzle_from_writemask(writemask), instr.dst.writemask);
    instr.src_count = 2;

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_constant(struct hlsl_ctx *ctx,
        struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_constant *constant)
{
    const unsigned int dimx = constant->node.data_type->dimx;
    struct sm4_instruction instr;
    unsigned int i;

    memset(&instr, 0, sizeof(instr));
    instr.opcode = VKD3D_SM4_OP_MOV;

    sm4_register_from_node(&instr.dst.reg, &instr.dst.writemask, &constant->node);
    instr.has_dst = 1;

    instr.srcs[0].reg.dim = (dimx > 1) ? VKD3D_SM4_DIMENSION_VEC4 : VKD3D_SM4_DIMENSION_SCALAR;
    instr.srcs[0].reg.type = VKD3D_SM4_RT_IMMCONST;
    instr.srcs[0].reg.idx[0] = constant->reg.id;
    instr.srcs[0].reg.idx_count = 1;
    for (i = 0; i < dimx; ++i)
        instr.srcs[0].reg.immconst_uint[i] = constant->value.u[i];
    instr.src_count = 1,

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_expr(struct hlsl_ctx *ctx,
        struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_expr *expr)
{
    const struct hlsl_ir_node *arg1 = expr->operands[0].node;
    const struct hlsl_ir_node *arg2 = expr->operands[1].node;

    assert(expr->node.reg.allocated);

    switch (expr->node.data_type->base_type)
    {
        case HLSL_TYPE_FLOAT:
        {
            switch (expr->op)
            {
                case HLSL_OP1_CAST:
                {
                    const struct hlsl_type *src_type = arg1->data_type;

                    /* Narrowing casts need to be lowered. */
                    if (src_type->dimx != expr->node.data_type->dimx)
                        hlsl_fixme(ctx, expr->node.loc, "Narrowing cast.\n");

                    switch (src_type->base_type)
                    {
                        case HLSL_TYPE_HALF:
                        case HLSL_TYPE_FLOAT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_MOV, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_INT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_ITOF, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_UINT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_UTOF, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_BOOL:
                            hlsl_fixme(ctx, expr->node.loc, "Casts from bool to float are not implemented.\n");
                            break;

                        case HLSL_TYPE_DOUBLE:
                            hlsl_fixme(ctx, expr->node.loc, "Casts from double to float are not implemented.\n");
                            break;

                        default:
                            break;
                    }
                    break;
                }

                case HLSL_OP1_NEG:
                    write_sm4_unary_op(buffer, VKD3D_SM4_OP_MOV, &expr->node, arg1, VKD3D_SM4_REGISTER_MODIFIER_NEGATE);
                    break;

                case HLSL_OP2_ADD:
                    write_sm4_binary_op(buffer, VKD3D_SM4_OP_ADD, &expr->node, arg1, arg2);
                    break;

                case HLSL_OP2_DIV:
                    write_sm4_binary_op(buffer, VKD3D_SM4_OP_DIV, &expr->node, arg1, arg2);
                    break;

                case HLSL_OP2_MAX:
                    write_sm4_binary_op(buffer, VKD3D_SM4_OP_MAX, &expr->node, arg1, arg2);
                    break;

                case HLSL_OP2_MIN:
                    write_sm4_binary_op(buffer, VKD3D_SM4_OP_MIN, &expr->node, arg1, arg2);
                    break;

                case HLSL_OP2_MUL:
                    write_sm4_binary_op(buffer, VKD3D_SM4_OP_MUL, &expr->node, arg1, arg2);
                    break;

                default:
                    hlsl_fixme(ctx, expr->node.loc, "SM4 float \"%s\" expression.", debug_hlsl_expr_op(expr->op));
                    break;
            }
            break;
        }

        case HLSL_TYPE_INT:
        {
            switch (expr->op)
            {
                case HLSL_OP1_CAST:
                {
                    const struct hlsl_type *src_type = arg1->data_type;

                    /* Narrowing casts need to be lowered. */
                    if (src_type->dimx != expr->node.data_type->dimx)
                        hlsl_fixme(ctx, expr->node.loc, "Narrowing cast.");

                    switch (src_type->base_type)
                    {
                        case HLSL_TYPE_HALF:
                        case HLSL_TYPE_FLOAT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_FTOI, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_INT:
                        case HLSL_TYPE_UINT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_MOV, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_BOOL:
                            hlsl_fixme(ctx, expr->node.loc, "SM4 cast from bool to int.");
                            break;

                        case HLSL_TYPE_DOUBLE:
                            hlsl_fixme(ctx, expr->node.loc, "SM4 cast from double to int.");
                            break;

                        default:
                            break;
                    }
                    break;
                }

                default:
                    hlsl_fixme(ctx, expr->node.loc, "SM4 int \"%s\" expression.", debug_hlsl_expr_op(expr->op));
                    break;
            }
            break;
        }

        case HLSL_TYPE_UINT:
        {
            switch (expr->op)
            {
                case HLSL_OP1_CAST:
                {
                    const struct hlsl_type *src_type = arg1->data_type;

                    /* Narrowing casts need to be lowered. */
                    if (src_type->dimx != expr->node.data_type->dimx)
                        hlsl_fixme(ctx, expr->node.loc, "SM4 narrowing cast.\n");

                    switch (src_type->base_type)
                    {
                        case HLSL_TYPE_HALF:
                        case HLSL_TYPE_FLOAT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_FTOU, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_INT:
                        case HLSL_TYPE_UINT:
                            write_sm4_unary_op(buffer, VKD3D_SM4_OP_MOV, &expr->node, arg1, 0);
                            break;

                        case HLSL_TYPE_BOOL:
                            hlsl_fixme(ctx, expr->node.loc, "SM4 cast from bool to uint.\n");
                            break;

                        case HLSL_TYPE_DOUBLE:
                            hlsl_fixme(ctx, expr->node.loc, "SM4 cast from double to uint.\n");
                            break;

                        default:
                            break;
                    }
                    break;
                }

                default:
                    hlsl_fixme(ctx, expr->node.loc, "SM4 uint \"%s\" expression.\n", debug_hlsl_expr_op(expr->op));
                    break;
            }
            break;
        }

        default:
        {
            struct vkd3d_string_buffer *string;

            if ((string = hlsl_type_to_string(ctx, expr->node.data_type)))
                hlsl_fixme(ctx, expr->node.loc, "SM4 %s expression.", string->buffer);
            hlsl_release_string_buffer(ctx, string);
            break;
        }
    }
}

static void write_sm4_load(struct hlsl_ctx *ctx,
        struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_load *load)
{
    struct sm4_instruction instr;
    unsigned int writemask;

    memset(&instr, 0, sizeof(instr));
    instr.opcode = VKD3D_SM4_OP_MOV;

    sm4_register_from_node(&instr.dst.reg, &instr.dst.writemask, &load->node);
    instr.has_dst = 1;

    sm4_register_from_deref(ctx, &instr.srcs[0].reg, &writemask, &load->src, load->node.data_type);
    instr.srcs[0].swizzle = hlsl_map_swizzle(hlsl_swizzle_from_writemask(writemask), instr.dst.writemask);
    instr.src_count = 1;

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_store(struct hlsl_ctx *ctx,
        struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_store *store)
{
    const struct hlsl_ir_node *rhs = store->rhs.node;
    struct sm4_instruction instr;
    unsigned int writemask;

    if (store->lhs.var->data_type->type == HLSL_CLASS_MATRIX)
    {
        hlsl_fixme(ctx, store->node.loc, "Store to a matrix variable.\n");
        return;
    }

    memset(&instr, 0, sizeof(instr));
    instr.opcode = VKD3D_SM4_OP_MOV;

    sm4_register_from_deref(ctx, &instr.dst.reg, &writemask, &store->lhs, rhs->data_type);
    instr.dst.writemask = hlsl_combine_writemasks(writemask, store->writemask);
    instr.has_dst = 1;

    sm4_register_from_node(&instr.srcs[0].reg, &writemask, rhs);
    instr.srcs[0].swizzle = hlsl_map_swizzle(hlsl_swizzle_from_writemask(writemask), instr.dst.writemask);
    instr.src_count = 1;

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_swizzle(struct hlsl_ctx *ctx,
        struct vkd3d_bytecode_buffer *buffer, const struct hlsl_ir_swizzle *swizzle)
{
    struct sm4_instruction instr;
    unsigned int writemask;

    memset(&instr, 0, sizeof(instr));
    instr.opcode = VKD3D_SM4_OP_MOV;

    sm4_register_from_node(&instr.dst.reg, &instr.dst.writemask, &swizzle->node);
    instr.has_dst = 1;

    sm4_register_from_node(&instr.srcs[0].reg, &writemask, swizzle->val.node);
    instr.srcs[0].swizzle = hlsl_map_swizzle(hlsl_combine_swizzles(hlsl_swizzle_from_writemask(writemask),
            swizzle->swizzle, swizzle->node.data_type->dimx), instr.dst.writemask);
    instr.src_count = 1;

    write_sm4_instruction(buffer, &instr);
}

static void write_sm4_shdr(struct hlsl_ctx *ctx,
        const struct hlsl_ir_function_decl *entry_func, struct dxbc_writer *dxbc)
{
    const struct hlsl_profile_info *profile = ctx->profile;
    struct vkd3d_bytecode_buffer buffer = {0};
    const struct hlsl_buffer *cbuffer;
    const struct hlsl_ir_node *instr;
    const struct hlsl_ir_var *var;
    size_t token_count_position;

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
    token_count_position = put_u32(&buffer, 0);

    LIST_FOR_EACH_ENTRY(cbuffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        if (cbuffer->reg.allocated)
            write_sm4_dcl_constant_buffer(&buffer, cbuffer);
    }

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if ((var->is_input_semantic && var->last_read) || (var->is_output_semantic && var->first_write))
            write_sm4_dcl_semantic(ctx, &buffer, var);
    }

    if (ctx->temp_count)
        write_sm4_dcl_temps(&buffer, ctx->temp_count);

    LIST_FOR_EACH_ENTRY(instr, entry_func->body, struct hlsl_ir_node, entry)
    {
        if (instr->data_type)
        {
            if (instr->data_type->type == HLSL_CLASS_MATRIX)
            {
                FIXME("Matrix operations need to be lowered.\n");
                break;
            }

            assert(instr->data_type->type == HLSL_CLASS_SCALAR || instr->data_type->type == HLSL_CLASS_VECTOR);
        }

        switch (instr->type)
        {
            case HLSL_IR_CONSTANT:
                write_sm4_constant(ctx, &buffer, hlsl_ir_constant(instr));
                break;

            case HLSL_IR_EXPR:
                write_sm4_expr(ctx, &buffer, hlsl_ir_expr(instr));
                break;

            case HLSL_IR_LOAD:
                write_sm4_load(ctx, &buffer, hlsl_ir_load(instr));
                break;

            case HLSL_IR_STORE:
                write_sm4_store(ctx, &buffer, hlsl_ir_store(instr));
                break;

            case HLSL_IR_SWIZZLE:
                write_sm4_swizzle(ctx, &buffer, hlsl_ir_swizzle(instr));
                break;

            default:
                FIXME("Unhandled instruction type %s.\n", hlsl_node_type_to_string(instr->type));
        }
    }

    write_sm4_ret(&buffer);

    set_u32(&buffer, token_count_position, bytecode_get_size(&buffer) / sizeof(uint32_t));

    dxbc_writer_add_section(dxbc, TAG_SHDR, buffer.data, buffer.size);
}

int hlsl_sm4_write(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out)
{
    struct dxbc_writer dxbc;
    size_t i;
    int ret;

    dxbc_writer_init(&dxbc);

    write_sm4_signature(ctx, &dxbc, false);
    write_sm4_signature(ctx, &dxbc, true);
    write_sm4_rdef(ctx, &dxbc);
    write_sm4_shdr(ctx, entry_func, &dxbc);

    if (!(ret = ctx->result))
        ret = dxbc_writer_write(&dxbc, out);
    for (i = 0; i < dxbc.section_count; ++i)
        vkd3d_free((void *)dxbc.sections[i].data);
    return ret;
}
