/*
 * FX (Direct3D 9/10/11 effect) support
 *
 * Copyright 2023 Nikolay Sivov for CodeWeavers
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

struct fx_write_context
{
    struct vkd3d_bytecode_buffer unstructured;
    struct vkd3d_bytecode_buffer structured;

    uint32_t technique_count;
    int status;
};

static uint32_t fx_put_raw_string(struct fx_write_context *fx, const char *string)
{
    /* NULLs are emitted as empty strings using the same 4 bytes at the start of the section. */
    return string ? put_string(&fx->unstructured, string) : 0;
}

static void write_technique(struct hlsl_ir_var *var, struct fx_write_context *fx)
{
    struct vkd3d_bytecode_buffer *buffer = &fx->structured;
    uint32_t name_offset;

    name_offset = fx_put_raw_string(fx, var->name);
    put_u32(buffer, name_offset);
    put_u32(buffer, 0); /* Pass count. */
    put_u32(buffer, 0); /* Annotation count. */

    /* TODO: passes */
}

static void set_status(struct fx_write_context *fx, int status)
{
    if (fx->status < 0)
        return;
    if (status < 0)
        fx->status = status;
}

static void write_techniques(struct hlsl_scope *scope, struct fx_write_context *fx)
{
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
    {
        const struct hlsl_type *type = var->data_type;

        if (type->base_type == HLSL_TYPE_TECHNIQUE && type->e.version == 10)
        {
            write_technique(var, fx);
            ++fx->technique_count;
        }
    }

    set_status(fx, fx->unstructured.status);
    set_status(fx, fx->structured.status);
}

static int hlsl_fx_4_write(struct hlsl_ctx *ctx, struct vkd3d_shader_code *out)
{
    struct vkd3d_bytecode_buffer buffer = { 0 };
    struct fx_write_context fx;
    uint32_t size_offset, size;

    memset(&fx, 0, sizeof(fx));

    put_u32(&fx.unstructured, 0); /* Empty string placeholder. */

    /* TODO: buffers */
    /* TODO: objects */
    /* TODO: shared buffers */
    /* TODO: shared objects */

    write_techniques(ctx->globals, &fx);

    put_u32(&buffer, ctx->profile->minor_version == 0 ? 0xfeff1001 : 0xfeff1011); /* Version. */
    put_u32(&buffer, 0); /* Buffer count. */
    put_u32(&buffer, 0); /* Variable count. */
    put_u32(&buffer, 0); /* Object count. */
    put_u32(&buffer, 0); /* Pool buffer count. */
    put_u32(&buffer, 0); /* Pool variable count. */
    put_u32(&buffer, 0); /* Pool object count. */
    put_u32(&buffer, fx.technique_count);
    size_offset = put_u32(&buffer, 0); /* Unstructured size. */
    put_u32(&buffer, 0); /* String count. */
    put_u32(&buffer, 0); /* Texture object count. */
    put_u32(&buffer, 0); /* Depth stencil state count. */
    put_u32(&buffer, 0); /* Blend state count. */
    put_u32(&buffer, 0); /* Rasterizer state count. */
    put_u32(&buffer, 0); /* Sampler state count. */
    put_u32(&buffer, 0); /* Rendertarget view count. */
    put_u32(&buffer, 0); /* Depth stencil view count. */
    put_u32(&buffer, 0); /* Shader count. */
    put_u32(&buffer, 0); /* Inline shader count. */

    size = align(fx.unstructured.size, 4);
    set_u32(&buffer, size_offset, size);

    bytecode_put_bytes(&buffer, fx.unstructured.data, fx.unstructured.size);
    bytecode_put_bytes(&buffer, fx.structured.data, fx.structured.size);

    vkd3d_free(fx.unstructured.data);
    vkd3d_free(fx.structured.data);

    set_status(&fx, buffer.status);

    if (!fx.status)
    {
        out->code = buffer.data;
        out->size = buffer.size;
    }

    if (fx.status < 0)
        ctx->result = fx.status;

    return fx.status;
}

int hlsl_emit_effect_binary(struct hlsl_ctx *ctx, struct vkd3d_shader_code *out)
{
    if (ctx->profile->major_version == 2)
    {
        hlsl_fixme(ctx, &ctx->location, "Writing fx_2_0 binaries is not implemented.");
        return VKD3D_ERROR_NOT_IMPLEMENTED;
    }
    else if (ctx->profile->major_version == 4)
    {
        return hlsl_fx_4_write(ctx, out);
    }
    else if (ctx->profile->major_version == 5)
    {
        hlsl_fixme(ctx, &ctx->location, "Writing fx_5_0 binaries is not implemented.");
        return VKD3D_ERROR_NOT_IMPLEMENTED;
    }
    else
    {
        vkd3d_unreachable();
    }
}
