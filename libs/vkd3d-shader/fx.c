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

struct string_entry
{
    struct rb_entry entry;
    /* String points to original data, should not be freed. */
    const char *string;
    uint32_t offset;
};

static int string_storage_compare(const void *key, const struct rb_entry *entry)
{
    struct string_entry *string_entry = RB_ENTRY_VALUE(entry, struct string_entry, entry);
    const char *string = key;

    return strcmp(string, string_entry->string);
}

static void string_storage_destroy(struct rb_entry *entry, void *context)
{
    struct string_entry *string_entry = RB_ENTRY_VALUE(entry, struct string_entry, entry);

    vkd3d_free(string_entry);
}

struct fx_write_context
{
    struct hlsl_ctx *ctx;

    struct vkd3d_bytecode_buffer unstructured;
    struct vkd3d_bytecode_buffer structured;

    struct rb_tree strings;

    unsigned int min_technique_version;
    unsigned int max_technique_version;

    uint32_t technique_count;
    uint32_t group_count;
    int status;
};

static void fx_write_context_init(struct hlsl_ctx *ctx, struct fx_write_context *fx)
{
    unsigned int version = ctx->profile->major_version;

    memset(fx, 0, sizeof(*fx));

    fx->ctx = ctx;
    if (version == 2)
    {
        fx->min_technique_version = 9;
        fx->max_technique_version = 9;
    }
    else if (version == 4)
    {
        fx->min_technique_version = 10;
        fx->max_technique_version = 10;
    }
    else if (version == 5)
    {
        fx->min_technique_version = 10;
        fx->max_technique_version = 11;
    }

    rb_init(&fx->strings, string_storage_compare);
}

static int fx_write_context_cleanup(struct fx_write_context *fx)
{
    int status = fx->status;
    rb_destroy(&fx->strings, string_storage_destroy, NULL);

    return status;
}

static bool technique_matches_version(const struct hlsl_ir_var *var, const struct fx_write_context *fx)
{
    const struct hlsl_type *type = var->data_type;

    if (type->base_type != HLSL_TYPE_TECHNIQUE)
        return false;

    return type->e.version >= fx->min_technique_version && type->e.version <= fx->max_technique_version;
}

static uint32_t fx_put_raw_string(struct fx_write_context *fx, const char *string)
{
    struct string_entry *string_entry;
    struct rb_entry *entry;

    /* NULLs are emitted as empty strings using the same 4 bytes at the start of the section. */
    if (!string)
        return 0;

    if ((entry = rb_get(&fx->strings, string)))
    {
        string_entry = RB_ENTRY_VALUE(entry, struct string_entry, entry);
        return string_entry->offset;
    }

    if (!(string_entry = hlsl_alloc(fx->ctx, sizeof(*string_entry))))
        return 0;

    string_entry->offset = put_string(&fx->unstructured, string);
    string_entry->string = string;

    rb_put(&fx->strings, string, &string_entry->entry);

    return string_entry->offset;
}

static void write_pass(struct hlsl_ir_var *var, struct fx_write_context *fx)
{
    struct vkd3d_bytecode_buffer *buffer = &fx->structured;
    uint32_t name_offset;

    name_offset = fx_put_raw_string(fx, var->name);
    put_u32(buffer, name_offset);
    put_u32(buffer, 0); /* Annotation count. */
    put_u32(buffer, 0); /* Assignment count. */
}

static void write_technique(struct hlsl_ir_var *var, struct fx_write_context *fx)
{
    struct vkd3d_bytecode_buffer *buffer = &fx->structured;
    uint32_t name_offset, count = 0;
    struct hlsl_ir_var *pass;
    uint32_t count_offset;

    name_offset = fx_put_raw_string(fx, var->name);
    put_u32(buffer, name_offset);
    count_offset = put_u32(buffer, 0);
    put_u32(buffer, 0); /* Annotation count. */

    LIST_FOR_EACH_ENTRY(pass, &var->scope->vars, struct hlsl_ir_var, scope_entry)
    {
        write_pass(pass, fx);
        ++count;
    }

    set_u32(buffer, count_offset, count);
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

static void write_group(struct hlsl_scope *scope, const char *name, struct fx_write_context *fx)
{
    struct vkd3d_bytecode_buffer *buffer = &fx->structured;
    uint32_t name_offset = fx_put_raw_string(fx, name);
    uint32_t count_offset, count;

    put_u32(buffer, name_offset);
    count_offset = put_u32(buffer, 0); /* Technique count */
    put_u32(buffer, 0); /* Annotation count */

    count = fx->technique_count;
    write_techniques(scope, fx);
    set_u32(buffer, count_offset, fx->technique_count - count);

    ++fx->group_count;
}

static void write_groups(struct hlsl_scope *scope, struct fx_write_context *fx)
{
    bool needs_default_group = false;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
    {
        if (technique_matches_version(var, fx))
        {
            needs_default_group = true;
            break;
        }
    }

    if (needs_default_group)
        write_group(scope, NULL, fx);
    LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
    {
        const struct hlsl_type *type = var->data_type;

        if (type->base_type == HLSL_TYPE_EFFECT_GROUP)
            write_group(var->scope, var->name, fx);
    }
}

static int hlsl_fx_4_write(struct hlsl_ctx *ctx, struct vkd3d_shader_code *out)
{
    struct vkd3d_bytecode_buffer buffer = { 0 };
    struct fx_write_context fx;
    uint32_t size_offset, size;

    fx_write_context_init(ctx, &fx);

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

    return fx_write_context_cleanup(&fx);
}

static int hlsl_fx_5_write(struct hlsl_ctx *ctx, struct vkd3d_shader_code *out)
{
    struct vkd3d_bytecode_buffer buffer = { 0 };
    struct fx_write_context fx;
    uint32_t size_offset, size;

    fx_write_context_init(ctx, &fx);

    put_u32(&fx.unstructured, 0); /* Empty string placeholder. */

    /* TODO: buffers */
    /* TODO: objects */
    /* TODO: interface variables */

    write_groups(ctx->globals, &fx);

    put_u32(&buffer, 0xfeff2001); /* Version. */
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
    put_u32(&buffer, fx.group_count); /* Group count. */
    put_u32(&buffer, 0); /* UAV count. */
    put_u32(&buffer, 0); /* Interface variables count. */
    put_u32(&buffer, 0); /* Interface variable element count. */
    put_u32(&buffer, 0); /* Class instance elements count. */

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

    return fx_write_context_cleanup(&fx);
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
        return hlsl_fx_5_write(ctx, out);
    }
    else
    {
        vkd3d_unreachable();
    }
}
