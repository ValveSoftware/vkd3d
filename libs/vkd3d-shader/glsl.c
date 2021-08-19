/*
 * Copyright 2021 Atharva Nimbalkar
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

struct vkd3d_glsl_generator
{
    struct vkd3d_string_buffer buffer;
    struct vkd3d_shader_location location;
    struct vkd3d_shader_message_context *message_context;
    bool failed;
};

struct vkd3d_glsl_generator *vkd3d_glsl_generator_create(const struct vkd3d_shader_compile_info *compile_info,
        struct vkd3d_shader_message_context *message_context)
{
    struct vkd3d_glsl_generator *generator;

    if (!(generator = vkd3d_malloc(sizeof(*generator))))
        return NULL;

    memset(generator, 0, sizeof(*generator));
    generator->location.source_name = compile_info->source_name;
    generator->location.line = 2; /* Line 1 is the version token. */
    generator->message_context = message_context;
    return generator;
}

static void VKD3D_PRINTF_FUNC(3, 4) vkd3d_glsl_compiler_error(
        struct vkd3d_glsl_generator *generator,
        enum vkd3d_shader_error error, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vkd3d_shader_verror(generator->message_context, &generator->location, error, fmt, args);
    va_end(args);
    generator->failed = true;
    return;
}

static void vkd3d_glsl_handle_instruction(struct vkd3d_glsl_generator *generator,
        const struct vkd3d_shader_instruction *instruction)
{
    switch (instruction->handler_idx)
    {
        default:
            vkd3d_glsl_compiler_error(generator,
                    VKD3D_SHADER_ERROR_GLSL_INTERNAL,
                    "Unhandled instruction %#x", instruction->handler_idx);
            break;
    }

    ++generator->location.line;
    return;
}

int vkd3d_glsl_generator_generate(void *parser_data, const uint32_t *parser_ptr,
        struct vkd3d_glsl_generator *generator,
        struct vkd3d_shader_code *out)
{
    void *code;
    struct vkd3d_shader_instruction ins;

    vkd3d_string_buffer_printf(&generator->buffer, "#version 440\n\n");
    vkd3d_string_buffer_printf(&generator->buffer, "void main()\n{\n");

    while (!shader_sm4_is_end(parser_data, &parser_ptr))
    {
        shader_sm4_read_instruction(parser_data, &parser_ptr, &ins);

        if (ins.handler_idx == VKD3DSIH_INVALID)
        {
            vkd3d_glsl_compiler_error(generator,
                    VKD3D_SHADER_ERROR_GLSL_INTERNAL,
                    "Encountered unrecognized or invalid instruction.");
            break;
        }

        vkd3d_glsl_handle_instruction(generator, &ins);
    }

    if (generator->failed)
        return VKD3D_ERROR;

    vkd3d_string_buffer_printf(&generator->buffer, "}\n");

    if ((code = vkd3d_malloc(generator->buffer.buffer_size)))
    {
        memcpy(code, generator->buffer.buffer, generator->buffer.content_size);
        out->size = generator->buffer.content_size;
        out->code = code;
    }
    else return VKD3D_ERROR_OUT_OF_MEMORY;

    return VKD3D_OK;
}

void vkd3d_glsl_generator_destroy(struct vkd3d_glsl_generator *generator)
{
    vkd3d_string_buffer_cleanup(&generator->buffer);
    vkd3d_free(generator);
}
