/*
 * Copyright 2020-2021 Zebediah Figura for CodeWeavers
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

/*
 * This application contains code derived from piglit, the license for which
 * follows:
 *
 * Copyright © 2010 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifdef __MINGW32__
# define _HRESULT_DEFINED
typedef int HRESULT;
#endif

#define COBJMACROS
#define CONST_VTABLE
#include "config.h"
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include "vkd3d_windows.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3dcompiler.h"
#include "vkd3d_common.h"
#include "vkd3d_test.h"
#include "shader_runner.h"

static void VKD3D_NORETURN VKD3D_PRINTF_FUNC(1, 2) fatal_error(const char *format, ...)
{
    va_list args;

    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    exit(1);
}

enum parse_state
{
    STATE_NONE,
    STATE_PREPROC,
    STATE_PREPROC_INVALID,
    STATE_REQUIRE,
    STATE_SAMPLER,
    STATE_SHADER_INVALID_PIXEL,
    STATE_SHADER_PIXEL,
    STATE_TEXTURE,
    STATE_TEST,
};

static bool match_string(const char *line, const char *token, const char **const rest)
{
    size_t len = strlen(token);

    if (strncmp(line, token, len) || !isspace(line[len]))
        return false;
    if (rest)
    {
        *rest = line + len;
        while (isspace(**rest))
            ++*rest;
    }
    return true;
}

static void parse_require_directive(struct shader_context *context, const char *line)
{
    if (match_string(line, "shader model >=", &line))
    {
        static const char *const model_strings[] =
        {
            [SHADER_MODEL_4_0] = "4.0",
            [SHADER_MODEL_4_1] = "4.1",
            [SHADER_MODEL_5_0] = "5.0",
            [SHADER_MODEL_5_1] = "5.1",
        };
        unsigned int i;

        for (i = 0; i < ARRAY_SIZE(model_strings); ++i)
        {
            if (match_string(line, model_strings[i], &line))
            {
                context->minimum_shader_model = i;
                return;
            }
        }

        fatal_error("Unknown shader model '%s'.\n", line);
    }
    else
    {
        fatal_error("Unknown require directive '%s'.\n", line);
    }
}

static void parse_texture_format(struct texture_params *texture, const char *line)
{
    static const struct
    {
        const char *string;
        enum texture_data_type data_type;
        unsigned int texel_size;
        DXGI_FORMAT format;
    }
    formats[] =
    {
        {"r32g32b32a32 float",  TEXTURE_DATA_FLOAT, 16, DXGI_FORMAT_R32G32B32A32_FLOAT},
        {"r32g32 uint",         TEXTURE_DATA_UINT,   8, DXGI_FORMAT_R32G32_UINT},
        {"r32 float",           TEXTURE_DATA_FLOAT,  4, DXGI_FORMAT_R32_FLOAT},
        {"r32 sint",            TEXTURE_DATA_SINT,   4, DXGI_FORMAT_R32_SINT},
    };
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(formats); ++i)
    {
        if (match_string(line, formats[i].string, &line))
        {
            texture->format = formats[i].format;
            texture->data_type = formats[i].data_type;
            texture->texel_size = formats[i].texel_size;
            return;
        }
    }

    fatal_error("Unknown format '%s'.\n", line);
}

static D3D12_TEXTURE_ADDRESS_MODE parse_sampler_address_mode(const char *line, const char **rest)
{
    if (match_string(line, "border", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_BORDER;
    if (match_string(line, "clamp", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    if (match_string(line, "mirror_once", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_MIRROR_ONCE;
    if (match_string(line, "mirror", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_MIRROR;
    if (match_string(line, "wrap", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_WRAP;

    fatal_error("Unknown sampler address mode '%s'.\n", line);
}

static void parse_sampler_directive(struct sampler *sampler, const char *line)
{
    if (match_string(line, "address", &line))
    {
        sampler->u_address = parse_sampler_address_mode(line, &line);
        sampler->v_address = parse_sampler_address_mode(line, &line);
        sampler->w_address = parse_sampler_address_mode(line, &line);
    }
    else if (match_string(line, "filter", &line))
    {
        static const struct
        {
            const char *string;
            D3D12_FILTER filter;
        }
        filters[] =
        {
            {"point point point",       D3D12_FILTER_MIN_MAG_MIP_POINT},
            {"point point linear",      D3D12_FILTER_MIN_MAG_POINT_MIP_LINEAR},
            {"point linear point",      D3D12_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT},
            {"point linear linear",     D3D12_FILTER_MIN_POINT_MAG_MIP_LINEAR},
            {"linear point point",      D3D12_FILTER_MIN_LINEAR_MAG_MIP_POINT},
            {"linear point linear",     D3D12_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR},
            {"linear linear point",     D3D12_FILTER_MIN_MAG_LINEAR_MIP_POINT},
            {"linear linear linear",    D3D12_FILTER_MIN_MAG_MIP_LINEAR},
        };
        unsigned int i;

        for (i = 0; i < ARRAY_SIZE(filters); ++i)
        {
            if (match_string(line, filters[i].string, &line))
            {
                sampler->filter = filters[i].filter;
                return;
            }
        }

        fatal_error("Unknown sampler filter '%s'.\n", line);
    }
    else
    {
        fatal_error("Unknown sampler directive '%s'.\n", line);
    }
}

static void parse_texture_directive(struct texture_params *texture, const char *line)
{
    int ret;

    if (match_string(line, "format", &line))
    {
        parse_texture_format(texture, line);
    }
    else if (match_string(line, "size", &line))
    {
        ret = sscanf(line, "( %u , %u )", &texture->width, &texture->height);
        if (ret < 2)
            fatal_error("Malformed texture size '%s'.\n", line);
    }
    else
    {
        union
        {
            float f;
            int32_t i;
            uint32_t u;
        } u;
        char *rest;

        u.u = 0;

        for (;;)
        {
            switch (texture->data_type)
            {
                case TEXTURE_DATA_FLOAT:
                    u.f = strtof(line, &rest);
                    break;

                case TEXTURE_DATA_SINT:
                    u.i = strtol(line, &rest, 10);
                    break;

                case TEXTURE_DATA_UINT:
                    u.u = strtoul(line, &rest, 10);
                    break;
            }

            if (rest == line)
                break;

            vkd3d_array_reserve((void **)&texture->data, &texture->data_capacity, texture->data_size + sizeof(u), 1);
            memcpy(texture->data + texture->data_size, &u, sizeof(u));
            texture->data_size += sizeof(u);
            line = rest;
        }
    }
}

static void parse_test_directive(struct shader_context *context, const char *line)
{
    if (match_string(line, "draw quad", &line))
    {
        context->ops->draw_quad(context);
    }
    else if (match_string(line, "probe all rgba", &line))
    {
        static const RECT rect = {0, 0, 640, 480};
        unsigned int ulps;
        struct vec4 v;
        int ret;

        ret = sscanf(line, "( %f , %f , %f , %f ) %u", &v.x, &v.y, &v.z, &v.w, &ulps);
        if (ret < 4)
            fatal_error("Malformed probe arguments '%s'.\n", line);
        if (ret < 5)
            ulps = 0;

        context->ops->probe_vec4(context, &rect, &v, ulps);
    }
    else if (match_string(line, "probe rect rgba", &line))
    {
        unsigned int x, y, w, h, ulps;
        struct vec4 v;
        RECT rect;
        int ret;

        ret = sscanf(line, "( %u , %u , %u , %u ) ( %f , %f , %f , %f ) %u",
                     &x, &y, &w, &h, &v.x, &v.y, &v.z, &v.w, &ulps);
        if (ret < 8)
            fatal_error("Malformed probe arguments '%s'.\n", line);
        if (ret < 9)
            ulps = 0;

        rect.left = x;
        rect.right = x + w;
        rect.top = y;
        rect.bottom = y + h;
        context->ops->probe_vec4(context, &rect, &v, ulps);
    }
    else if (match_string(line, "probe rgba", &line))
    {
        unsigned int x, y, ulps;
        struct vec4 v;
        RECT rect;
        int ret;

        ret = sscanf(line, "( %u , %u ) ( %f , %f , %f , %f ) %u", &x, &y, &v.x, &v.y, &v.z, &v.w, &ulps);
        if (ret < 6)
            fatal_error("Malformed probe arguments '%s'.\n", line);
        if (ret < 7)
            ulps = 0;

        rect.left = x;
        rect.right = x + 1;
        rect.top = y;
        rect.bottom = y + 1;
        context->ops->probe_vec4(context, &rect, &v, ulps);
    }
    else if (match_string(line, "uniform", &line))
    {
        unsigned int offset;

        if (!sscanf(line, "%u", &offset))
            fatal_error("Unknown uniform type '%s'.\n", line);
        line = strchr(line, ' ') + 1;

        if (match_string(line, "float4", &line))
        {
            struct vec4 v;

            if (sscanf(line, "%f %f %f %f", &v.x, &v.y, &v.z, &v.w) < 4)
                fatal_error("Malformed float4 constant '%s'.\n", line);
            if (offset + 4 > context->uniform_count)
            {
                context->uniform_count = offset + 4;
                context->uniforms = realloc(context->uniforms, context->uniform_count * sizeof(*context->uniforms));
            }
            memcpy(context->uniforms + offset, &v, sizeof(v));
        }
        else if (match_string(line, "float", &line))
        {
            float f;

            if (sscanf(line, "%f", &f) < 1)
                fatal_error("Malformed float constant '%s'.\n", line);
            if (offset + 1 > context->uniform_count)
            {
                context->uniform_count = offset + 1;
                context->uniforms = realloc(context->uniforms, context->uniform_count * sizeof(*context->uniforms));
            }
            memcpy(context->uniforms + offset, &f, sizeof(f));
        }
        else if (match_string(line, "int", &line))
        {
            int i;

            if (sscanf(line, "%i", &i) < 1)
                fatal_error("Malformed int constant '%s'.\n", line);
            if (offset + 1 > context->uniform_count)
            {
                context->uniform_count = offset + 1;
                context->uniforms = realloc(context->uniforms, context->uniform_count * sizeof(*context->uniforms));
            }
            memcpy(context->uniforms + offset, &i, sizeof(i));
        }
        else if (match_string(line, "uint", &line))
        {
            unsigned int u;

            if (sscanf(line, "%u", &u) < 1)
                fatal_error("Malformed uint constant '%s'.\n", line);
            if (offset + 1 > context->uniform_count)
            {
                context->uniform_count = offset + 1;
                context->uniforms = realloc(context->uniforms, context->uniform_count * sizeof(*context->uniforms));
            }
            memcpy(context->uniforms + offset, &u, sizeof(u));
        }
    }
    else
    {
        fatal_error("Unknown test directive '%s'.\n", line);
    }
}

static struct sampler *get_sampler(struct shader_context *context, unsigned int slot)
{
    struct sampler *sampler;
    size_t i;

    for (i = 0; i < context->sampler_count; ++i)
    {
        sampler = &context->samplers[i];

        if (sampler->slot == slot)
            return sampler;
    }

    return NULL;
}

static void set_texture(struct shader_context *context, struct texture *texture)
{
    size_t i;

    for (i = 0; i < context->texture_count; ++i)
    {
        if (context->textures[i]->slot == texture->slot)
        {
            context->ops->destroy_texture(context, context->textures[i]);
            context->textures[i] = texture;
            return;
        }
    }

    context->textures = realloc(context->textures, (context->texture_count + 1) * sizeof(*context->textures));
    context->textures[context->texture_count++] = texture;
}

void run_shader_tests(struct shader_context *context, int argc, char **argv, const struct shader_runner_ops *ops)
{
    size_t shader_source_size = 0, shader_source_len = 0;
    struct sampler *current_sampler = NULL;
    struct texture_params current_texture;
    enum parse_state state = STATE_NONE;
    unsigned int i, line_number = 0;
    const char *filename = NULL;
    char *shader_source = NULL;
    char line[256];
    FILE *f;

    for (i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
        {
            filename = argv[i];
            break;
        }
    }

    if (!filename)
    {
        fprintf(stderr, "Usage: %s [file]\n", argv[0]);
        return;
    }

    if (!(f = fopen(filename, "r")))
    {
        fatal_error("Unable to open '%s' for reading: %s\n", argv[1], strerror(errno));
        return;
    }

    memset(context, 0, sizeof(*context));
    context->ops = ops;

    for (;;)
    {
        char *ret = fgets(line, sizeof(line), f);

        ++line_number;

        if (!ret || line[0] == '[')
        {
            switch (state)
            {
                case STATE_NONE:
                case STATE_REQUIRE:
                case STATE_SAMPLER:
                case STATE_TEST:
                    break;

                case STATE_TEXTURE:
                    set_texture(context, context->ops->create_texture(context, &current_texture));
                    free(current_texture.data);
                    break;

                case STATE_SHADER_PIXEL:
                    if (!(context->ps_code = context->ops->compile_shader(context,
                            shader_source, context->minimum_shader_model)))
                        return;
                    shader_source_len = 0;
                    break;

                case STATE_SHADER_INVALID_PIXEL:
                {
                    ID3D10Blob *blob = NULL, *errors = NULL;
                    HRESULT hr;

                    hr = D3DCompile(shader_source, strlen(shader_source), NULL,
                            NULL, NULL, "main", "ps_4_0", 0, 0, &blob, &errors);
                    ok(hr == E_FAIL, "Got unexpected hr %#x.\n", hr);
                    ok(!blob, "Expected no compiled shader blob.\n");
                    ok(!!errors, "Expected non-NULL error blob.\n");
                    if (!errors)
                        return;

                    if (vkd3d_test_state.debug_level)
                        trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
                    ID3D10Blob_Release(errors);

                    shader_source_len = 0;
                    break;
                }

                case STATE_PREPROC_INVALID:
                {
                    ID3D10Blob *blob = NULL, *errors = NULL;
                    HRESULT hr;

                    hr = D3DPreprocess(shader_source, strlen(shader_source), NULL, NULL, NULL, &blob, &errors);
                    ok(hr == E_FAIL, "Got unexpected hr %#x.\n", hr);
                    ok(!blob, "Expected no compiled shader blob.\n");
                    ok(!!errors, "Expected non-NULL error blob.\n");

                    if (errors)
                    {
                        if (vkd3d_test_state.debug_level)
                            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
                        ID3D10Blob_Release(errors);
                    }

                    shader_source_len = 0;
                    break;
                }

                case STATE_PREPROC:
                {
                    ID3D10Blob *blob = NULL, *errors = NULL;
                    SIZE_T size;
                    HRESULT hr;
                    char *text;

                    hr = D3DPreprocess(shader_source, strlen(shader_source), NULL, NULL, NULL, &blob, &errors);
                    ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
                    if (hr == S_OK)
                    {
                        if (errors)
                        {
                            if (vkd3d_test_state.debug_level)
                                trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
                            ID3D10Blob_Release(errors);
                        }

                        text = ID3D10Blob_GetBufferPointer(blob);
                        size = ID3D10Blob_GetBufferSize(blob);
                        ok(vkd3d_memmem(text, size, "pass", strlen("pass")),
                                "'pass' not found in preprocessed shader.\n");
                        ok(!vkd3d_memmem(text, size, "fail", strlen("fail")),
                                "'fail' found in preprocessed shader.\n");
                        ID3D10Blob_Release(blob);
                    }

                    shader_source_len = 0;
                    break;
                }
            }
        }

        if (!ret)
            break;

        if (line[0] == '[')
        {
            unsigned int index;

            if (!strcmp(line, "[require]\n"))
            {
                state = STATE_REQUIRE;
            }
            else if (!strcmp(line, "[pixel shader]\n"))
            {
                state = STATE_SHADER_PIXEL;

                if (context->ps_code)
                    ID3D10Blob_Release(context->ps_code);
                context->ps_code = NULL;
            }
            else if (!strcmp(line, "[pixel shader fail]\n"))
            {
                state = STATE_SHADER_INVALID_PIXEL;
            }
            else if (sscanf(line, "[sampler %u]\n", &index))
            {
                state = STATE_SAMPLER;

                if ((current_sampler = get_sampler(context, index)))
                {
                    memset(current_sampler, 0, sizeof(*current_sampler));
                }
                else
                {
                    context->samplers = realloc(context->samplers,
                            ++context->sampler_count * sizeof(*context->samplers));
                    current_sampler = &context->samplers[context->sampler_count - 1];
                    memset(current_sampler, 0, sizeof(*current_sampler));
                }
                current_sampler->slot = index;
                current_sampler->filter = D3D12_FILTER_MIN_MAG_MIP_POINT;
                current_sampler->u_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
                current_sampler->v_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
                current_sampler->w_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
            }
            else if (sscanf(line, "[texture %u]\n", &index))
            {
                state = STATE_TEXTURE;

                memset(&current_texture, 0, sizeof(current_texture));

                current_texture.slot = index;
                current_texture.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_texture.data_type = TEXTURE_DATA_FLOAT;
                current_texture.texel_size = 16;
            }
            else if (!strcmp(line, "[test]\n"))
            {
                state = STATE_TEST;
            }
            else if (!strcmp(line, "[preproc]\n"))
            {
                state = STATE_PREPROC;
            }
            else if (!strcmp(line, "[preproc fail]\n"))
            {
                state = STATE_PREPROC_INVALID;
            }

            vkd3d_test_set_context("Section %.*s, line %u", strlen(line) - 1, line, line_number);
        }
        else if (line[0] != '%' && line[0] != '\n')
        {
            switch (state)
            {
                case STATE_NONE:
                    fatal_error("Malformed line '%s'.\n", line);
                    break;

                case STATE_PREPROC:
                case STATE_PREPROC_INVALID:
                case STATE_SHADER_INVALID_PIXEL:
                case STATE_SHADER_PIXEL:
                {
                    size_t len = strlen(line);

                    vkd3d_array_reserve((void **)&shader_source, &shader_source_size, shader_source_len + len + 1, 1);
                    memcpy(shader_source + shader_source_len, line, len + 1);
                    shader_source_len += len;
                    break;
                }

                case STATE_REQUIRE:
                    parse_require_directive(context, line);
                    break;

                case STATE_SAMPLER:
                    parse_sampler_directive(current_sampler, line);
                    break;

                case STATE_TEXTURE:
                    parse_texture_directive(&current_texture, line);
                    break;

                case STATE_TEST:
                    parse_test_directive(context, line);
                    break;
            }
        }
    }

    if (context->ps_code)
        ID3D10Blob_Release(context->ps_code);
    for (i = 0; i < context->texture_count; ++i)
    {
        if (context->textures[i])
            context->ops->destroy_texture(context, context->textures[i]);
    }
    free(context->textures);

    fclose(f);
}

START_TEST(shader_runner)
{
    run_shader_tests_d3d12(argc, argv);
}
