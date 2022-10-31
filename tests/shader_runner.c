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
#include <string.h>
#include "vkd3d_windows.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3dcompiler.h"
#include "vkd3d_test.h"
#include "shader_runner.h"

void fatal_error(const char *format, ...)
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
    STATE_INPUT_LAYOUT,
    STATE_PREPROC,
    STATE_PREPROC_INVALID,
    STATE_REQUIRE,
    STATE_RESOURCE,
    STATE_SAMPLER,
    STATE_SHADER_COMPUTE,
    STATE_SHADER_COMPUTE_TODO,
    STATE_SHADER_PIXEL,
    STATE_SHADER_PIXEL_TODO,
    STATE_SHADER_VERTEX,
    STATE_TEST,
};

static bool match_string(const char *line, const char *token, const char **const rest)
{
    size_t len = strlen(token);

    while (isspace(*line))
        ++line;

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

static void parse_require_directive(struct shader_runner *runner, const char *line)
{
    if (match_string(line, "shader model >=", &line))
    {
        static const char *const model_strings[] =
        {
            [SHADER_MODEL_2_0] = "2.0",
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
                runner->minimum_shader_model = i;
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

static DXGI_FORMAT parse_format(const char *line, enum texture_data_type *data_type, unsigned int *texel_size, const char **rest)
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
        {"r32g32 float",        TEXTURE_DATA_FLOAT,  8, DXGI_FORMAT_R32G32_FLOAT},
        {"r32g32 uint",         TEXTURE_DATA_UINT,   8, DXGI_FORMAT_R32G32_UINT},
        {"r32 float",           TEXTURE_DATA_FLOAT,  4, DXGI_FORMAT_R32_FLOAT},
        {"r32 sint",            TEXTURE_DATA_SINT,   4, DXGI_FORMAT_R32_SINT},
        {"r32 uint",            TEXTURE_DATA_UINT,   4, DXGI_FORMAT_R32_UINT},
    };
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(formats); ++i)
    {
        if (match_string(line, formats[i].string, rest))
        {
            if (data_type)
                *data_type = formats[i].data_type;
            *texel_size = formats[i].texel_size;
            return formats[i].format;
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

static void parse_resource_directive(struct resource_params *resource, const char *line)
{
    int ret;

    if (match_string(line, "format", &line))
    {
        resource->format = parse_format(line, &resource->data_type, &resource->texel_size, &line);
    }
    else if (match_string(line, "size", &line))
    {
        ret = sscanf(line, "( %u , %u )", &resource->width, &resource->height);
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
            switch (resource->data_type)
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

            vkd3d_array_reserve((void **)&resource->data, &resource->data_capacity, resource->data_size + sizeof(u), 1);
            memcpy(resource->data + resource->data_size, &u, sizeof(u));
            resource->data_size += sizeof(u);
            line = rest;
        }
    }
}

static void parse_input_layout_directive(struct shader_runner *runner, const char *line)
{
    struct input_element *element;
    const char *rest;

    vkd3d_array_reserve((void **)&runner->input_elements, &runner->input_element_capacity,
            runner->input_element_count + 1, sizeof(*runner->input_elements));
    element = &runner->input_elements[runner->input_element_count++];

    element->slot = strtoul(line, (char **)&rest, 10);
    if (rest == line)
        fatal_error("Malformed input layout directive '%s'.\n", line);
    line = rest;

    element->format = parse_format(line, NULL, &element->texel_size, &line);

    if (!(rest = strpbrk(line, " \n")))
        rest = line + strlen(line);
    element->name = malloc(rest - line + 1);
    memcpy(element->name, line, rest - line);
    element->name[rest - line] = 0;
    line = rest;

    element->index = strtoul(line, (char **)&rest, 10);
    if (rest == line)
        element->index = 0;
}

void init_resource(struct resource *resource, const struct resource_params *params)
{
    resource->type = params->type;
    resource->slot = params->slot;
    resource->format = params->format;
    resource->size = params->data_size;
    resource->texel_size = params->texel_size;
    resource->width = params->width;
    resource->height = params->height;
}

static struct resource *get_resource(struct shader_runner *runner, enum resource_type type, unsigned int slot)
{
    struct resource *resource;
    size_t i;

    for (i = 0; i < runner->resource_count; ++i)
    {
        resource = runner->resources[i];

        if (resource->type == type && resource->slot == slot)
            return resource;
    }

    return NULL;
}

static void set_resource(struct shader_runner *runner, struct resource *resource)
{
    size_t i;

    for (i = 0; i < runner->resource_count; ++i)
    {
        if (runner->resources[i]->slot == resource->slot && runner->resources[i]->type == resource->type)
        {
            runner->ops->destroy_resource(runner, runner->resources[i]);
            runner->resources[i] = resource;
            return;
        }
    }

    if (runner->resource_count == MAX_RESOURCES)
        fatal_error("Too many resources declared.\n");

    runner->resources[runner->resource_count++] = resource;
}

static void set_uniforms(struct shader_runner *runner, size_t offset, size_t count, const void *uniforms)
{
    runner->uniform_count = align(max(runner->uniform_count, offset + count), 4);
    vkd3d_array_reserve((void **)&runner->uniforms, &runner->uniform_capacity,
            runner->uniform_count, sizeof(*runner->uniforms));
    memcpy(runner->uniforms + offset, uniforms, count * sizeof(*runner->uniforms));
}

static void parse_test_directive(struct shader_runner *runner, const char *line)
{
    char *rest;
    int ret;

    runner->is_todo = false;

    if (match_string(line, "todo", &line))
        runner->is_todo = true;

    if (match_string(line, "dispatch", &line))
    {
        unsigned int x, y, z;

        ret = sscanf(line, "%u %u %u", &x, &y, &z);
        if (ret < 3)
            fatal_error("Malformed dispatch arguments '%s'.\n", line);

        runner->last_render_failed = !runner->ops->dispatch(runner, x, y, z);
    }
    else if (match_string(line, "draw quad", &line))
    {
        struct resource_params params;
        struct input_element *element;

        /* For simplicity, draw a large triangle instead. */
        static const struct vec2 quad[] =
        {
            {-2.0f, -2.0f},
            {-2.0f,  4.0f},
            { 4.0f, -2.0f},
        };

        static const char vs_source[] =
            "void main(inout float4 position : sv_position)\n"
            "{\n"
            "}";

        if (!get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0))
        {
            memset(&params, 0, sizeof(params));
            params.slot = 0;
            params.type = RESOURCE_TYPE_RENDER_TARGET;
            params.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
            params.data_type = TEXTURE_DATA_FLOAT;
            params.texel_size = 16;
            params.width = RENDER_TARGET_WIDTH;
            params.height = RENDER_TARGET_HEIGHT;

            set_resource(runner, runner->ops->create_resource(runner, &params));
        }

        vkd3d_array_reserve((void **)&runner->input_elements, &runner->input_element_capacity,
                1, sizeof(*runner->input_elements));
        element = &runner->input_elements[0];
        element->name = strdup("sv_position");
        element->slot = 0;
        element->format = DXGI_FORMAT_R32G32_FLOAT;
        element->texel_size = sizeof(*quad);
        element->index = 0;
        runner->input_element_count = 1;

        memset(&params, 0, sizeof(params));
        params.slot = 0;
        params.type = RESOURCE_TYPE_VERTEX_BUFFER;
        params.data = malloc(sizeof(quad));
        memcpy(params.data, quad, sizeof(quad));
        params.data_size = sizeof(quad);
        set_resource(runner, runner->ops->create_resource(runner, &params));

        if (!runner->vs_source)
            runner->vs_source = strdup(vs_source);

        runner->last_render_failed = !runner->ops->draw(runner, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST, 3);
    }
    else if (match_string(line, "draw", &line))
    {
        D3D_PRIMITIVE_TOPOLOGY topology;
        struct resource_params params;
        unsigned int vertex_count;
        char *rest;

        if (!get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0))
        {
            memset(&params, 0, sizeof(params));
            params.slot = 0;
            params.type = RESOURCE_TYPE_RENDER_TARGET;
            params.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
            params.data_type = TEXTURE_DATA_FLOAT;
            params.texel_size = 16;
            params.width = RENDER_TARGET_WIDTH;
            params.height = RENDER_TARGET_HEIGHT;

            set_resource(runner, runner->ops->create_resource(runner, &params));
        }

        if (match_string(line, "triangle list", &line))
            topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
        else if (match_string(line, "triangle strip", &line))
            topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP;
        else
            fatal_error("Unknown primitive topology '%s'.\n", line);

        vertex_count = strtoul(line, &rest, 10);
        if (line == rest)
            fatal_error("Malformed vertex count '%s'.\n", line);

        runner->last_render_failed = !runner->ops->draw(runner, topology, vertex_count);
    }
    else if (match_string(line, "probe", &line))
    {
        unsigned int left, top, right, bottom, ulps, slot;
        struct resource_readback *rb;
        struct resource *resource;
        RECT rect;
        int len;

        if (runner->last_render_failed)
            return;

        if (match_string(line, "uav", &line))
        {
            slot = strtoul(line, &rest, 10);

            if (rest == line)
                fatal_error("Malformed UAV index '%s'.\n", line);
            line = rest;

            resource = get_resource(runner, RESOURCE_TYPE_UAV, slot);
        }
        else if (match_string(line, "render target", &line))
        {
            slot = strtoul(line, &rest, 10);

            if (rest == line)
                fatal_error("Malformed render target index '%s'.\n", line);
            line = rest;

            resource = get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, slot);
        }
        else
        {
            resource = get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0);
        }

        rb = runner->ops->get_resource_readback(runner, resource);

        if (match_string(line, "all", &line))
        {
            set_rect(&rect, 0, 0, resource->width, resource->height);
        }
        else if (sscanf(line, " ( %d , %d , %d , %d )%n", &left, &top, &right, &bottom, &len) == 4)
        {
            set_rect(&rect, left, top, right, bottom);
            line += len;
        }
        else if (sscanf(line, " ( %u , %u )%n", &left, &top, &len) == 2)
        {
            set_rect(&rect, left, top, left + 1, top + 1);
            line += len;
        }
        else
        {
            fatal_error("Malformed probe arguments '%s'.\n", line);
        }

        if (match_string(line, "rgba", &line))
        {
            struct vec4 v;

            ret = sscanf(line, "( %f , %f , %f , %f ) %u", &v.x, &v.y, &v.z, &v.w, &ulps);
            if (ret < 4)
                fatal_error("Malformed probe arguments '%s'.\n", line);
            if (ret < 5)
                ulps = 0;
            todo_if(runner->is_todo) check_readback_data_vec4(rb, &rect, &v, ulps);
        }
        else if (match_string(line, "r", &line))
        {
            float expect;

            ret = sscanf(line, "( %f ) %u", &expect, &ulps);
            if (ret < 1)
                fatal_error("Malformed probe arguments '%s'.\n", line);
            if (ret < 2)
                ulps = 0;
            todo_if(runner->is_todo) check_readback_data_float(rb, &rect, expect, ulps);
        }
        else
        {
            fatal_error("Malformed probe arguments '%s'.\n", line);
        }

        runner->ops->release_readback(runner, rb);
    }
    else if (match_string(line, "uniform", &line))
    {
        unsigned int offset;

        if (!sscanf(line, "%u", &offset))
            fatal_error("Malformed uniform offset '%s'.\n", line);
        line = strchr(line, ' ') + 1;

        if (match_string(line, "float4", &line))
        {
            struct vec4 v;

            if (sscanf(line, "%f %f %f %f", &v.x, &v.y, &v.z, &v.w) < 4)
                fatal_error("Malformed float4 constant '%s'.\n", line);
            set_uniforms(runner, offset, 4, &v);
        }
        else if (match_string(line, "float", &line))
        {
            float f;

            if (sscanf(line, "%f", &f) < 1)
                fatal_error("Malformed float constant '%s'.\n", line);
            set_uniforms(runner, offset, 1, &f);
        }
        else if (match_string(line, "int4", &line) || match_string(line, "uint4", &line))
        {
            int v[4];

            if (sscanf(line, "%i %i %i %i", &v[0], &v[1], &v[2], &v[3]) < 4)
                fatal_error("Malformed (u)int4 constant '%s'.\n", line);
            set_uniforms(runner, offset, 4, v);
        }
        else if (match_string(line, "int", &line))
        {
            int i;

            if (sscanf(line, "%i", &i) < 1)
                fatal_error("Malformed int constant '%s'.\n", line);
            set_uniforms(runner, offset, 1, &i);
        }
        else if (match_string(line, "uint", &line))
        {
            unsigned int u;

            if (sscanf(line, "%u", &u) < 1)
                fatal_error("Malformed uint constant '%s'.\n", line);
            set_uniforms(runner, offset, 1, &u);
        }
        else
        {
            fatal_error("Unknown uniform type '%s'.\n", line);
        }
    }
    else
    {
        fatal_error("Unknown test directive '%s'.\n", line);
    }
}

static struct sampler *get_sampler(struct shader_runner *runner, unsigned int slot)
{
    struct sampler *sampler;
    size_t i;

    for (i = 0; i < runner->sampler_count; ++i)
    {
        sampler = &runner->samplers[i];

        if (sampler->slot == slot)
            return sampler;
    }

    return NULL;
}

unsigned int get_vb_stride(const struct shader_runner *runner, unsigned int slot)
{
    unsigned int stride = 0;
    size_t i;

    /* We currently don't deal with vertex formats less than 32 bits, so don't
     * bother with alignment. */
    for (i = 0; i < runner->input_element_count; ++i)
    {
        const struct input_element *element = &runner->input_elements[i];

        if (element->slot == slot)
            stride += element->texel_size;
    }

    return stride;
}

static void compile_shader(struct shader_runner *runner, const char *source, size_t len, const char *type, HRESULT expect)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    char profile[7];
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "4_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
    };

    sprintf(profile, "%s_%s", type, shader_models[runner->minimum_shader_model]);
    hr = D3DCompile(source, len, NULL, NULL, NULL, "main", profile, 0, 0, &blob, &errors);
    ok(hr == expect, "Got unexpected hr %#x.\n", hr);
    if (hr == S_OK)
    {
        ID3D10Blob_Release(blob);
    }
    else
    {
        assert_that(!blob, "Expected no compiled shader blob.\n");
        assert_that(!!errors, "Expected non-NULL error blob.\n");
    }
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
}

static void run_shader_tests(struct shader_runner *runner, int argc, char **argv, const struct shader_runner_ops *ops)
{
    size_t shader_source_size = 0, shader_source_len = 0;
    struct resource_params current_resource;
    struct sampler *current_sampler = NULL;
    enum parse_state state = STATE_NONE;
    unsigned int i, line_number = 0;
    const char *filename = NULL;
    char *shader_source = NULL;
    HRESULT expect_hr = S_OK;
    char line[256];
    FILE *f;

    runner->minimum_shader_model = SHADER_MODEL_2_0;

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

    memset(runner, 0, sizeof(*runner));
    runner->ops = ops;

    for (;;)
    {
        char *ret = fgets(line, sizeof(line), f);

        ++line_number;

        if (!ret || line[0] == '[')
        {
            switch (state)
            {
                case STATE_INPUT_LAYOUT:
                case STATE_NONE:
                case STATE_SAMPLER:
                case STATE_TEST:
                    break;

                case STATE_REQUIRE:
                    if (runner->ops->check_requirements && !runner->ops->check_requirements(runner))
                        goto out;
                    break;

                case STATE_RESOURCE:
                    set_resource(runner, runner->ops->create_resource(runner, &current_resource));
                    free(current_resource.data);
                    break;

                case STATE_SHADER_COMPUTE:
                case STATE_SHADER_COMPUTE_TODO:
                    todo_if (state == STATE_SHADER_COMPUTE_TODO)
                        compile_shader(runner, shader_source, shader_source_len, "cs", expect_hr);
                    free(runner->cs_source);
                    runner->cs_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_SHADER_PIXEL:
                case STATE_SHADER_PIXEL_TODO:
                    todo_if (state == STATE_SHADER_PIXEL_TODO)
                        compile_shader(runner, shader_source, shader_source_len, "ps", expect_hr);
                    free(runner->ps_source);
                    runner->ps_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_SHADER_VERTEX:
                    compile_shader(runner, shader_source, shader_source_len, "vs", expect_hr);
                    free(runner->vs_source);
                    runner->vs_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

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

            if (!strcmp(line, "[compute shader]\n"))
            {
                state = STATE_SHADER_COMPUTE;
                expect_hr = S_OK;
            }
            else if (!strcmp(line, "[compute shader todo]\n"))
            {
                state = STATE_SHADER_COMPUTE_TODO;
                expect_hr = S_OK;
            }
            else if (!strcmp(line, "[compute shader fail]\n"))
            {
                state = STATE_SHADER_COMPUTE;
                expect_hr = E_FAIL;
            }
            else if (!strcmp(line, "[compute shader fail todo]\n"))
            {
                state = STATE_SHADER_COMPUTE_TODO;
                expect_hr = E_FAIL;
            }
            else if (!strcmp(line, "[require]\n"))
            {
                state = STATE_REQUIRE;
            }
            else if (!strcmp(line, "[pixel shader]\n"))
            {
                state = STATE_SHADER_PIXEL;
                expect_hr = S_OK;
            }
            else if (!strcmp(line, "[pixel shader todo]\n"))
            {
                state = STATE_SHADER_PIXEL_TODO;
                expect_hr = S_OK;
            }
            else if (!strcmp(line, "[pixel shader fail]\n"))
            {
                state = STATE_SHADER_PIXEL;
                expect_hr = E_FAIL;
            }
            else if (!strcmp(line, "[pixel shader fail todo]\n"))
            {
                state = STATE_SHADER_PIXEL_TODO;
                expect_hr = E_FAIL;
            }
            else if (!strcmp(line, "[pixel shader notimpl]\n"))
            {
                state = STATE_SHADER_PIXEL;
                expect_hr = E_NOTIMPL;
            }
            else if (!strcmp(line, "[pixel shader notimpl todo]\n"))
            {
                state = STATE_SHADER_PIXEL_TODO;
                expect_hr = E_NOTIMPL;
            }
            else if (sscanf(line, "[sampler %u]\n", &index))
            {
                state = STATE_SAMPLER;

                if (!(current_sampler = get_sampler(runner, index)))
                {
                    if (runner->sampler_count == MAX_SAMPLERS)
                        fatal_error("Too many samplers declared.\n");

                    current_sampler = &runner->samplers[runner->sampler_count++];
                }
                memset(current_sampler, 0, sizeof(*current_sampler));
                current_sampler->slot = index;
                current_sampler->filter = D3D12_FILTER_MIN_MAG_MIP_POINT;
                current_sampler->u_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
                current_sampler->v_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
                current_sampler->w_address = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
            }
            else if (sscanf(line, "[render target %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_RENDER_TARGET;
                current_resource.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
                current_resource.texel_size = 16;
            }
            else if (sscanf(line, "[texture %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_TEXTURE;
                current_resource.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
                current_resource.texel_size = 16;
            }
            else if (sscanf(line, "[uav %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_UAV;
                current_resource.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
                current_resource.texel_size = 16;
            }
            else if (sscanf(line, "[vertex buffer %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_VERTEX_BUFFER;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
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
            else if (!strcmp(line, "[vertex shader]\n"))
            {
                state = STATE_SHADER_VERTEX;
            }
            else if (!strcmp(line, "[input layout]\n"))
            {
                state = STATE_INPUT_LAYOUT;

                for (i = 0; i < runner->input_element_count; ++i)
                    free(runner->input_elements[i].name);
                runner->input_element_count = 0;
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

                case STATE_INPUT_LAYOUT:
                    parse_input_layout_directive(runner, line);
                    break;

                case STATE_PREPROC:
                case STATE_PREPROC_INVALID:
                case STATE_SHADER_COMPUTE:
                case STATE_SHADER_COMPUTE_TODO:
                case STATE_SHADER_PIXEL:
                case STATE_SHADER_PIXEL_TODO:
                case STATE_SHADER_VERTEX:
                {
                    size_t len = strlen(line);

                    vkd3d_array_reserve((void **)&shader_source, &shader_source_size, shader_source_len + len + 1, 1);
                    memcpy(shader_source + shader_source_len, line, len + 1);
                    shader_source_len += len;
                    break;
                }

                case STATE_REQUIRE:
                    parse_require_directive(runner, line);
                    break;

                case STATE_RESOURCE:
                    parse_resource_directive(&current_resource, line);
                    break;

                case STATE_SAMPLER:
                    parse_sampler_directive(current_sampler, line);
                    break;

                case STATE_TEST:
                    parse_test_directive(runner, line);
                    break;
            }
        }
    }

out:
    for (i = 0; i < runner->input_element_count; ++i)
        free(runner->input_elements[i].name);
    free(runner->input_elements);
    free(runner->vs_source);
    free(runner->ps_source);
    for (i = 0; i < runner->resource_count; ++i)
    {
        if (runner->resources[i])
            runner->ops->destroy_resource(runner, runner->resources[i]);
    }

    fclose(f);

    vkd3d_test_set_context(NULL);
}

#ifdef _WIN32
static void print_dll_version(const char *file_name)
{
    BOOL (WINAPI *GetFileVersionInfoA)(const char *, DWORD, DWORD, void *);
    BOOL (WINAPI *VerQueryValueA)(void *, char *, void **, UINT*);
    DWORD (WINAPI *GetFileVersionInfoSizeA)(const char *, DWORD *);
    HMODULE version_module;
    DWORD size, handle;
    bool done = false;

    version_module = LoadLibraryA("version.dll");
    if (!version_module)
        goto out;

#define X(name) name = (void *)GetProcAddress(version_module, #name);
    X(GetFileVersionInfoSizeA);
    X(GetFileVersionInfoA);
    X(VerQueryValueA);
#undef X

    if (!GetFileVersionInfoSizeA || !GetFileVersionInfoA || !VerQueryValueA)
    {
        FreeLibrary(version_module);
        goto out;
    }

    size = GetFileVersionInfoSizeA(file_name, &handle);
    if (size)
    {
        char *data = malloc(size);

        if (GetFileVersionInfoA(file_name, handle, size, data))
        {
            VS_FIXEDFILEINFO *info;
            UINT len;

            if (VerQueryValueA(data, "\\", (void **)&info, &len))
            {
                trace("%s version: %lu.%lu.%lu.%lu\n", file_name,
                        info->dwFileVersionMS >> 16, info->dwFileVersionMS & 0xffff,
                        info->dwFileVersionLS >> 16, info->dwFileVersionLS & 0xffff);
                done = true;
            }
        }
        free(data);
    }

    FreeLibrary(version_module);

out:
    if (!done)
        trace("%s version: unknown\n", file_name);
}
#endif

START_TEST(shader_runner)
{
    shader_runner_run(run_shader_tests, argc, argv);
}
