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
#include "dxcompiler.h"

struct test_options test_options = {0};

#ifdef VKD3D_CROSSTEST
static const char HLSL_COMPILER[] = "d3dcompiler47.dll";
#else
static const char HLSL_COMPILER[] = "vkd3d-shader";
#endif
static const char *const model_strings[] =
{
    [SHADER_MODEL_2_0] = "2.0",
    [SHADER_MODEL_3_0] = "3.0",
    [SHADER_MODEL_4_0] = "4.0",
    [SHADER_MODEL_4_1] = "4.1",
    [SHADER_MODEL_5_0] = "5.0",
    [SHADER_MODEL_5_1] = "5.1",
    [SHADER_MODEL_6_0] = "6.0",
};

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
    STATE_SHADER_VERTEX_TODO,
    STATE_SHADER_EFFECT,
    STATE_SHADER_EFFECT_TODO,
    STATE_TEST,
};

static bool match_tag(struct shader_runner *runner, const char *tag)
{
    for (size_t i = 0; i < runner->caps->tag_count; ++i)
    {
        if (!strcmp(tag, runner->caps->tags[i]))
            return true;
    }

    return false;
}

static bool check_qualifier_args_conjunction(struct shader_runner *runner, const char *line, const char **const rest)
{
    bool holds = true;
    unsigned int i;

    static const struct
    {
        const char *text;
        enum shader_model sm_min, sm_max;
        bool tag;
    }
    valid_args[] =
    {
        {"sm>=4", SHADER_MODEL_4_0, SHADER_MODEL_6_0},
        {"sm>=6", SHADER_MODEL_6_0, SHADER_MODEL_6_0},
        {"sm<4",  SHADER_MODEL_2_0, SHADER_MODEL_4_0 - 1},
        {"sm<6",  SHADER_MODEL_2_0, SHADER_MODEL_6_0 - 1},
        {"glsl", 0, 0, true},
    };

    while (*line != ')' && *line != '|')
    {
        bool match = false;

        while (isspace(*line))
            ++line;

        for (i = 0; i < ARRAY_SIZE(valid_args); ++i)
        {
            const char *option_text = valid_args[i].text;
            size_t option_len = strlen(option_text);

            if (strncmp(line, option_text, option_len))
                continue;

            match = true;
            line += option_len;
            if (valid_args[i].tag)
                holds = match_tag(runner, option_text);
            else if (runner->minimum_shader_model < valid_args[i].sm_min
                    || runner->minimum_shader_model > valid_args[i].sm_max)
                holds = false;

            break;
        }

        while (isspace(*line))
            ++line;

        if (match && *line == '&')
        {
            ++line;
        }
        else if (*line != ')' && *line != '|')
        {
            fatal_error("Invalid qualifier argument '%s'.\n", line);
        }
    }

    assert(*line == ')' || *line == '|');
    if (rest)
        *rest = line;

    return holds;
}

static bool check_qualifier_args(struct shader_runner *runner, const char *line, const char **const rest)
{
    bool first = true;
    bool holds = false;

    assert(*line == '(');
    ++line;

    while (*line != ')')
    {
        if (!first && *line == '|')
            ++line;
        first = false;

        holds = check_qualifier_args_conjunction(runner, line, &line) || holds;
    }

    assert(*line == ')');
    if (rest)
        *rest = line + 1;

    return holds;
}

static bool match_string_generic(struct shader_runner *runner, const char *line, const char *token,
        const char **const rest, bool exclude_bracket, bool allow_qualifier_args)
{
    size_t len = strlen(token);
    bool holds = true;

    while (isspace(*line) || (exclude_bracket && *line == ']'))
        ++line;

    if (strncmp(line, token, len) || !(isspace(line[len]) || line[len] == '('
            || (exclude_bracket && line[len] == ']')))
        return false;
    line += len;

    if (allow_qualifier_args && *line == '(')
        holds = check_qualifier_args(runner, line, &line);

    if (rest)
    {
        *rest = line;
        while (isspace(**rest))
            ++*rest;
    }
    return holds;
}

static bool match_string_with_args(struct shader_runner *runner,
        const char *line, const char *token, const char **const rest)
{
    return match_string_generic(runner, line, token, rest, false, true);
}

static bool match_string(const char *line, const char *token, const char **const rest)
{
    return match_string_generic(NULL, line, token, rest, false, false);
}

static bool match_directive_substring_with_args(struct shader_runner *runner,
        const char *line, const char *token, const char **const rest)
{
    return match_string_generic(runner, line, token, rest, true, true);
}

static bool match_directive_substring(const char *line, const char *token, const char **const rest)
{
    return match_string_generic(NULL, line, token, rest, true, false);
}

static const char *close_parentheses(const char *line)
{
    while (isspace(*line))
        ++line;

    if (*line != ')')
        fatal_error("Malformed probe arguments '%s'.\n", line);

    return line;
}

static void parse_require_directive(struct shader_runner *runner, const char *line)
{
    bool less_than = false;
    unsigned int i;

    if (match_string(line, "shader model >=", &line)
            || (less_than = match_string(line, "shader model <", &line)))
    {
        for (i = 0; i < ARRAY_SIZE(model_strings); ++i)
        {
            if (match_string(line, model_strings[i], &line))
            {
                if (less_than)
                {
                    if (!i)
                        fatal_error("Shader model < '%s' is invalid.\n", line);
                    runner->maximum_shader_model = min(runner->maximum_shader_model, i - 1);
                }
                else
                {
                    runner->minimum_shader_model = max(runner->minimum_shader_model, i);
                }
                return;
            }
        }

        fatal_error("Unknown shader model '%s'.\n", line);
    }
    else if (match_string(line, "options:", &line))
    {
        static const struct option
        {
            unsigned int option;
            const char *name;
        }
        options[] =
        {
            { 0, "none" },
            { D3DCOMPILE_PACK_MATRIX_ROW_MAJOR, "row-major" },
            { D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR, "column-major" },
            { D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY, "backcompat" },
            { D3DCOMPILE_ENABLE_UNBOUNDED_DESCRIPTOR_TABLES, "unbounded-descriptor-arrays" },
        };

        runner->compile_options = 0;
        for (i = 0; i < ARRAY_SIZE(options); ++i)
        {
            if (match_string(line, options[i].name, &line))
                runner->compile_options |= options[i].option;
        }
    }
    else if (match_string(line, "float64", &line))
    {
        runner->require_float64 = true;
    }
    else if (match_string(line, "int64", &line))
    {
        runner->require_int64 = true;
    }
    else if (match_string(line, "rov", &line))
    {
        runner->require_rov = true;
    }
    else
    {
        fatal_error("Unknown require directive '%s'.\n", line);
    }
}

static DXGI_FORMAT parse_format(const char *line, enum texture_data_type *data_type, unsigned int *texel_size,
        bool *is_shadow, const char **rest)
{
    static const struct
    {
        const char *string;
        enum texture_data_type data_type;
        unsigned int texel_size;
        DXGI_FORMAT format;
        bool is_shadow;
    }
    formats[] =
    {
        {"r32g32b32a32 float",  TEXTURE_DATA_FLOAT, 16, DXGI_FORMAT_R32G32B32A32_FLOAT},
        {"r32g32b32a32 sint",   TEXTURE_DATA_SINT,  16, DXGI_FORMAT_R32G32B32A32_SINT},
        {"r32g32b32a32 uint",   TEXTURE_DATA_UINT,  16, DXGI_FORMAT_R32G32B32A32_UINT},
        {"r32g32 float",        TEXTURE_DATA_FLOAT,  8, DXGI_FORMAT_R32G32_FLOAT},
        {"r32g32 int",          TEXTURE_DATA_SINT,   8, DXGI_FORMAT_R32G32_SINT},
        {"r32g32 uint",         TEXTURE_DATA_UINT,   8, DXGI_FORMAT_R32G32_UINT},
        {"r32 float shadow",    TEXTURE_DATA_FLOAT,  4, DXGI_FORMAT_R32_FLOAT, true},
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
            if (is_shadow)
                *is_shadow = formats[i].is_shadow;
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
                if (sampler->func)
                    sampler->filter |= D3D12_FILTER_REDUCTION_TYPE_COMPARISON << D3D12_FILTER_REDUCTION_TYPE_SHIFT;
                return;
            }
        }

        fatal_error("Unknown sampler filter '%s'.\n", line);
    }
    else if (match_string(line, "comparison", &line))
    {
        static const struct
        {
            const char *string;
            D3D12_COMPARISON_FUNC func;
        }
        funcs[] =
        {
            {"less equal", D3D12_COMPARISON_FUNC_LESS_EQUAL},
            {"not equal", D3D12_COMPARISON_FUNC_NOT_EQUAL},
            {"greater equal", D3D12_COMPARISON_FUNC_GREATER_EQUAL},
            {"never", D3D12_COMPARISON_FUNC_NEVER},
            {"less", D3D12_COMPARISON_FUNC_LESS},
            {"equal", D3D12_COMPARISON_FUNC_EQUAL},
            {"greater", D3D12_COMPARISON_FUNC_GREATER},
            {"always", D3D12_COMPARISON_FUNC_ALWAYS},
        };
        unsigned int i;

        for (i = 0; i < ARRAY_SIZE(funcs); ++i)
        {
            if (match_string(line, funcs[i].string, &line))
            {
                sampler->filter |= D3D12_FILTER_REDUCTION_TYPE_COMPARISON << D3D12_FILTER_REDUCTION_TYPE_SHIFT;
                sampler->func = funcs[i].func;
                return;
            }
        }

        fatal_error("Unknown sampler func '%s'.\n", line);
    }
    else
    {
        fatal_error("Unknown sampler directive '%s'.\n", line);
    }
}

static void parse_resource_directive(struct resource_params *resource, const char *line)
{
    if (match_string(line, "format", &line))
    {
        resource->format = parse_format(line, &resource->data_type, &resource->texel_size, &resource->is_shadow, &line);
    }
    else if (match_string(line, "stride", &line))
    {
        if (sscanf(line, "%u", &resource->stride) < 1)
            fatal_error("Malformed texture stride '%s'.\n", line);
        resource->texel_size = resource->stride;
        resource->format = DXGI_FORMAT_UNKNOWN;
    }
    else if (match_string(line, "size", &line))
    {
        if (sscanf(line, "( buffer , %u ) ", &resource->width) == 1)
        {
            resource->dimension = RESOURCE_DIMENSION_BUFFER;
            resource->height = 1;
        }
        else if (sscanf(line, "( counter_buffer , %u ) ", &resource->width) == 1)
        {
            resource->dimension = RESOURCE_DIMENSION_BUFFER;
            resource->height = 1;
            resource->is_uav_counter = true;
            resource->stride = sizeof(uint32_t);
            resource->texel_size = resource->stride;
            resource->format = DXGI_FORMAT_UNKNOWN;
        }
        else if (sscanf(line, "( 2d , %u , %u ) ", &resource->width, &resource->height) == 2)
        {
            resource->dimension = RESOURCE_DIMENSION_2D;
        }
        else
        {
            fatal_error("Malformed resource size '%s'.\n", line);
        }
    }
    else if (match_string(line, "levels", &line))
    {
        char *rest;

        resource->level_count = strtoul(line, &rest, 10);
        if (rest == line)
            fatal_error("Malformed texture directive '%s'.\n", line);
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
                    u.i = strtol(line, &rest, 0);
                    break;

                case TEXTURE_DATA_UINT:
                    u.u = strtoul(line, &rest, 0);
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

    element->format = parse_format(line, NULL, &element->texel_size, NULL, &line);

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
    resource->dimension = params->dimension;
    resource->slot = params->slot;
    resource->format = params->format;
    resource->size = params->data_size;
    resource->texel_size = params->texel_size;
    resource->width = params->width;
    resource->height = params->height;
}

struct resource *shader_runner_get_resource(struct shader_runner *runner, enum resource_type type, unsigned int slot)
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
    size_t initial_count = runner->uniform_count;

    runner->uniform_count = align(max(runner->uniform_count, offset + count), 4);
    vkd3d_array_reserve((void **)&runner->uniforms, &runner->uniform_capacity,
            runner->uniform_count, sizeof(*runner->uniforms));
    memset(runner->uniforms + initial_count, 127,
            (runner->uniform_count - initial_count) * sizeof(*runner->uniforms));
    memcpy(runner->uniforms + offset, uniforms, count * sizeof(*runner->uniforms));
}

static void read_int(const char **line, int *i, bool is_uniform)
{
    char *rest;
    long val;

    errno = 0;
    val = strtol(*line, &rest, 0);

    if (errno != 0 || (is_uniform && *rest != '\0' && !isspace((unsigned char)*rest)))
        fatal_error("Malformed int constant '%s'.\n", *line);

    *i = val;
    if (*i != val)
        fatal_error("Out of range int constant '%.*s'.\n", (int)(rest - *line), *line);

    *line = rest + (!is_uniform && *rest == ',');
}

static void read_uint(const char **line, unsigned int *u, bool is_uniform)
{
    char *rest;
    unsigned long val;

    errno = 0;
    val = strtoul(*line, &rest, 0);

    if (errno != 0 || (is_uniform && *rest != '\0' && !isspace((unsigned char)*rest)))
        fatal_error("Malformed uint constant '%s'.\n", *line);

    *u = val;
    if (*u != val)
        fatal_error("Out of range uint constant '%.*s'.\n", (int)(rest - *line), *line);

    *line = rest + (!is_uniform && *rest == ',');
}

static void read_int4(const char **line, struct ivec4 *v, bool is_uniform)
{
    read_int(line, &v->x, is_uniform);
    read_int(line, &v->y, is_uniform);
    read_int(line, &v->z, is_uniform);
    read_int(line, &v->w, is_uniform);
}

static void read_uint4(const char **line, struct uvec4 *v, bool is_uniform)
{
    read_uint(line, &v->x, is_uniform);
    read_uint(line, &v->y, is_uniform);
    read_uint(line, &v->z, is_uniform);
    read_uint(line, &v->w, is_uniform);
}

static void read_int64(const char **line, int64_t *i, bool is_uniform)
{
    char *rest;
    int64_t val;

    errno = 0;
    val = strtoll(*line, &rest, 0);

    if (errno != 0 || (is_uniform && *rest != '\0' && !isspace((unsigned char)*rest)))
        fatal_error("Malformed int64 constant '%s'.\n", *line);

    *i = val;
    *line = rest + (!is_uniform && *rest == ',');
}

static void read_uint64(const char **line, uint64_t *u, bool is_uniform)
{
    char *rest;
    uint64_t val;

    errno = 0;
    val = strtoull(*line, &rest, 0);

    if (errno != 0 || (is_uniform && *rest != '\0' && !isspace((unsigned char)*rest)))
        fatal_error("Malformed uint64 constant '%s'.\n", *line);

    *u = val;
    *line = rest + (!is_uniform && *rest == ',');
}

static void read_int64_t2(const char **line, struct i64vec2 *v)
{
    read_int64(line, &v->x, true);
    read_int64(line, &v->y, true);
}

static void read_uint64_t2(const char **line, struct u64vec2 *v)
{
    read_uint64(line, &v->x, true);
    read_uint64(line, &v->y, true);
}

static void parse_test_directive(struct shader_runner *runner, const char *line)
{
    bool skip_directive = false;
    const char *line_ini;
    bool match = true;
    char *rest;
    int ret;

    runner->is_todo = false;

    while (match)
    {
        match = false;

        if (match_string_with_args(runner, line, "todo", &line))
        {
            runner->is_todo = true;
            match = true;
        }

        line_ini = line;
        if (match_string_with_args(runner, line, "if", &line))
        {
            match = true;
        }
        else if (line != line_ini)
        {
            /* Matched "if" but for other shader models. */
            skip_directive = true;
            match = true;
        }
    }

    if (skip_directive)
    {
        const char *new_line;

        if ((new_line = strchr(line, '\n')))
            line = new_line + 1;
        else
            line += strlen(line);
        return;
    }

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
        unsigned int i;

        /* For simplicity, draw a large triangle instead. */
        static const struct vec2 quad[] =
        {
            {-2.0f, -2.0f},
            {-2.0f,  4.0f},
            { 4.0f, -2.0f},
        };

        static const char vs_source[] =
            "float4 main(float4 pos : position) : sv_position\n"
            "{\n"
            "    return pos;\n"
            "}";

        if (!shader_runner_get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0))
        {
            memset(&params, 0, sizeof(params));
            params.slot = 0;
            params.type = RESOURCE_TYPE_RENDER_TARGET;
            params.dimension = RESOURCE_DIMENSION_2D;
            params.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
            params.data_type = TEXTURE_DATA_FLOAT;
            params.texel_size = 16;
            params.width = RENDER_TARGET_WIDTH;
            params.height = RENDER_TARGET_HEIGHT;
            params.level_count = 1;

            set_resource(runner, runner->ops->create_resource(runner, &params));
        }

        for (i = 0; i < runner->input_element_count; ++i)
            free(runner->input_elements[i].name);

        vkd3d_array_reserve((void **)&runner->input_elements, &runner->input_element_capacity,
                1, sizeof(*runner->input_elements));
        element = &runner->input_elements[0];
        element->name = strdup("position");
        element->slot = 0;
        element->format = DXGI_FORMAT_R32G32_FLOAT;
        element->texel_size = sizeof(*quad);
        element->index = 0;
        runner->input_element_count = 1;

        memset(&params, 0, sizeof(params));
        params.slot = 0;
        params.type = RESOURCE_TYPE_VERTEX_BUFFER;
        params.dimension = RESOURCE_DIMENSION_BUFFER;
        params.data = malloc(sizeof(quad));
        memcpy(params.data, quad, sizeof(quad));
        params.data_size = sizeof(quad);
        set_resource(runner, runner->ops->create_resource(runner, &params));
        free(params.data);

        if (!runner->vs_source)
            runner->vs_source = strdup(vs_source);

        runner->last_render_failed = !runner->ops->draw(runner, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST, 3);
    }
    else if (match_string(line, "draw", &line))
    {
        D3D_PRIMITIVE_TOPOLOGY topology;
        struct resource_params params;
        unsigned int vertex_count;

        if (!shader_runner_get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0))
        {
            memset(&params, 0, sizeof(params));
            params.slot = 0;
            params.type = RESOURCE_TYPE_RENDER_TARGET;
            params.dimension = RESOURCE_DIMENSION_2D;
            params.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
            params.data_type = TEXTURE_DATA_FLOAT;
            params.texel_size = 16;
            params.width = RENDER_TARGET_WIDTH;
            params.height = RENDER_TARGET_HEIGHT;
            params.level_count = 1;

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
        bool is_signed = false;
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

            resource = shader_runner_get_resource(runner, RESOURCE_TYPE_UAV, slot);
        }
        else if (match_string(line, "rtv", &line))
        {
            slot = strtoul(line, &rest, 10);

            if (rest == line)
                fatal_error("Malformed render target index '%s'.\n", line);
            line = rest;

            resource = shader_runner_get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, slot);
        }
        else
        {
            resource = shader_runner_get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0);
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
        else if (sscanf(line, " ( %u )%n", &left, &len) == 1)
        {
            set_rect(&rect, left, 0, left + 1, 1);
            line += len;
        }
        else
        {
            fatal_error("Malformed probe arguments '%s'.\n", line);
        }

        if (match_string(line, "rgbaui", &line))
        {
            struct uvec4 v;

            if (*line != '(')
                fatal_error("Malformed probe arguments '%s'.\n", line);
            ++line;
            read_uint4(&line, &v, false);
            line = close_parentheses(line);
            todo_if(runner->is_todo) check_readback_data_uvec4(rb, &rect, &v);
        }
        else if (match_string(line, "rgbai", &line))
        {
            struct ivec4 v;

            if (*line != '(')
                fatal_error("Malformed probe arguments '%s'.\n", line);
            ++line;
            read_int4(&line, &v, false);
            line = close_parentheses(line);
            todo_if(runner->is_todo) check_readback_data_ivec4(rb, &rect, &v);
        }
        else if (match_string(line, "rgba", &line))
        {
            struct vec4 v;

            ret = sscanf(line, "( %f , %f , %f , %f ) %u", &v.x, &v.y, &v.z, &v.w, &ulps);
            if (ret < 4)
                fatal_error("Malformed probe arguments '%s'.\n", line);
            if (ret < 5)
                ulps = 0;
            todo_if(runner->is_todo) check_readback_data_vec4(rb, &rect, &v, ulps);
        }
        else if (match_string(line, "rui", &line) || (is_signed = match_string(line, "ri", &line)))
        {
            unsigned int expect;
            D3D12_BOX box;

            box.left = rect.left;
            box.right = rect.right;
            box.top = rect.top;
            box.bottom = rect.bottom;
            box.front = 0;
            box.back = 1;
            if (*line != '(')
                fatal_error("Malformed probe arguments '%s'.\n", line);
            ++line;
            if (is_signed)
                read_int(&line, (int *)&expect, false);
            else
                read_uint(&line, &expect, false);
            line = close_parentheses(line);
            todo_if(runner->is_todo) check_readback_data_uint(rb, &box, expect, 0);
        }
        else if (match_string(line, "rui64", &line) || (is_signed = match_string(line, "ri64", &line)))
        {
            uint64_t expect;
            D3D12_BOX box;

            box.left = rect.left;
            box.right = rect.right;
            box.top = rect.top;
            box.bottom = rect.bottom;
            box.front = 0;
            box.back = 1;
            if (*line != '(')
                fatal_error("Malformed probe arguments '%s'.\n", line);
            ++line;
            if (is_signed)
                read_int64(&line, (int64_t *)&expect, false);
            else
                read_uint64(&line, &expect, false);
            line = close_parentheses(line);
            todo_if(runner->is_todo) check_readback_data_uint64(rb, &box, expect, 0);
        }
        else if (match_string(line, "rd", &line))
        {
            double expect;

            ret = sscanf(line, "( %lf ) %u", &expect, &ulps);
            if (ret < 1)
                fatal_error("Malformed probe arguments '%s'.\n", line);
            if (ret < 2)
                ulps = 0;
            todo_if(runner->is_todo) check_readback_data_double(rb, &rect, expect, ulps);
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
        else if (match_string(line, "double2", &line))
        {
            struct dvec2 v;

            if (sscanf(line, "%lf %lf", &v.x, &v.y) < 2)
                fatal_error("Malformed double2 constant '%s'.\n", line);
            set_uniforms(runner, offset, 4, &v);
        }
        else if (match_string(line, "int4", &line))
        {
            struct ivec4 v;

            read_int4(&line, &v, true);
            set_uniforms(runner, offset, 4, &v);
        }
        else if (match_string(line, "uint4", &line))
        {
            struct uvec4 v;

            read_uint4(&line, &v, true);
            set_uniforms(runner, offset, 4, &v);
        }
        else if (match_string(line, "int", &line))
        {
            int i;

            read_int(&line, &i, true);
            set_uniforms(runner, offset, 1, &i);
        }
        else if (match_string(line, "uint", &line))
        {
            unsigned int u;

            read_uint(&line, &u, true);
            set_uniforms(runner, offset, 1, &u);
        }
        else if (match_string(line, "int64_t2", &line))
        {
            struct i64vec2 v;

            read_int64_t2(&line, &v);
            set_uniforms(runner, offset, 4, &v);
        }
        else if (match_string(line, "uint64_t2", &line))
        {
            struct u64vec2 v;

            read_uint64_t2(&line, &v);
            set_uniforms(runner, offset, 4, &v);
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

struct sampler *shader_runner_get_sampler(struct shader_runner *runner, unsigned int slot)
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

static HRESULT map_special_hrs(HRESULT hr)
{
    if (hr == 0x88760b59)
    {
        trace("Mapping hr %#x (D3DXERR_INVALIDDATA) as %#x.\n", hr, E_FAIL);
        return E_FAIL;
    }
    if (hr == 0x80010064)
    {
        trace("Mapping unidentified hr %#x as %#x.\n", hr, E_FAIL);
        return E_FAIL;
    }
    return hr;
}

const char *shader_type_string(enum shader_type type)
{
    static const char *const shader_types[] =
    {
        [SHADER_TYPE_CS] = "cs",
        [SHADER_TYPE_PS] = "ps",
        [SHADER_TYPE_VS] = "vs",
        [SHADER_TYPE_FX] = "fx",
    };
    assert(type < ARRAY_SIZE(shader_types));
    return shader_types[type];
}

/* Avoid issues with calling convention mismatch and different methods for string
 * retrieval by copying all IDxcBlob objects to a new ID3D10Blob. */

static void d3d10_blob_from_dxc_blob_utf8(IDxcBlobUtf8 *blob, ID3D10Blob **blob_out)
{
    ID3D10Blob *d3d_blob;
    size_t size;
    HRESULT hr;

    size = IDxcBlobUtf8_GetStringLength(blob) + 1;
    if (FAILED(hr = D3DCreateBlob(size, (ID3DBlob **)&d3d_blob)))
    {
        trace("Failed to create blob, hr %#x.\n", hr);
        return;
    }

    memcpy(ID3D10Blob_GetBufferPointer(d3d_blob), IDxcBlobUtf8_GetStringPointer(blob), size);
    *blob_out = d3d_blob;
}

static HRESULT d3d10_blob_from_dxc_blob(IDxcBlob *blob, ID3D10Blob **blob_out)
{
    ID3D10Blob *d3d_blob;
    size_t size;
    HRESULT hr;

    size = IDxcBlob_GetBufferSize(blob);
    if (FAILED(hr = D3DCreateBlob(size, (ID3DBlob **)&d3d_blob)))
    {
        trace("Failed to create blob, hr %#x.\n", hr);
        return hr;
    }

    memcpy(ID3D10Blob_GetBufferPointer(d3d_blob), IDxcBlob_GetBufferPointer(blob), size);
    *blob_out = d3d_blob;

    return S_OK;
}

HRESULT dxc_compiler_compile_shader(void *dxc_compiler, enum shader_type type, unsigned int compile_options,
        const char *hlsl, ID3D10Blob **blob_out, ID3D10Blob **errors_out)
{
    DxcBuffer src_buf = {hlsl, strlen(hlsl), 65001};
    IDxcCompiler3 *compiler = dxc_compiler;
    HRESULT hr, compile_hr;
    IDxcBlobUtf8 *errors;
    IDxcResult *result;
    size_t arg_count;
    IDxcBlob *blob;

    static const WCHAR *const shader_profiles[] =
    {
        [SHADER_TYPE_CS] = L"cs_6_0",
        [SHADER_TYPE_PS] = L"ps_6_0",
        [SHADER_TYPE_VS] = L"vs_6_0",
    };
    const WCHAR *args[] =
    {
        L"/T",
        shader_profiles[type],
        L"/Qstrip_reflect",
        L"/Qstrip_debug",
        L"/flegacy-macro-expansion",
        L"/flegacy-resource-reservation",
        NULL,
        NULL,
        NULL,
    };

    *blob_out = NULL;
    *errors_out = NULL;

    arg_count = ARRAY_SIZE(args) - 3;
    if (compile_options & D3DCOMPILE_PACK_MATRIX_ROW_MAJOR)
        args[arg_count++] = L"/Zpr";
    if (compile_options & D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR)
        args[arg_count++] = L"/Zpc";
    if (compile_options & D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY)
        args[arg_count++] = L"/Gec";

    if (FAILED(hr = IDxcCompiler3_Compile(compiler, &src_buf, args, arg_count, NULL, &IID_IDxcResult, (void **)&result)))
    {
        trace("Failed to compile shader, hr %#x.\n", hr);
        return hr;
    }

    if (IDxcResult_HasOutput(result, DXC_OUT_ERRORS)
            && SUCCEEDED(hr = IDxcResult_GetOutput(result, DXC_OUT_ERRORS, &IID_IDxcBlobUtf8, (void **)&errors, NULL)))
    {
        if (IDxcBlobUtf8_GetStringLength(errors))
            d3d10_blob_from_dxc_blob_utf8(errors, errors_out);
        IDxcBlobUtf8_Release(errors);
    }

    if (FAILED(hr = IDxcResult_GetStatus(result, &compile_hr)) || FAILED((hr = compile_hr)))
    {
        if (hr == DXC_E_LLVM_CAST_ERROR)
            hr = E_FAIL;
        goto result_release;
    }

    if (FAILED(hr = IDxcResult_GetOutput(result, DXC_OUT_OBJECT, &IID_IDxcBlob, (void **)&blob, NULL)))
        goto result_release;

    IDxcResult_Release(result);

    hr = d3d10_blob_from_dxc_blob(blob, blob_out);
    IDxcBlob_Release(blob);
    return hr;

result_release:
    IDxcResult_Release(result);
    return hr;
}

static void compile_shader(struct shader_runner *runner, IDxcCompiler3 *dxc_compiler, const char *source, size_t len,
        enum shader_type type, HRESULT expect)
{
    bool use_dxcompiler = runner->minimum_shader_model >= SHADER_MODEL_6_0;
    ID3D10Blob *blob = NULL, *errors = NULL;
    char profile[7];
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "2_0",
        [SHADER_MODEL_3_0] = "3_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
        [SHADER_MODEL_6_0] = "6_0",
    };

    static const char *const effect_models[] =
    {
        [SHADER_MODEL_2_0] = "2_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
    };

    if (use_dxcompiler)
    {
        assert(dxc_compiler);
        hr = dxc_compiler_compile_shader(dxc_compiler, type, runner->compile_options, source, &blob, &errors);
    }
    else
    {
        if (type == SHADER_TYPE_FX)
            sprintf(profile, "%s_%s", shader_type_string(type), effect_models[runner->minimum_shader_model]);
        else
            sprintf(profile, "%s_%s", shader_type_string(type), shader_models[runner->minimum_shader_model]);
        hr = D3DCompile(source, len, NULL, NULL, NULL, "main", profile, runner->compile_options, 0, &blob, &errors);
    }
    hr = map_special_hrs(hr);
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

static enum parse_state get_parse_state_todo(enum parse_state state)
{
    if (state == STATE_SHADER_COMPUTE)
        return STATE_SHADER_COMPUTE_TODO;
    else if (state == STATE_SHADER_PIXEL)
        return STATE_SHADER_PIXEL_TODO;
    else if (state == STATE_SHADER_VERTEX)
        return STATE_SHADER_VERTEX_TODO;
    else
        return STATE_SHADER_EFFECT_TODO;
}

static enum parse_state read_shader_directive(struct shader_runner *runner, enum parse_state state, const char *line,
        const char *src, HRESULT *expect_hr)
{
    while (*src && *src != ']')
    {
        const char *src_start = src;

        if (match_directive_substring_with_args(runner, src, "todo", &src))
        {
            /* 'todo' is not meaningful when dxcompiler is in use. */
            if (runner->minimum_shader_model >= SHADER_MODEL_6_0)
                continue;
            state = get_parse_state_todo(state);
        }
        else if (match_directive_substring_with_args(runner, src, "fail", &src))
        {
            *expect_hr = E_FAIL;
        }
        else if (match_directive_substring_with_args(runner, src, "notimpl", &src))
        {
            *expect_hr = E_NOTIMPL;
        }

        if (src == src_start)
        {
            fatal_error("Malformed line '%s'.\n", line);
        }
    }

    if (strcmp(src, "]\n"))
        fatal_error("Malformed line '%s'.\n", line);

    return state;
}

static bool check_requirements(const struct shader_runner *runner, const struct shader_runner_caps *caps)
{
    if (runner->maximum_shader_model < runner->minimum_shader_model)
        return false;
    if (runner->require_float64 && !caps->float64)
        return false;
    if (runner->require_int64 && !caps->int64)
        return false;
    if (runner->require_rov && !caps->rov)
        return false;

    return true;
}

static void trace_tags(const struct shader_runner_caps *caps)
{
    char tags[80], *p;
    size_t rem;
    int rc;

    p = tags;
    rem = ARRAY_SIZE(tags);
    rc = snprintf(p, rem, "      tags:");
    p += rc;
    rem -= rc;

    for (size_t i = 0; i < caps->tag_count; ++i)
    {
        rc = snprintf(p, rem, " \"%s\"%s", caps->tags[i], i == caps->tag_count - 1 ? "" : ",");
        if (!(rc >= 0 && (size_t)rc < rem))
        {
            *p = 0;
            trace("%s\n", tags);

            p = tags;
            rem = ARRAY_SIZE(tags);
            rc = snprintf(p, rem, "           ");
            --i;
        }
        p += rc;
        rem -= rc;
    }
    trace("%s.\n", tags);
}

void run_shader_tests(struct shader_runner *runner, const struct shader_runner_caps *caps,
        const struct shader_runner_ops *ops, void *dxc_compiler)
{
    size_t shader_source_size = 0, shader_source_len = 0;
    struct resource_params current_resource;
    struct sampler *current_sampler = NULL;
    enum parse_state state = STATE_NONE;
    unsigned int i, line_number = 0;
    char *shader_source = NULL;
    HRESULT expect_hr = S_OK;
    bool skip_tests = false;
    char line_buffer[256];
    const char *testname;
    FILE *f;

    trace("Compiling SM%s-SM%s shaders with %s and executing with %s.\n",
            model_strings[caps->minimum_shader_model], model_strings[caps->maximum_shader_model],
            dxc_compiler ? "dxcompiler" : HLSL_COMPILER, caps->runner);
    if (caps->tag_count)
        trace_tags(caps);
    trace("   float64: %u.\n", caps->float64);
    trace("     int64: %u.\n", caps->int64);
    trace("       rov: %u.\n", caps->rov);

    if (!test_options.filename)
        fatal_error("No filename specified.\n");

    if (!(f = fopen(test_options.filename, "r")))
        fatal_error("Unable to open '%s' for reading: %s\n", test_options.filename, strerror(errno));

    memset(runner, 0, sizeof(*runner));
    runner->ops = ops;
    runner->caps = caps;
    runner->minimum_shader_model = caps->minimum_shader_model;
    runner->maximum_shader_model = caps->maximum_shader_model;

    if ((testname = strrchr(test_options.filename, '/')))
        ++testname;
    else
        testname = test_options.filename;

    for (;;)
    {
        char *ret = fgets(line_buffer, sizeof(line_buffer), f);
        const char *line = line_buffer;

        if (line_number++)
            vkd3d_test_pop_context();
        vkd3d_test_push_context("%s:%u", testname, line_number);

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
                    if (!check_requirements(runner, caps))
                        skip_tests = true;
                    break;

                case STATE_RESOURCE:
                    /* Not every backend supports every resource type
                     * (specifically, D3D9 doesn't support UAVs and
                     * textures with data type other than float). */
                    if (!skip_tests)
                    {
                        set_resource(runner, runner->ops->create_resource(runner, &current_resource));
                    }
                    free(current_resource.data);
                    break;

                case STATE_SHADER_COMPUTE:
                case STATE_SHADER_COMPUTE_TODO:
                    if (!skip_tests)
                    {
                        todo_if (state == STATE_SHADER_COMPUTE_TODO)
                        compile_shader(runner, dxc_compiler, shader_source, shader_source_len, SHADER_TYPE_CS,
                                expect_hr);
                    }
                    free(runner->cs_source);
                    runner->cs_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_SHADER_PIXEL:
                case STATE_SHADER_PIXEL_TODO:
                    if (!skip_tests)
                    {
                        todo_if (state == STATE_SHADER_PIXEL_TODO)
                        compile_shader(runner, dxc_compiler, shader_source, shader_source_len, SHADER_TYPE_PS,
                                expect_hr);
                    }
                    free(runner->ps_source);
                    runner->ps_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_SHADER_VERTEX:
                case STATE_SHADER_VERTEX_TODO:
                    if (!skip_tests)
                    {
                        todo_if (state == STATE_SHADER_VERTEX_TODO)
                        compile_shader(runner, dxc_compiler, shader_source, shader_source_len, SHADER_TYPE_VS,
                                expect_hr);
                    }
                    free(runner->vs_source);
                    runner->vs_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_SHADER_EFFECT:
                case STATE_SHADER_EFFECT_TODO:
                    if (!skip_tests)
                    {
                        todo_if (state == STATE_SHADER_EFFECT_TODO)
                        compile_shader(runner, dxc_compiler, shader_source, shader_source_len, SHADER_TYPE_FX,
                                expect_hr);
                    }
                    free(runner->fx_source);
                    runner->fx_source = shader_source;
                    shader_source = NULL;
                    shader_source_len = 0;
                    shader_source_size = 0;
                    break;

                case STATE_PREPROC_INVALID:
                {
                    ID3D10Blob *blob = NULL, *errors = NULL;
                    HRESULT hr;

                    if (skip_tests)
                        break;

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

                    if (skip_tests)
                        break;

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

            if (!ret)
                break;
        }

        if (line[0] == '[')
        {
            unsigned int index;

            if (match_directive_substring(line, "[compute shader", &line))
            {
                state = STATE_SHADER_COMPUTE;
                expect_hr = S_OK;
                state = read_shader_directive(runner, state, line_buffer, line, &expect_hr);
            }
            else if (!strcmp(line, "[require]\n"))
            {
                state = STATE_REQUIRE;
                runner->minimum_shader_model = caps->minimum_shader_model;
                runner->maximum_shader_model = caps->maximum_shader_model;
                runner->require_float64 = false;
                runner->require_int64 = false;
                runner->require_rov = false;
                runner->compile_options = 0;
                skip_tests = false;
            }
            else if (match_directive_substring(line, "[pixel shader", &line))
            {
                state = STATE_SHADER_PIXEL;
                expect_hr = S_OK;
                state = read_shader_directive(runner, state, line_buffer, line, &expect_hr);
            }
            else if (sscanf(line, "[sampler %u]\n", &index))
            {
                state = STATE_SAMPLER;

                if (!(current_sampler = shader_runner_get_sampler(runner, index)))
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
            else if (sscanf(line, "[rtv %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_RENDER_TARGET;
                current_resource.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
                current_resource.texel_size = 16;
                current_resource.level_count = 1;
            }
            else if (sscanf(line, "[srv %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_TEXTURE;
                current_resource.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_resource.data_type = TEXTURE_DATA_FLOAT;
                current_resource.texel_size = 16;
                current_resource.level_count = 1;
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
                current_resource.level_count = 1;
            }
            else if (sscanf(line, "[vb %u]\n", &index))
            {
                state = STATE_RESOURCE;

                memset(&current_resource, 0, sizeof(current_resource));

                current_resource.slot = index;
                current_resource.type = RESOURCE_TYPE_VERTEX_BUFFER;
                current_resource.dimension = RESOURCE_DIMENSION_BUFFER;
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
            else if (match_directive_substring(line, "[vertex shader", &line))
            {
                state = STATE_SHADER_VERTEX;
                expect_hr = S_OK;
                state = read_shader_directive(runner, state, line_buffer, line, &expect_hr);
            }
            else if (match_directive_substring(line, "[effect", &line))
            {
                state = STATE_SHADER_EFFECT;
                expect_hr = S_OK;
                state = read_shader_directive(runner, state, line_buffer, line, &expect_hr);
            }
            else if (!strcmp(line, "[input layout]\n"))
            {
                state = STATE_INPUT_LAYOUT;

                for (i = 0; i < runner->input_element_count; ++i)
                    free(runner->input_elements[i].name);
                runner->input_element_count = 0;
            }
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
                case STATE_SHADER_VERTEX_TODO:
                case STATE_SHADER_EFFECT:
                case STATE_SHADER_EFFECT_TODO:
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
                    /* Compilation which fails with dxcompiler is not 'todo', therefore the tests are
                     * not 'todo' either. They cannot run, so skip them entirely. */
                    if (!skip_tests && SUCCEEDED(expect_hr))
                        parse_test_directive(runner, line);
                    break;
            }
        }
    }

    if (line_number)
        vkd3d_test_pop_context();

    for (i = 0; i < runner->input_element_count; ++i)
        free(runner->input_elements[i].name);
    free(runner->input_elements);
    free(runner->vs_source);
    free(runner->ps_source);
    free(runner->cs_source);
    free(runner->fx_source);
    free(runner->uniforms);
    for (i = 0; i < runner->resource_count; ++i)
    {
        if (runner->resources[i])
            runner->ops->destroy_resource(runner, runner->resources[i]);
    }

    fclose(f);
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

#if defined(SONAME_LIBDXCOMPILER) && !defined(VKD3D_CROSSTEST)
static IDxcCompiler3 *dxcompiler_create(void)
{
    DxcCreateInstanceProc create_instance;
    IDxcCompiler3 *compiler;
    HRESULT hr;
    void *dll;

    dll = vkd3d_dlopen(SONAME_LIBDXCOMPILER);
    ok(dll, "Failed to load dxcompiler library, %s.\n", vkd3d_dlerror());
    if (!dll)
        return NULL;

    create_instance = (DxcCreateInstanceProc)vkd3d_dlsym(dll, "DxcCreateInstance");
    ok(create_instance, "Failed to get DxcCreateInstance() pointer.\n");
    if (!create_instance)
        return NULL;

    hr = create_instance(&CLSID_DxcCompiler, &IID_IDxcCompiler3, (void **)&compiler);
    ok(SUCCEEDED(hr), "Failed to create instance, hr %#x.\n", hr);
    if (FAILED(hr))
        return NULL;

    return compiler;
}
#elif !defined(VKD3D_CROSSTEST)
static IDxcCompiler3 *dxcompiler_create(void)
{
    return NULL;
}
#endif

START_TEST(shader_runner)
{
#ifndef VKD3D_CROSSTEST
    IDxcCompiler3 *dxc_compiler;
#endif

    parse_args(argc, argv);

#if defined(VKD3D_CROSSTEST)
    trace("Running tests from a Windows cross build\n");

    run_shader_tests_d3d9();

    run_shader_tests_d3d11();

    run_shader_tests_d3d12(NULL);

    print_dll_version("d3dcompiler_47.dll");
    print_dll_version("dxgi.dll");
    print_dll_version("d3d9.dll");
    print_dll_version("d3d11.dll");
    print_dll_version("d3d12.dll");

#elif defined(_WIN32)
    trace("Running tests from a Windows non-cross build\n");

    run_shader_tests_d3d9();

    run_shader_tests_d3d11();

    dxc_compiler = dxcompiler_create();
    run_shader_tests_d3d12(dxc_compiler);

    if (dxc_compiler)
    {
        IDxcCompiler3_Release(dxc_compiler);
        print_dll_version(SONAME_LIBDXCOMPILER);
    }
    print_dll_version("d3d9.dll");
    print_dll_version("d3d11.dll");

#else
    trace("Running tests from a Unix build\n");

# ifdef HAVE_OPENGL
    run_shader_tests_gl();
# endif

    run_shader_tests_vulkan();

    dxc_compiler = dxcompiler_create();
    run_shader_tests_d3d12(dxc_compiler);

    if (dxc_compiler)
        IDxcCompiler3_Release(dxc_compiler);

#endif
}
