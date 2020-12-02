/*
 * Copyright 2020 Zebediah Figura for CodeWeavers
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

#include "d3d12_crosstest.h"
#include <errno.h>

static bool vkd3d_array_reserve(void **elements, size_t *capacity, size_t element_count, size_t element_size)
{
    size_t new_capacity, max_capacity;
    void *new_elements;

    if (element_count <= *capacity)
        return true;

    max_capacity = ~(size_t)0 / element_size;
    if (max_capacity < element_count)
        return false;

    new_capacity = max(*capacity, 4);
    while (new_capacity < element_count && new_capacity <= max_capacity / 2)
        new_capacity *= 2;

    if (new_capacity < element_count)
        new_capacity = element_count;

    if (!(new_elements = realloc(*elements, new_capacity * element_size)))
        return false;

    *elements = new_elements;
    *capacity = new_capacity;

    return true;
}

struct shader_context
{
    struct test_context c;

    ID3D10Blob *ps_code;

    uint32_t *uniforms;
    size_t uniform_count;
};

static ID3D10Blob *compile_shader(const char *source, const char *target)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    HRESULT hr;

    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", target, 0, 0, &blob, &errors);
    ok(hr == S_OK, "Failed to compile shader, hr %#x.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

enum parse_state
{
    STATE_NONE,
    STATE_PREPROC,
    STATE_PREPROC_INVALID,
    STATE_SHADER_INVALID_PIXEL,
    STATE_SHADER_PIXEL,
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

static void parse_test_directive(struct shader_context *context, const char *line)
{
    const char *const orig_line = line;

    if (match_string(line, "draw quad", &line))
    {
        D3D12_SHADER_BYTECODE ps
                = {ID3D10Blob_GetBufferPointer(context->ps_code), ID3D10Blob_GetBufferSize(context->ps_code)};
        ID3D12GraphicsCommandList *command_list = context->c.list;
        static const float clear_color[4];
        ID3D12PipelineState *pso;

        context->c.root_signature = create_32bit_constants_root_signature(context->c.device,
                0, context->uniform_count, D3D12_SHADER_VISIBILITY_ALL);

        pso = create_pipeline_state(context->c.device, context->c.root_signature,
                context->c.render_target_desc.Format, NULL, &ps, NULL);

        ID3D12GraphicsCommandList_SetGraphicsRootSignature(command_list, context->c.root_signature);
        ID3D12GraphicsCommandList_SetGraphicsRoot32BitConstants(command_list, 0,
                context->uniform_count, context->uniforms, 0);
        ID3D12GraphicsCommandList_OMSetRenderTargets(command_list, 1, &context->c.rtv, false, NULL);
        ID3D12GraphicsCommandList_RSSetScissorRects(command_list, 1, &context->c.scissor_rect);
        ID3D12GraphicsCommandList_RSSetViewports(command_list, 1, &context->c.viewport);
        ID3D12GraphicsCommandList_IASetPrimitiveTopology(command_list, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        ID3D12GraphicsCommandList_ClearRenderTargetView(command_list, context->c.rtv, clear_color, 0, NULL);
        ID3D12GraphicsCommandList_SetPipelineState(command_list, pso);
        ID3D12GraphicsCommandList_DrawInstanced(command_list, 3, 1, 0, 0);
        ID3D12PipelineState_Release(pso);
        transition_resource_state(command_list, context->c.render_target,
                D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_COPY_SOURCE);
    }
    else if (match_string(line, "probe all rgba", &line))
    {
        unsigned int ulps;
        struct vec4 v;
        int ret;

        ret = sscanf(line, "( %f , %f , %f , %f ) %u", &v.x, &v.y, &v.z, &v.w, &ulps);
        if (ret < 4)
            goto err;
        if (ret < 5)
            ulps = 0;
        check_sub_resource_vec4(context->c.render_target, 0, context->c.queue, context->c.list, &v, ulps);
        reset_command_list(context->c.list, context->c.allocator);
    }
    else if (match_string(line, "probe rect rgba", &line))
    {
        unsigned int x, y, w, h, ulps;
        struct resource_readback rb;
        struct vec4 v;
        RECT rect;
        int ret;

        ret = sscanf(line, "( %u , %u , %u , %u ) ( %f , %f , %f , %f )", &x, &y, &w, &h, &v.x, &v.y, &v.z, &v.w);
        if (ret < 8)
            goto err;
        if (ret < 9)
            ulps = 0;

        get_texture_readback_with_command_list(context->c.render_target, 0, &rb, context->c.queue, context->c.list);
        rect.left = x;
        rect.right = x + w;
        rect.top = y;
        rect.bottom = y + h;
        check_readback_data_vec4(&rb, &rect, &v, ulps);
        release_resource_readback(&rb);
        reset_command_list(context->c.list, context->c.allocator);
    }
    else if (match_string(line, "probe rgba", &line))
    {
        struct resource_readback rb;
        unsigned int x, y, ulps;
        struct vec4 v;
        RECT rect;
        int ret;

        ret = sscanf(line, "( %u , %u ) ( %f , %f , %f , %f ) %u", &x, &y, &v.x, &v.y, &v.z, &v.w, &ulps);
        if (ret < 6)
            goto err;
        if (ret < 7)
            ulps = 0;

        get_texture_readback_with_command_list(context->c.render_target, 0, &rb, context->c.queue, context->c.list);
        rect.left = x;
        rect.right = x + 1;
        rect.top = y;
        rect.bottom = y + 1;
        check_readback_data_vec4(&rb, &rect, &v, ulps);
        release_resource_readback(&rb);
        reset_command_list(context->c.list, context->c.allocator);
    }
    else if (match_string(line, "uniform", &line))
    {
        unsigned int offset;

        if (!sscanf(line, "%u", &offset))
            goto err;
        line = strchr(line, ' ') + 1;

        if (match_string(line, "float4", &line))
        {
            struct vec4 v;

            if (sscanf(line, "%f %f %f %f", &v.x, &v.y, &v.z, &v.w) < 4)
                goto err;
            if (offset + 4 > context->uniform_count)
            {
                context->uniform_count = offset + 4;
                context->uniforms = realloc(context->uniforms, context->uniform_count * sizeof(*context->uniforms));
            }
            memcpy(context->uniforms + offset, &v, sizeof(v));
        }
        else if (match_string(line, "uint", &line))
        {
            unsigned int u;

            sscanf(line, "%u", &u);
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
        goto err;
    }

    return;

err:
    fprintf(stderr, "Ignoring malformed line '%s'.\n", orig_line);
}

START_TEST(shader_runner_d3d12)
{
    static const struct test_context_desc desc =
    {
        .rt_width = 640,
        .rt_height = 480,
        .no_root_signature = true,
        .rt_format = DXGI_FORMAT_R32G32B32A32_FLOAT,
    };
    size_t shader_source_size = 0, shader_source_len = 0;
    enum parse_state state = STATE_NONE;
    unsigned int i, line_number = 0;
    struct shader_context context;
    const char *filename = NULL;
    char *shader_source = NULL;
    char line[256];
    FILE *f;

    parse_args(argc, argv);
    enable_d3d12_debug_layer(argc, argv);
    init_adapter_info();

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
        fprintf(stderr, "Unable to open '%s' for reading: %s\n", argv[1], strerror(errno));
        return;
    }

    memset(&context, 0, sizeof(context));
    init_test_context(&context.c, &desc);

    for (;;)
    {
        char *ret = fgets(line, sizeof(line), f);

        ++line_number;

        if (!ret || line[0] == '[')
        {
            switch (state)
            {
                case STATE_NONE:
                case STATE_TEST:
                    break;

                case STATE_SHADER_PIXEL:
                    if (!(context.ps_code = compile_shader(shader_source, "ps_4_0")))
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
            if (!strcmp(line, "[pixel shader]\n"))
                state = STATE_SHADER_PIXEL;
            else if (!strcmp(line, "[pixel shader fail]\n"))
                state = STATE_SHADER_INVALID_PIXEL;
            else if (!strcmp(line, "[test]\n"))
                state = STATE_TEST;
            else if (!strcmp(line, "[preproc]\n"))
                state = STATE_PREPROC;
            else if (!strcmp(line, "[preproc fail]\n"))
                state = STATE_PREPROC_INVALID;

            vkd3d_test_set_context("Section %.*s, line %u", strlen(line) - 1, line, line_number);
        }
        else if (line[0] != '\n')
        {
            switch (state)
            {
                case STATE_NONE:
                    if (line[0] != '#')
                        fprintf(stderr, "Ignoring line '%s' in %s.\n", line, argv[1]);
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

                case STATE_TEST:
                    if (line[0] != '#')
                        parse_test_directive(&context, line);
                    break;
            }
        }
    }

    if (context.ps_code)
        ID3D10Blob_Release(context.ps_code);
    destroy_test_context(&context.c);

    fclose(f);
}
