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

enum texture_data_type
{
    TEXTURE_DATA_FLOAT,
    TEXTURE_DATA_SINT,
    TEXTURE_DATA_UINT,
};

struct sampler
{
    unsigned int slot;

    D3D12_FILTER filter;
    D3D12_TEXTURE_ADDRESS_MODE u_address, v_address, w_address;
};

struct texture
{
    unsigned int slot;

    DXGI_FORMAT format;
    enum texture_data_type data_type;
    unsigned int texel_size;
    unsigned int width, height;
    uint8_t *data;
    size_t data_size, data_capacity;

    D3D12_DESCRIPTOR_RANGE descriptor_range;
    ID3D12DescriptorHeap *heap;
    ID3D12Resource *resource;
    unsigned int root_index;
};

struct shader_context
{
    struct test_context c;

    ID3D10Blob *ps_code;

    uint32_t *uniforms;
    size_t uniform_count;

    struct texture *textures;
    size_t texture_count;

    struct sampler *samplers;
    size_t sampler_count;
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

static void free_texture(struct texture *texture)
{
    ID3D12DescriptorHeap_Release(texture->heap);
    ID3D12Resource_Release(texture->resource);
    free(texture->data);
    memset(texture, 0, sizeof(*texture));
}

enum parse_state
{
    STATE_NONE,
    STATE_PREPROC,
    STATE_PREPROC_INVALID,
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

static void parse_texture_format(struct texture *texture, const char *line)
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

    fprintf(stderr, "Unknown format '%s'.\n", line);
    texture->format = DXGI_FORMAT_R32G32B32A32_FLOAT;
    texture->data_type = TEXTURE_DATA_FLOAT;
    texture->texel_size = 16;
}

static D3D12_TEXTURE_ADDRESS_MODE parse_sampler_address_mode(const char *line, const char **rest)
{
    if (match_string(line, "border", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_BORDER;
    if (match_string(line, "clamp", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    if (match_string(line, "mirror once", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_MIRROR_ONCE;
    if (match_string(line, "mirror", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_MIRROR;
    if (match_string(line, "wrap", rest))
        return D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    fprintf(stderr, "Malformed address mode '%s'.\n", line);
    return D3D12_TEXTURE_ADDRESS_MODE_WRAP;
}

static void parse_sampler_directive(struct sampler *sampler, const char *line)
{
    const char *const orig_line = line;

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
                sampler->filter = filters[i].filter;
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

static void parse_texture_directive(struct texture *texture, const char *line)
{
    const char *const orig_line = line;
    int ret;

    if (match_string(line, "format", &line))
    {
        parse_texture_format(texture, line);
    }
    else if (match_string(line, "size", &line))
    {
        ret = sscanf(line, "( %u , %u )", &texture->width, &texture->height);
        if (ret < 2)
            goto err;
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

    return;

err:
    fprintf(stderr, "Ignoring malformed line '%s'.\n", orig_line);
}

static void parse_test_directive(struct shader_context *context, const char *line)
{
    const char *const orig_line = line;

    if (match_string(line, "draw quad", &line))
    {
        D3D12_SHADER_BYTECODE ps
                = {ID3D10Blob_GetBufferPointer(context->ps_code), ID3D10Blob_GetBufferSize(context->ps_code)};
        ID3D12GraphicsCommandList *command_list = context->c.list;
        D3D12_ROOT_SIGNATURE_DESC root_signature_desc = {0};
        D3D12_ROOT_PARAMETER root_params[3], *root_param;
        D3D12_STATIC_SAMPLER_DESC static_samplers[1];
        static const float clear_color[4];
        unsigned int uniform_index;
        ID3D12PipelineState *pso;
        HRESULT hr;
        size_t i;

        root_signature_desc.NumParameters = 0;
        root_signature_desc.pParameters = root_params;
        root_signature_desc.NumStaticSamplers = 0;
        root_signature_desc.pStaticSamplers = static_samplers;

        if (context->uniform_count)
        {
            uniform_index = root_signature_desc.NumParameters++;
            root_param = &root_params[uniform_index];
            root_param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
            root_param->Constants.ShaderRegister = 0;
            root_param->Constants.RegisterSpace = 0;
            root_param->Constants.Num32BitValues = context->uniform_count;
            root_param->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
        }

        for (i = 0; i < context->texture_count; ++i)
        {
            struct texture *texture = &context->textures[i];
            D3D12_DESCRIPTOR_RANGE *range = &texture->descriptor_range;
            D3D12_SUBRESOURCE_DATA resource_data;

            texture->root_index = root_signature_desc.NumParameters++;
            root_param = &root_params[texture->root_index];
            root_param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
            root_param->DescriptorTable.NumDescriptorRanges = 1;
            root_param->DescriptorTable.pDescriptorRanges = range;
            root_param->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

            range->RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
            range->NumDescriptors = 1;
            range->BaseShaderRegister = texture->slot;
            range->RegisterSpace = 0;
            range->OffsetInDescriptorsFromTableStart = 0;

            if (!texture->resource)
            {
                texture->heap = create_gpu_descriptor_heap(context->c.device,
                        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, 1);
                texture->resource = create_default_texture(context->c.device, texture->width, texture->height,
                        texture->format, 0, D3D12_RESOURCE_STATE_COPY_DEST);
                resource_data.pData = texture->data;
                resource_data.SlicePitch = resource_data.RowPitch = texture->width * texture->texel_size;
                upload_texture_data(texture->resource, &resource_data, 1, context->c.queue, command_list);
                reset_command_list(command_list, context->c.allocator);
                transition_resource_state(command_list, texture->resource, D3D12_RESOURCE_STATE_COPY_DEST,
                        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
                ID3D12Device_CreateShaderResourceView(context->c.device, texture->resource,
                        NULL, get_cpu_descriptor_handle(&context->c, texture->heap, 0));
            }
        }

        assert(root_signature_desc.NumParameters <= ARRAY_SIZE(root_params));

        for (i = 0; i < context->sampler_count; ++i)
        {
            D3D12_STATIC_SAMPLER_DESC *sampler_desc = &static_samplers[root_signature_desc.NumStaticSamplers++];
            const struct sampler *sampler = &context->samplers[i];

            memset(sampler_desc, 0, sizeof(*sampler_desc));
            sampler_desc->Filter = sampler->filter;
            sampler_desc->AddressU = sampler->u_address;
            sampler_desc->AddressV = sampler->v_address;
            sampler_desc->AddressW = sampler->w_address;
            sampler_desc->ShaderRegister = sampler->slot;
            sampler_desc->RegisterSpace = 0;
            sampler_desc->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
        }

        if (context->c.root_signature)
            ID3D12RootSignature_Release(context->c.root_signature);
        hr = create_root_signature(context->c.device, &root_signature_desc, &context->c.root_signature);
        ok(hr == S_OK, "Failed to create root signature, hr %#x.\n", hr);

        pso = create_pipeline_state(context->c.device, context->c.root_signature,
                context->c.render_target_desc.Format, NULL, &ps, NULL);
        if (!pso)
            return;

        ID3D12GraphicsCommandList_SetGraphicsRootSignature(command_list, context->c.root_signature);
        if (context->uniform_count)
            ID3D12GraphicsCommandList_SetGraphicsRoot32BitConstants(command_list, uniform_index,
                    context->uniform_count, context->uniforms, 0);
        for (i = 0; i < context->texture_count; ++i)
            ID3D12GraphicsCommandList_SetGraphicsRootDescriptorTable(command_list, context->textures[i].root_index,
                    get_gpu_descriptor_handle(&context->c, context->textures[i].heap, 0));

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

        ret = sscanf(line, "( %u , %u , %u , %u ) ( %f , %f , %f , %f ) %u",
                     &x, &y, &w, &h, &v.x, &v.y, &v.z, &v.w, &ulps);
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
        else if (match_string(line, "float", &line))
        {
            float f;

            if (sscanf(line, "%f", &f) < 1)
                goto err;
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
                goto err;
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
                goto err;
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

static struct texture *get_texture(struct shader_context *context, unsigned int slot)
{
    struct texture *texture;
    size_t i;

    for (i = 0; i < context->texture_count; ++i)
    {
        texture = &context->textures[i];
        if (texture->slot == slot)
            return texture;
    }

    return NULL;
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
    struct sampler *current_sampler;
    struct texture *current_texture;
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
                case STATE_SAMPLER:
                case STATE_TEST:
                case STATE_TEXTURE:
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
            unsigned int index;

            if (!strcmp(line, "[pixel shader]\n"))
            {
                state = STATE_SHADER_PIXEL;

                if (context.ps_code)
                    ID3D10Blob_Release(context.ps_code);
                context.ps_code = NULL;
            }
            else if (!strcmp(line, "[pixel shader fail]\n"))
            {
                state = STATE_SHADER_INVALID_PIXEL;
            }
            else if (sscanf(line, "[sampler %u]\n", &index))
            {
                state = STATE_SAMPLER;

                if ((current_sampler = get_sampler(&context, index)))
                {
                    memset(current_sampler, 0, sizeof(*current_sampler));
                }
                else
                {
                    context.samplers = realloc(context.samplers,
                            ++context.sampler_count * sizeof(*context.samplers));
                    current_sampler = &context.samplers[context.sampler_count - 1];
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

                if ((current_texture = get_texture(&context, index)))
                {
                    free_texture(current_texture);
                }
                else
                {
                    context.textures = realloc(context.textures,
                            ++context.texture_count * sizeof(*context.textures));
                    current_texture = &context.textures[context.texture_count - 1];
                    memset(current_texture, 0, sizeof(*current_texture));
                }
                current_texture->slot = index;
                current_texture->format = DXGI_FORMAT_R32G32B32A32_FLOAT;
                current_texture->data_type = TEXTURE_DATA_FLOAT;
                current_texture->texel_size = 16;
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

                case STATE_SAMPLER:
                    parse_sampler_directive(current_sampler, line);
                    break;

                case STATE_TEXTURE:
                    parse_texture_directive(current_texture, line);
                    break;

                case STATE_TEST:
                    parse_test_directive(&context, line);
                    break;
            }
        }
    }

    if (context.ps_code)
        ID3D10Blob_Release(context.ps_code);
    for (i = 0; i < context.texture_count; ++i)
        free_texture(&context.textures[i]);
    destroy_test_context(&context.c);

    fclose(f);
}
