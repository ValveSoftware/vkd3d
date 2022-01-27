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

#include "config.h"
#include <assert.h>
#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
#include "d3d12_crosstest.h"
#include "shader_runner.h"

struct d3d12_shader_context
{
    struct shader_context c;

    struct test_context test_context;
};

static struct d3d12_shader_context *d3d12_shader_context(struct shader_context *c)
{
    return CONTAINING_RECORD(c, struct d3d12_shader_context, c);
}

static ID3D10Blob *d3d12_runner_compile_shader(struct shader_context *c,
        const char *source, enum shader_model shader_model)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_4_0] = "ps_4_0",
        [SHADER_MODEL_4_1] = "ps_4_1",
        [SHADER_MODEL_5_0] = "ps_5_0",
        [SHADER_MODEL_5_1] = "ps_5_1",
    };

    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main",
            shader_models[shader_model], 0, 0, &blob, &errors);
    ok(hr == S_OK, "Failed to compile shader, hr %#x.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

static void d3d12_runner_draw_quad(struct shader_context *c)
{
    struct d3d12_shader_context *context = d3d12_shader_context(c);
    struct test_context *test_context = &context->test_context;

    D3D12_SHADER_BYTECODE ps
            = {ID3D10Blob_GetBufferPointer(context->c.ps_code), ID3D10Blob_GetBufferSize(context->c.ps_code)};
    ID3D12GraphicsCommandList *command_list = test_context->list;
    D3D12_ROOT_SIGNATURE_DESC root_signature_desc = {0};
    D3D12_ROOT_PARAMETER root_params[3], *root_param;
    ID3D12CommandQueue *queue = test_context->queue;
    D3D12_STATIC_SAMPLER_DESC static_samplers[1];
    ID3D12Device *device = test_context->device;
    static const float clear_color[4];
    unsigned int uniform_index;
    ID3D12PipelineState *pso;
    HRESULT hr;
    size_t i;

    root_signature_desc.NumParameters = 0;
    root_signature_desc.pParameters = root_params;
    root_signature_desc.NumStaticSamplers = 0;
    root_signature_desc.pStaticSamplers = static_samplers;

    if (context->c.uniform_count)
    {
        uniform_index = root_signature_desc.NumParameters++;
        root_param = &root_params[uniform_index];
        root_param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        root_param->Constants.ShaderRegister = 0;
        root_param->Constants.RegisterSpace = 0;
        root_param->Constants.Num32BitValues = context->c.uniform_count;
        root_param->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
    }

    for (i = 0; i < context->c.texture_count; ++i)
    {
        struct texture *texture = &context->c.textures[i];
        D3D12_SUBRESOURCE_DATA resource_data;
        D3D12_DESCRIPTOR_RANGE *range;
        ID3D12Resource *resource;

        range = &texture->descriptor_range;

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

        texture->heap = create_gpu_descriptor_heap(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, 1);
        resource = create_default_texture(device, texture->width, texture->height,
                texture->format, 0, D3D12_RESOURCE_STATE_COPY_DEST);
        resource_data.pData = texture->data;
        resource_data.SlicePitch = resource_data.RowPitch = texture->width * texture->texel_size;
        upload_texture_data(resource, &resource_data, 1, queue, command_list);
        reset_command_list(command_list, test_context->allocator);
        transition_resource_state(command_list, resource, D3D12_RESOURCE_STATE_COPY_DEST,
                D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
        ID3D12Device_CreateShaderResourceView(device, resource,
                NULL, get_cpu_descriptor_handle(test_context, texture->heap, 0));

        texture->resource = resource;
    }

    assert(root_signature_desc.NumParameters <= ARRAY_SIZE(root_params));

    for (i = 0; i < context->c.sampler_count; ++i)
    {
        D3D12_STATIC_SAMPLER_DESC *sampler_desc = &static_samplers[root_signature_desc.NumStaticSamplers++];
        const struct sampler *sampler = &context->c.samplers[i];

        memset(sampler_desc, 0, sizeof(*sampler_desc));
        sampler_desc->Filter = sampler->filter;
        sampler_desc->AddressU = sampler->u_address;
        sampler_desc->AddressV = sampler->v_address;
        sampler_desc->AddressW = sampler->w_address;
        sampler_desc->ShaderRegister = sampler->slot;
        sampler_desc->RegisterSpace = 0;
        sampler_desc->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
    }

    if (test_context->root_signature)
        ID3D12RootSignature_Release(test_context->root_signature);
    hr = create_root_signature(device, &root_signature_desc, &test_context->root_signature);
    ok(hr == S_OK, "Failed to create root signature, hr %#x.\n", hr);

    pso = create_pipeline_state(device, test_context->root_signature,
            test_context->render_target_desc.Format, NULL, &ps, NULL);
    if (!pso)
        return;
    vkd3d_array_reserve((void **)&test_context->pso, &test_context->pso_capacity,
            test_context->pso_count + 1, sizeof(*test_context->pso));
    test_context->pso[test_context->pso_count++] = pso;

    ID3D12GraphicsCommandList_SetGraphicsRootSignature(command_list, test_context->root_signature);
    if (context->c.uniform_count)
        ID3D12GraphicsCommandList_SetGraphicsRoot32BitConstants(command_list, uniform_index,
                context->c.uniform_count, context->c.uniforms, 0);
    for (i = 0; i < context->c.texture_count; ++i)
    {
        struct texture *texture = &context->c.textures[i];

        ID3D12GraphicsCommandList_SetGraphicsRootDescriptorTable(command_list, texture->root_index,
                get_gpu_descriptor_handle(test_context, texture->heap, 0));
    }

    ID3D12GraphicsCommandList_OMSetRenderTargets(command_list, 1, &test_context->rtv, false, NULL);
    ID3D12GraphicsCommandList_RSSetScissorRects(command_list, 1, &test_context->scissor_rect);
    ID3D12GraphicsCommandList_RSSetViewports(command_list, 1, &test_context->viewport);
    ID3D12GraphicsCommandList_IASetPrimitiveTopology(command_list, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    ID3D12GraphicsCommandList_ClearRenderTargetView(command_list, test_context->rtv, clear_color, 0, NULL);
    ID3D12GraphicsCommandList_SetPipelineState(command_list, pso);
    ID3D12GraphicsCommandList_DrawInstanced(command_list, 3, 1, 0, 0);
    transition_resource_state(command_list, test_context->render_target,
            D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_COPY_SOURCE);

    /* Finish the command list so that we can destroy objects. */
    hr = ID3D12GraphicsCommandList_Close(command_list);
    ok(hr == S_OK, "Failed to close command list, hr %#x.\n", hr);
    exec_command_list(queue, command_list);
    wait_queue_idle(device, queue);
    reset_command_list(command_list, test_context->allocator);

    for (i = 0; i < context->c.texture_count; ++i)
    {
        struct texture *texture = &context->c.textures[i];

        ID3D12DescriptorHeap_Release(texture->heap);
        ID3D12Resource_Release(texture->resource);
        free(texture);
    }
}

static void d3d12_runner_probe_vec4(struct shader_context *c,
        const RECT *rect, const struct vec4 *v, unsigned int ulps)
{
    struct d3d12_shader_context *context = d3d12_shader_context(c);
    struct test_context *test_context = &context->test_context;
    struct resource_readback rb;

    get_texture_readback_with_command_list(test_context->render_target, 0, &rb,
            test_context->queue, test_context->list);
    check_readback_data_vec4(&rb, rect, v, ulps);
    release_resource_readback(&rb);
    reset_command_list(test_context->list, test_context->allocator);
}

static const struct shader_runner_ops d3d12_runner_ops =
{
    .compile_shader = d3d12_runner_compile_shader,
    .draw_quad = d3d12_runner_draw_quad,
    .probe_vec4 = d3d12_runner_probe_vec4,
};

void run_shader_tests_d3d12(int argc, char **argv)
{
    static const struct test_context_desc desc =
    {
        .rt_width = 640,
        .rt_height = 480,
        .no_root_signature = true,
        .rt_format = DXGI_FORMAT_R32G32B32A32_FLOAT,
    };
    struct d3d12_shader_context context;

    parse_args(argc, argv);
    enable_d3d12_debug_layer(argc, argv);
    init_adapter_info();
    init_test_context(&context.test_context, &desc);
    run_shader_tests(&context.c, argc, argv, &d3d12_runner_ops);
    destroy_test_context(&context.test_context);
}
