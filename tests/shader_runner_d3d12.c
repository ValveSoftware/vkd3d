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
#ifndef __MINGW32__
#define WIDL_C_INLINE_WRAPPERS
#endif
#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
#include "d3d12_crosstest.h"
#include "shader_runner.h"
#include "dxcompiler.h"

struct d3d12_resource
{
    struct resource r;

    D3D12_DESCRIPTOR_RANGE descriptor_range;
    ID3D12Resource *resource;
    unsigned int root_index;
};

static struct d3d12_resource *d3d12_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct d3d12_resource, r);
}

struct d3d12_shader_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;

    struct test_context test_context;

    ID3D12DescriptorHeap *heap, *rtv_heap, *dsv_heap;

    ID3D12CommandQueue *compute_queue;
    ID3D12CommandAllocator *compute_allocator;
    ID3D12GraphicsCommandList *compute_list;

    IDxcCompiler3 *dxc_compiler;
};

static struct d3d12_shader_runner *d3d12_shader_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct d3d12_shader_runner, r);
}

static ID3D10Blob *compile_shader(const struct d3d12_shader_runner *runner, const char *source, enum shader_type type)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    char profile[7];
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "4_0",
        [SHADER_MODEL_3_0] = "4_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
        [SHADER_MODEL_6_0] = "6_0",
    };

    if (runner->r.minimum_shader_model >= SHADER_MODEL_6_0)
    {
        assert(runner->dxc_compiler);
        hr = dxc_compiler_compile_shader(runner->dxc_compiler, type, runner->r.compile_options, source, &blob, &errors);
    }
    else
    {
        sprintf(profile, "%s_%s", shader_type_string(type), shader_models[runner->r.minimum_shader_model]);
        hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", profile, runner->r.compile_options, 0, &blob, &errors);
    }
    ok(FAILED(hr) == !blob, "Got unexpected hr %#x, blob %p.\n", hr, blob);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

#define MAX_RESOURCE_DESCRIPTORS (MAX_RESOURCES * 2)

static struct resource *d3d12_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct d3d12_shader_runner *runner = d3d12_shader_runner(r);
    struct test_context *test_context = &runner->test_context;
    ID3D12Device *device = test_context->device;
    D3D12_SUBRESOURCE_DATA resource_data[3] = {0};
    struct d3d12_resource *resource;
    unsigned int buffer_offset = 0;
    D3D12_RESOURCE_STATES state;

    if (params->level_count > ARRAY_SIZE(resource_data))
        fatal_error("Level count %u is too high.\n", params->level_count);

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    for (unsigned int level = 0; level < params->level_count; ++level)
    {
        unsigned int level_width = get_level_dimension(params->width, level);
        unsigned int level_height = get_level_dimension(params->height, level);

        resource_data[level].pData = &params->data[buffer_offset];
        resource_data[level].RowPitch = level_width * params->texel_size;
        resource_data[level].SlicePitch = level_height * resource_data[level].RowPitch;
        buffer_offset += resource_data[level].SlicePitch;
    }

    switch (params->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            if (!runner->rtv_heap)
                runner->rtv_heap = create_cpu_descriptor_heap(device,
                        D3D12_DESCRIPTOR_HEAP_TYPE_RTV, MAX_RESOURCE_DESCRIPTORS);

            if (params->slot >= D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT)
                fatal_error("RTV slot %u is too high.\n", params->slot);
            if (params->sample_count > 1 && params->level_count > 1)
                fatal_error("Multisampled texture has multiple levels.\n");

            resource->resource = create_default_texture_(__LINE__, device, D3D12_RESOURCE_DIMENSION_TEXTURE2D,
                    params->width, params->height, 1, params->level_count, params->sample_count, params->format,
                    D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET, D3D12_RESOURCE_STATE_RENDER_TARGET);
            ID3D12Device_CreateRenderTargetView(device, resource->resource,
                    NULL, get_cpu_rtv_handle(test_context, runner->rtv_heap, resource->r.slot));
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            if (!runner->dsv_heap)
                runner->dsv_heap = create_cpu_descriptor_heap(device, D3D12_DESCRIPTOR_HEAP_TYPE_DSV, 1);

            resource->resource = create_default_texture2d(device, params->width, params->height, 1, params->level_count,
                    params->format, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL, D3D12_RESOURCE_STATE_DEPTH_WRITE);
            ID3D12Device_CreateDepthStencilView(device, resource->resource,
                    NULL, get_cpu_dsv_handle(test_context, runner->dsv_heap, 0));
            break;

        case RESOURCE_TYPE_TEXTURE:
            if (!runner->heap)
                runner->heap = create_gpu_descriptor_heap(device,
                        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, MAX_RESOURCE_DESCRIPTORS);

            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
            {
                D3D12_SHADER_RESOURCE_VIEW_DESC srv_desc = { 0 };

                resource->resource = create_default_buffer(device, params->data_size,
                        0, D3D12_RESOURCE_STATE_COPY_DEST);
                upload_buffer_data_with_states(resource->resource, 0, params->data_size, resource_data[0].pData,
                        test_context->queue, test_context->list,
                        RESOURCE_STATE_DO_NOT_CHANGE,
                        D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
                reset_command_list(test_context->list, test_context->allocator);

                srv_desc.Format = params->format;
                srv_desc.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
                srv_desc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
                srv_desc.Buffer.NumElements = params->width * params->height;

                ID3D12Device_CreateShaderResourceView(device, resource->resource,
                        &srv_desc, get_cpu_descriptor_handle(test_context, runner->heap, resource->r.slot));
            }
            else
            {
                if (params->sample_count > 1 && params->level_count > 1)
                    fatal_error("Multisampled texture has multiple levels.\n");

                resource->resource = create_default_texture_(__LINE__, device, D3D12_RESOURCE_DIMENSION_TEXTURE2D,
                        params->width, params->height, 1, params->level_count, params->sample_count, params->format,
                        /* Multisampled textures must have ALLOW_RENDER_TARGET set. */
                        (params->sample_count > 1) ? D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET : 0,
                        D3D12_RESOURCE_STATE_COPY_DEST);

                if (params->data)
                {
                    if (params->sample_count > 1)
                        fatal_error("Cannot upload data to a multisampled texture.\n");
                    upload_texture_data_with_states(resource->resource, resource_data,
                            params->level_count, test_context->queue, test_context->list,
                            RESOURCE_STATE_DO_NOT_CHANGE,
                            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE | D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);
                    reset_command_list(test_context->list, test_context->allocator);
                }

                ID3D12Device_CreateShaderResourceView(device, resource->resource,
                        NULL, get_cpu_descriptor_handle(test_context, runner->heap, resource->r.slot));
            }
            break;

        case RESOURCE_TYPE_UAV:
            if (!runner->heap)
                runner->heap = create_gpu_descriptor_heap(device,
                        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, MAX_RESOURCE_DESCRIPTORS);

            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
            {
                D3D12_UNORDERED_ACCESS_VIEW_DESC uav_desc = { 0 };

                resource->resource = create_default_buffer(device, params->data_size,
                        D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_DEST);
                upload_buffer_data_with_states(resource->resource, 0, params->data_size, resource_data[0].pData,
                        test_context->queue, test_context->list,
                        RESOURCE_STATE_DO_NOT_CHANGE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
                reset_command_list(test_context->list, test_context->allocator);

                uav_desc.Format = params->format;
                uav_desc.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
                uav_desc.Buffer.NumElements = params->width * params->height;
                uav_desc.Buffer.StructureByteStride = params->stride;

                ID3D12Device_CreateUnorderedAccessView(device, resource->resource,
                        params->is_uav_counter ? resource->resource : NULL, &uav_desc,
                        get_cpu_descriptor_handle(test_context, runner->heap, resource->r.slot + MAX_RESOURCES));
            }
            else
            {
                state = params->data ? D3D12_RESOURCE_STATE_COPY_DEST : D3D12_RESOURCE_STATE_UNORDERED_ACCESS;
                resource->resource = create_default_texture2d(device, params->width, params->height, 1, params->level_count,
                        params->format, D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS, state);
                if (params->data)
                {
                    upload_texture_data_with_states(resource->resource, resource_data,
                            params->level_count, test_context->queue, test_context->list,
                            RESOURCE_STATE_DO_NOT_CHANGE, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
                    reset_command_list(test_context->list, test_context->allocator);
                }
                ID3D12Device_CreateUnorderedAccessView(device, resource->resource, NULL, NULL,
                        get_cpu_descriptor_handle(test_context, runner->heap, resource->r.slot + MAX_RESOURCES));
            }
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            resource->resource = create_upload_buffer(device, params->data_size, params->data);
            break;
    }

    return &resource->r;
}

static void d3d12_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct d3d12_resource *resource = d3d12_resource(res);

    ID3D12Resource_Release(resource->resource);
    free(resource);
}

static ID3D12RootSignature *d3d12_runner_create_root_signature(struct d3d12_shader_runner *runner,
        ID3D12CommandQueue *queue, ID3D12CommandAllocator *allocator,
        ID3D12GraphicsCommandList *command_list, unsigned int *uniform_index)
{
    D3D12_ROOT_SIGNATURE_DESC root_signature_desc = {0};
    D3D12_ROOT_PARAMETER root_params[17], *root_param;
    D3D12_STATIC_SAMPLER_DESC static_samplers[7];
    struct d3d12_resource *base_resource = NULL;
    ID3D12RootSignature *root_signature;
    unsigned int slot;
    HRESULT hr;
    size_t i;

    root_signature_desc.NumParameters = 0;
    root_signature_desc.pParameters = root_params;
    root_signature_desc.NumStaticSamplers = 0;
    root_signature_desc.pStaticSamplers = static_samplers;
    root_signature_desc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    if (runner->r.uniform_count)
    {
        *uniform_index = root_signature_desc.NumParameters++;
        root_param = &root_params[*uniform_index];
        root_param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        root_param->Constants.ShaderRegister = 0;
        root_param->Constants.RegisterSpace = 0;
        root_param->Constants.Num32BitValues = runner->r.uniform_count;
        root_param->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d12_resource *resource = d3d12_resource(runner->r.resources[i]);
        D3D12_DESCRIPTOR_RANGE *range;

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                range = &resource->descriptor_range;

                if (base_resource && resource->r.type == base_resource->r.type && resource->r.slot == slot + 1)
                {
                    ++base_resource->descriptor_range.NumDescriptors;
                    resource->descriptor_range.NumDescriptors = 0;
                    ++slot;
                    continue;
                }

                resource->root_index = root_signature_desc.NumParameters++;
                root_param = &root_params[resource->root_index];
                root_param->ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
                root_param->DescriptorTable.NumDescriptorRanges = 1;
                root_param->DescriptorTable.pDescriptorRanges = range;
                root_param->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

                if (resource->r.type == RESOURCE_TYPE_UAV)
                    range->RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_UAV;
                else
                    range->RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
                range->NumDescriptors = 1;
                range->BaseShaderRegister = resource->r.slot;
                range->RegisterSpace = 0;
                range->OffsetInDescriptorsFromTableStart = 0;

                base_resource = resource;
                slot = resource->r.slot;
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;
        }
    }

    assert(root_signature_desc.NumParameters <= ARRAY_SIZE(root_params));

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        D3D12_STATIC_SAMPLER_DESC *sampler_desc = &static_samplers[root_signature_desc.NumStaticSamplers++];
        const struct sampler *sampler = &runner->r.samplers[i];

        memset(sampler_desc, 0, sizeof(*sampler_desc));
        sampler_desc->Filter = sampler->filter;
        sampler_desc->AddressU = sampler->u_address;
        sampler_desc->AddressV = sampler->v_address;
        sampler_desc->AddressW = sampler->w_address;
        sampler_desc->ComparisonFunc = sampler->func;
        sampler_desc->MaxLOD = FLT_MAX;
        sampler_desc->ShaderRegister = sampler->slot;
        sampler_desc->RegisterSpace = 0;
        sampler_desc->ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;
    }

    hr = create_root_signature(runner->test_context.device, &root_signature_desc, &root_signature);
    ok(hr == S_OK, "Failed to create root signature, hr %#x.\n", hr);
    return root_signature;
}

static void add_pso(struct test_context *test_context, ID3D12PipelineState *pso)
{
    vkd3d_array_reserve((void **)&test_context->pso, &test_context->pso_capacity,
            test_context->pso_count + 1, sizeof(*test_context->pso));
    test_context->pso[test_context->pso_count++] = pso;
}

static bool d3d12_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    struct d3d12_shader_runner *runner = d3d12_shader_runner(r);
    struct test_context *test_context = &runner->test_context;

    ID3D12GraphicsCommandList *command_list = runner->compute_list;
    ID3D12CommandAllocator *allocator = runner->compute_allocator;
    ID3D12CommandQueue *queue = runner->compute_queue;
    ID3D12Device *device = test_context->device;
    ID3D12RootSignature *root_signature;
    unsigned int uniform_index;
    ID3D12PipelineState *pso;
    D3D12_SHADER_BYTECODE cs;
    ID3D10Blob *cs_code;
    HRESULT hr;
    size_t i;

    cs_code = compile_shader(runner, runner->r.cs_source, SHADER_TYPE_CS);
    todo_if(runner->r.is_todo && runner->r.minimum_shader_model < SHADER_MODEL_6_0) ok(cs_code, "Failed to compile shader.\n");
    if (!cs_code)
        return false;

    root_signature = d3d12_runner_create_root_signature(runner, queue, allocator, command_list, &uniform_index);

    cs.pShaderBytecode = ID3D10Blob_GetBufferPointer(cs_code);
    cs.BytecodeLength = ID3D10Blob_GetBufferSize(cs_code);
    todo_if(runner->r.is_todo)
    pso = create_compute_pipeline_state(device, root_signature, cs);
    ID3D10Blob_Release(cs_code);

    if (!pso)
    {
        ID3D12RootSignature_Release(root_signature);
        return false;
    }

    add_pso(test_context, pso);

    ID3D12GraphicsCommandList_SetComputeRootSignature(command_list, root_signature);
    if (runner->r.uniform_count)
        ID3D12GraphicsCommandList_SetComputeRoot32BitConstants(command_list, uniform_index,
                runner->r.uniform_count, runner->r.uniforms, 0);
    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d12_resource *resource = d3d12_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_TEXTURE:
                if (resource->descriptor_range.NumDescriptors)
                    ID3D12GraphicsCommandList_SetComputeRootDescriptorTable(command_list, resource->root_index,
                            get_gpu_descriptor_handle(test_context, runner->heap, resource->r.slot));
                break;

            case RESOURCE_TYPE_UAV:
                if (resource->descriptor_range.NumDescriptors)
                    ID3D12GraphicsCommandList_SetComputeRootDescriptorTable(command_list, resource->root_index,
                            get_gpu_descriptor_handle(test_context, runner->heap, resource->r.slot + MAX_RESOURCES));
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;
        }
    }

    ID3D12GraphicsCommandList_SetPipelineState(command_list, pso);
    ID3D12GraphicsCommandList_Dispatch(command_list, x, y, z);
    ID3D12RootSignature_Release(root_signature);

    /* Finish the command list so that we can destroy objects.
     * Also, subsequent UAV probes will use the graphics command list, so make
     * sure that the above barriers are actually executed. */
    hr = ID3D12GraphicsCommandList_Close(command_list);
    ok(hr == S_OK, "Failed to close command list, hr %#x.\n", hr);
    exec_command_list(queue, command_list);
    wait_queue_idle(device, queue);
    reset_command_list(command_list, allocator);

    return true;
}

static void d3d12_runner_clear(struct shader_runner *r, struct resource *resource, const struct vec4 *clear_value)
{
    struct d3d12_shader_runner *runner = d3d12_shader_runner(r);
    struct test_context *test_context = &runner->test_context;

    ID3D12GraphicsCommandList *command_list = test_context->list;
    ID3D12CommandQueue *queue = test_context->queue;
    ID3D12Device *device = test_context->device;
    D3D12_CPU_DESCRIPTOR_HANDLE view;
    HRESULT hr;

    switch (resource->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            view = get_cpu_rtv_handle(test_context, runner->rtv_heap, resource->slot);
            ID3D12GraphicsCommandList_ClearRenderTargetView(command_list, view, (const float *)clear_value, 0, NULL);
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            view = get_cpu_dsv_handle(test_context, runner->dsv_heap, 0);
            ID3D12GraphicsCommandList_ClearDepthStencilView(command_list, view,
                    D3D12_CLEAR_FLAG_DEPTH, clear_value->x, 0, 0, NULL);
            break;

        default:
            fatal_error("Clears are not implemented for resource type %u.\n", resource->type);
    }

    hr = ID3D12GraphicsCommandList_Close(command_list);
    ok(hr == S_OK, "Failed to close command list, hr %#x.\n", hr);
    exec_command_list(queue, command_list);
    wait_queue_idle(device, queue);
    reset_command_list(command_list, test_context->allocator);
}

static bool d3d12_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count, unsigned int instance_count)
{
    struct d3d12_shader_runner *runner = d3d12_shader_runner(r);
    struct test_context *test_context = &runner->test_context;

    D3D12_CPU_DESCRIPTOR_HANDLE rtvs[D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT] = {0};
    ID3D10Blob *vs_code, *ps_code, *hs_code = NULL, *ds_code = NULL;
    ID3D12GraphicsCommandList *command_list = test_context->list;
    unsigned int uniform_index, sample_count, rtv_count = 0;
    D3D12_GRAPHICS_PIPELINE_STATE_DESC pso_desc = {0};
    ID3D12CommandQueue *queue = test_context->queue;
    D3D12_INPUT_ELEMENT_DESC *input_element_descs;
    ID3D12Device *device = test_context->device;
    D3D12_CPU_DESCRIPTOR_HANDLE dsv = {0};
    ID3D12PipelineState *pso;
    bool succeeded;
    HRESULT hr;
    size_t i;

    ps_code = compile_shader(runner, runner->r.ps_source, SHADER_TYPE_PS);
    vs_code = compile_shader(runner, runner->r.vs_source, SHADER_TYPE_VS);
    succeeded = ps_code && vs_code;

    if (runner->r.hs_source)
    {
        hs_code = compile_shader(runner, runner->r.hs_source, SHADER_TYPE_HS);
        succeeded = succeeded && hs_code;
    }
    if (runner->r.ds_source)
    {
        ds_code = compile_shader(runner, runner->r.ds_source, SHADER_TYPE_DS);
        succeeded = succeeded && ds_code;
    }

    todo_if(runner->r.is_todo && runner->r.minimum_shader_model < SHADER_MODEL_6_0)
    ok(succeeded, "Failed to compile shaders.\n");

    if (!succeeded)
    {
        if (ps_code)
            ID3D10Blob_Release(ps_code);
        if (vs_code)
            ID3D10Blob_Release(vs_code);
        if (hs_code)
            ID3D10Blob_Release(hs_code);
        if (ds_code)
            ID3D10Blob_Release(ds_code);
        return false;
    }

    if (test_context->root_signature)
        ID3D12RootSignature_Release(test_context->root_signature);
    test_context->root_signature = d3d12_runner_create_root_signature(runner,
            queue, test_context->allocator, command_list, &uniform_index);

    for (i = 0, sample_count = 1; i < runner->r.resource_count; ++i)
    {
        struct d3d12_resource *resource = d3d12_resource(runner->r.resources[i]);

        if (resource->r.type == RESOURCE_TYPE_RENDER_TARGET)
        {
            pso_desc.RTVFormats[resource->r.slot] = resource->r.format;
            pso_desc.NumRenderTargets = max(pso_desc.NumRenderTargets, resource->r.slot + 1);
            pso_desc.BlendState.RenderTarget[resource->r.slot].RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
            if (resource->r.sample_count)
                sample_count = resource->r.sample_count;
        }
        else if (resource->r.type == RESOURCE_TYPE_DEPTH_STENCIL)
        {
            assert(!resource->r.slot);
            pso_desc.DSVFormat = resource->r.format;
            pso_desc.DepthStencilState.DepthEnable = true;
            pso_desc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
            pso_desc.DepthStencilState.DepthFunc = runner->r.depth_func;
        }
    }

    pso_desc.VS.pShaderBytecode = ID3D10Blob_GetBufferPointer(vs_code);
    pso_desc.VS.BytecodeLength = ID3D10Blob_GetBufferSize(vs_code);
    pso_desc.PS.pShaderBytecode = ID3D10Blob_GetBufferPointer(ps_code);
    pso_desc.PS.BytecodeLength = ID3D10Blob_GetBufferSize(ps_code);
    if (hs_code)
    {
        pso_desc.HS.pShaderBytecode = ID3D10Blob_GetBufferPointer(hs_code);
        pso_desc.HS.BytecodeLength = ID3D10Blob_GetBufferSize(hs_code);
        pso_desc.DS.pShaderBytecode = ID3D10Blob_GetBufferPointer(ds_code);
        pso_desc.DS.BytecodeLength = ID3D10Blob_GetBufferSize(ds_code);
        pso_desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;
    }
    else
    {
        pso_desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    }
    pso_desc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
    pso_desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
    pso_desc.SampleDesc.Count = sample_count;
    pso_desc.SampleMask = r->sample_mask ? r->sample_mask : ~(UINT)0;
    pso_desc.pRootSignature = test_context->root_signature;

    input_element_descs = calloc(runner->r.input_element_count, sizeof(*input_element_descs));
    for (i = 0; i < runner->r.input_element_count; ++i)
    {
        const struct input_element *element = &runner->r.input_elements[i];
        D3D12_INPUT_ELEMENT_DESC *desc = &input_element_descs[i];

        desc->SemanticName = element->name;
        desc->SemanticIndex = element->index;
        desc->Format = element->format;
        desc->InputSlot = element->slot;
        desc->AlignedByteOffset = D3D12_APPEND_ALIGNED_ELEMENT;
        desc->InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA;
    }

    pso_desc.InputLayout.pInputElementDescs = input_element_descs;
    pso_desc.InputLayout.NumElements = runner->r.input_element_count;

    hr = ID3D12Device_CreateGraphicsPipelineState(device, &pso_desc,
            &IID_ID3D12PipelineState, (void **)&pso);
    todo_if(runner->r.is_todo) ok(hr == S_OK, "Failed to create state, hr %#x.\n", hr);
    ID3D10Blob_Release(vs_code);
    ID3D10Blob_Release(ps_code);
    if (hs_code)
        ID3D10Blob_Release(hs_code);
    if (ds_code)
        ID3D10Blob_Release(ds_code);
    free(input_element_descs);

    if (FAILED(hr))
        return false;

    add_pso(test_context, pso);

    ID3D12GraphicsCommandList_SetGraphicsRootSignature(command_list, test_context->root_signature);
    if (runner->r.uniform_count)
        ID3D12GraphicsCommandList_SetGraphicsRoot32BitConstants(command_list, uniform_index,
                runner->r.uniform_count, runner->r.uniforms, 0);
    if (runner->heap)
        ID3D12GraphicsCommandList_SetDescriptorHeaps(command_list, 1, &runner->heap);
    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d12_resource *resource = d3d12_resource(runner->r.resources[i]);
        D3D12_VERTEX_BUFFER_VIEW vbv;

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                rtvs[resource->r.slot] = get_cpu_rtv_handle(test_context, runner->rtv_heap, resource->r.slot);
                rtv_count = max(rtv_count, resource->r.slot + 1);
                break;

            case RESOURCE_TYPE_DEPTH_STENCIL:
                dsv = get_cpu_dsv_handle(test_context, runner->dsv_heap, 0);
                break;

            case RESOURCE_TYPE_TEXTURE:
                if (resource->descriptor_range.NumDescriptors)
                    ID3D12GraphicsCommandList_SetGraphicsRootDescriptorTable(command_list, resource->root_index,
                            get_gpu_descriptor_handle(test_context, runner->heap, resource->r.slot));
                break;

            case RESOURCE_TYPE_UAV:
                if (resource->descriptor_range.NumDescriptors)
                    ID3D12GraphicsCommandList_SetGraphicsRootDescriptorTable(command_list, resource->root_index,
                            get_gpu_descriptor_handle(test_context, runner->heap, resource->r.slot + MAX_RESOURCES));
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
                vbv.BufferLocation = ID3D12Resource_GetGPUVirtualAddress(resource->resource);
                vbv.StrideInBytes = get_vb_stride(&runner->r, resource->r.slot);
                vbv.SizeInBytes = resource->r.size;

                ID3D12GraphicsCommandList_IASetVertexBuffers(command_list, resource->r.slot, 1, &vbv);
                break;
        }
    }

    ID3D12GraphicsCommandList_OMSetRenderTargets(command_list, rtv_count, rtvs, false, dsv.ptr ? &dsv : NULL);

    ID3D12GraphicsCommandList_RSSetScissorRects(command_list, 1, &test_context->scissor_rect);
    ID3D12GraphicsCommandList_RSSetViewports(command_list, 1, &test_context->viewport);
    ID3D12GraphicsCommandList_IASetPrimitiveTopology(command_list, primitive_topology);
    ID3D12GraphicsCommandList_SetPipelineState(command_list, pso);
    ID3D12GraphicsCommandList_DrawInstanced(command_list, vertex_count, instance_count, 0, 0);

    /* Finish the command list so that we can destroy objects. */
    hr = ID3D12GraphicsCommandList_Close(command_list);
    ok(hr == S_OK, "Failed to close command list, hr %#x.\n", hr);
    exec_command_list(queue, command_list);
    wait_queue_idle(device, queue);
    reset_command_list(command_list, test_context->allocator);

    return true;
}

static struct resource_readback *d3d12_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct d3d12_shader_runner *runner = d3d12_shader_runner(r);
    struct test_context *test_context = &runner->test_context;
    struct d3d12_resource_readback *rb = malloc(sizeof(*rb));
    struct d3d12_resource *resource = d3d12_resource(res);
    D3D12_RESOURCE_STATES state;

    if (resource->r.type == RESOURCE_TYPE_RENDER_TARGET)
        state = D3D12_RESOURCE_STATE_RENDER_TARGET;
    else if (resource->r.type == RESOURCE_TYPE_DEPTH_STENCIL)
        state = D3D12_RESOURCE_STATE_DEPTH_WRITE;
    else
        state = D3D12_RESOURCE_STATE_UNORDERED_ACCESS;

    get_resource_readback_with_command_list_and_states(resource->resource, 0, rb,
            test_context->queue, test_context->list, state, state);
    reset_command_list(test_context->list, test_context->allocator);

    return &rb->rb;
}

static void d3d12_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct d3d12_resource_readback *d3d12_rb = CONTAINING_RECORD(rb, struct d3d12_resource_readback, rb);

    release_resource_readback(d3d12_rb);
    free(d3d12_rb);
}

static const struct shader_runner_ops d3d12_runner_ops =
{
    .create_resource = d3d12_runner_create_resource,
    .destroy_resource = d3d12_runner_destroy_resource,
    .dispatch = d3d12_runner_dispatch,
    .clear = d3d12_runner_clear,
    .draw = d3d12_runner_draw,
    .get_resource_readback = d3d12_runner_get_resource_readback,
    .release_readback = d3d12_runner_release_readback,
};

static void d3d12_runner_init_caps(struct d3d12_shader_runner *runner)
{
    ID3D12Device *device = runner->test_context.device;
    D3D12_FEATURE_DATA_D3D12_OPTIONS1 options1;
    D3D12_FEATURE_DATA_D3D12_OPTIONS options;
    HRESULT hr;

    hr = ID3D12Device_CheckFeatureSupport(device, D3D12_FEATURE_D3D12_OPTIONS, &options, sizeof(options));
    ok(hr == S_OK, "Failed to check feature options support, hr %#x.\n", hr);
    hr = ID3D12Device_CheckFeatureSupport(device, D3D12_FEATURE_D3D12_OPTIONS1, &options1, sizeof(options1));
    ok(hr == S_OK, "Failed to check feature options1 support, hr %#x.\n", hr);

#ifdef VKD3D_CROSSTEST
    runner->caps.runner = "d3d12.dll";
#else
    runner->caps.runner = "vkd3d";
#endif
    runner->caps.minimum_shader_model = SHADER_MODEL_4_0;
    runner->caps.maximum_shader_model = SHADER_MODEL_5_1;
    runner->caps.float64 = options.DoublePrecisionFloatShaderOps;
    runner->caps.int64 = options1.Int64ShaderOps;
    runner->caps.rov = options.ROVsSupported;
}

static bool device_supports_shader_model_6_0(ID3D12Device *device)
{
    D3D12_FEATURE_DATA_SHADER_MODEL sm = {D3D_SHADER_MODEL_6_0};
    HRESULT hr;

    hr = ID3D12Device_CheckFeatureSupport(device, D3D12_FEATURE_SHADER_MODEL, &sm, sizeof(sm));
    ok(hr == S_OK, "Failed to check feature shader model support, hr %#x.\n", hr);
    return sm.HighestShaderModel >= D3D_SHADER_MODEL_6_0;
}

void run_shader_tests_d3d12(void *dxc_compiler)
{
    static const struct test_context_desc desc =
    {
        .rt_width = RENDER_TARGET_WIDTH,
        .rt_height = RENDER_TARGET_HEIGHT,
        .no_root_signature = true,
        .rt_format = DXGI_FORMAT_R32G32B32A32_FLOAT,
    };
    struct d3d12_shader_runner runner = {0};
    ID3D12Device *device;
    HRESULT hr;

    enable_d3d12_debug_layer();
    init_adapter_info();
    if (!init_test_context(&runner.test_context, &desc))
        return;
    d3d12_runner_init_caps(&runner);

    device = runner.test_context.device;

    runner.dxc_compiler = dxc_compiler;

    runner.compute_queue = create_command_queue(device,
            D3D12_COMMAND_LIST_TYPE_COMPUTE, D3D12_COMMAND_QUEUE_PRIORITY_NORMAL);

    hr = ID3D12Device_CreateCommandAllocator(device, D3D12_COMMAND_LIST_TYPE_COMPUTE,
            &IID_ID3D12CommandAllocator, (void **)&runner.compute_allocator);
    ok(hr == S_OK, "Failed to create command allocator, hr %#x.\n", hr);

    hr = ID3D12Device_CreateCommandList(device, 0, D3D12_COMMAND_LIST_TYPE_COMPUTE,
            runner.compute_allocator, NULL, &IID_ID3D12GraphicsCommandList, (void **)&runner.compute_list);
    ok(hr == S_OK, "Failed to create command list, hr %#x.\n", hr);

    run_shader_tests(&runner.r, &runner.caps, &d3d12_runner_ops, NULL);
    if (dxc_compiler)
    {
        runner.caps.minimum_shader_model = SHADER_MODEL_6_0;
        runner.caps.maximum_shader_model = SHADER_MODEL_6_0;

        if (device_supports_shader_model_6_0(device))
            run_shader_tests(&runner.r, &runner.caps, &d3d12_runner_ops, dxc_compiler);
        else
            trace("Device does not support shader model 6.0.\n");
    }

    ID3D12GraphicsCommandList_Release(runner.compute_list);
    ID3D12CommandAllocator_Release(runner.compute_allocator);
    ID3D12CommandQueue_Release(runner.compute_queue);
    if (runner.heap)
        ID3D12DescriptorHeap_Release(runner.heap);
    if (runner.rtv_heap)
        ID3D12DescriptorHeap_Release(runner.rtv_heap);
    if (runner.dsv_heap)
        ID3D12DescriptorHeap_Release(runner.dsv_heap);
    destroy_test_context(&runner.test_context);
}
