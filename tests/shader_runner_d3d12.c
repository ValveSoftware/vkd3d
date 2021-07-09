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

#define VKD3D_TEST_NO_DEFS
#include "d3d12_crosstest.h"
#include "shader_runner.h"

static void d3d12_runner_draw_quad(void *private, const struct draw_params *params)
{
    struct test_context *context = private;

    D3D12_SHADER_BYTECODE ps = {params->ps_code, params->ps_code_size};
    ID3D12GraphicsCommandList *command_list = context->list;
    static const float clear_color[4];
    ID3D12PipelineState *pso;

    if (context->root_signature)
        ID3D12RootSignature_Release(context->root_signature);
    context->root_signature = create_32bit_constants_root_signature(context->device,
            0, params->ps_uniform_count, D3D12_SHADER_VISIBILITY_ALL);

    pso = create_pipeline_state(context->device, context->root_signature,
            context->render_target_desc.Format, NULL, &ps, NULL);
    if (!pso)
        return;

    ID3D12GraphicsCommandList_SetGraphicsRootSignature(command_list, context->root_signature);
    ID3D12GraphicsCommandList_SetGraphicsRoot32BitConstants(command_list, 0,
            params->ps_uniform_count, params->ps_uniforms, 0);
    ID3D12GraphicsCommandList_OMSetRenderTargets(command_list, 1, &context->rtv, false, NULL);
    ID3D12GraphicsCommandList_RSSetScissorRects(command_list, 1, &context->scissor_rect);
    ID3D12GraphicsCommandList_RSSetViewports(command_list, 1, &context->viewport);
    ID3D12GraphicsCommandList_IASetPrimitiveTopology(command_list, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    ID3D12GraphicsCommandList_ClearRenderTargetView(command_list, context->rtv, clear_color, 0, NULL);
    ID3D12GraphicsCommandList_SetPipelineState(command_list, pso);
    ID3D12GraphicsCommandList_DrawInstanced(command_list, 3, 1, 0, 0);
    ID3D12PipelineState_Release(pso);
    transition_resource_state(command_list, context->render_target,
            D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_COPY_SOURCE);
}

static void d3d12_runner_probe_vec4(void *private, const RECT *rect, const struct vec4 *v, unsigned int ulps)
{
    struct test_context *context = private;
    struct resource_readback rb;

    get_texture_readback_with_command_list(context->render_target, 0, &rb, context->queue, context->list);
    check_readback_data_vec4(&rb, rect, v, ulps);
    release_resource_readback(&rb);
    reset_command_list(context->list, context->allocator);
}

static const struct shader_runner_ops d3d12_runner_ops =
{
    .ps_profile = "ps_4_0",
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
    struct test_context test_context;

    parse_args(argc, argv);
    enable_d3d12_debug_layer(argc, argv);
    init_adapter_info();
    init_test_context(&test_context, &desc);

    run_shader_tests(argc, argv, &d3d12_runner_ops, &test_context);

    destroy_test_context(&test_context);
}
