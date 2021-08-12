/*
 * Copyright 2020-2022 Zebediah Figura for CodeWeavers
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

#ifdef __MINGW32__
# define _HRESULT_DEFINED
typedef int HRESULT;
#endif

#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
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

static void compile_shader(struct shader_runner *runner, const char *source, const char *type, HRESULT expect)
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
    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", profile, 0, 0, &blob, &errors);
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

void shader_runner_create_resource(struct shader_runner *runner, const struct resource_params *params)
{
    struct resource *resource = runner->ops->create_resource(runner, params);
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

void shader_runner_compile_cs(struct shader_runner *runner, const char *source, HRESULT expect_hr)
{
    compile_shader(runner, source, "cs", expect_hr);
    free(runner->cs_source);
    runner->cs_source = strdup(source);
}

void shader_runner_compile_ps(struct shader_runner *runner, const char *source, HRESULT expect_hr)
{
    compile_shader(runner, source, "ps", expect_hr);
    free(runner->ps_source);
    runner->ps_source = strdup(source);
}

void shader_runner_compile_vs(struct shader_runner *runner, const char *source, HRESULT expect_hr)
{
    compile_shader(runner, source, "vs", expect_hr);
    free(runner->vs_source);
    runner->vs_source = strdup(source);
}

void shader_runner_dispatch(struct shader_runner *runner, unsigned int x, unsigned int y, unsigned int z)
{
    runner->last_render_failed = !runner->ops->dispatch(runner, x, y, z);
}

void shader_runner_draw(struct shader_runner *runner, D3D_PRIMITIVE_TOPOLOGY topology, unsigned int vertex_count)
{
    if (!get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, 0))
    {
        struct resource_params params = {0};

        params.slot = 0;
        params.type = RESOURCE_TYPE_RENDER_TARGET;
        params.format = DXGI_FORMAT_R32G32B32A32_FLOAT;
        params.data_type = TEXTURE_DATA_FLOAT;
        params.texel_size = 16;
        params.width = RENDER_TARGET_WIDTH;
        params.height = RENDER_TARGET_HEIGHT;

        shader_runner_create_resource(runner, &params);
    }

    runner->last_render_failed = !runner->ops->draw(runner, topology, vertex_count);
}

void shader_runner_draw_quad(struct shader_runner *runner)
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
    shader_runner_create_resource(runner, &params);

    if (!runner->vs_source)
        shader_runner_compile_vs(runner, vs_source, S_OK);

    shader_runner_draw(runner, D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST, 3);
}

struct resource_readback *shader_runner_get_rt_readback(struct shader_runner *runner, unsigned int slot)
{
    return runner->ops->get_resource_readback(runner, get_resource(runner, RESOURCE_TYPE_RENDER_TARGET, slot));
}

struct resource_readback *shader_runner_get_uav_readback(struct shader_runner *runner, unsigned int slot)
{
    return runner->ops->get_resource_readback(runner, get_resource(runner, RESOURCE_TYPE_UAV, slot));
}

void shader_runner_release_readback(struct shader_runner *runner, struct resource_readback *rb)
{
    runner->ops->release_readback(runner, rb);
}

void shader_runner_run(shader_runner_frontend_func func, int argc, char **argv)
{
#if defined(VKD3D_CROSSTEST)
    trace("Running tests from a Windows cross build\n");

    trace("Compiling shaders with d3dcompiler_47.dll and executing with d3d9.dll\n");
    run_shader_tests_d3d9(func, argc, argv);

    trace("Compiling shaders with d3dcompiler_47.dll and executing with d3d11.dll\n");
    run_shader_tests_d3d11(func, argc, argv);

    trace("Compiling shaders with d3dcompiler_47.dll and executing with d3d12.dll\n");
    run_shader_tests_d3d12(func, argc, argv);

    print_dll_version("d3dcompiler_47.dll");
    print_dll_version("dxgi.dll");
    print_dll_version("d3d9.dll");
    print_dll_version("d3d11.dll");
    print_dll_version("d3d12.dll");
#elif defined(_WIN32)
    trace("Running tests from a Windows non-cross build\n");

    trace("Compiling shaders with vkd3d-shader and executing with d3d9.dll\n");
    run_shader_tests_d3d9(func, argc, argv);

    trace("Compiling shaders with vkd3d-shader and executing with d3d11.dll\n");
    run_shader_tests_d3d11(func, argc, argv);

    trace("Compiling shaders with vkd3d-shader and executing with vkd3d\n");
    run_shader_tests_d3d12(func, argc, argv);

    print_dll_version("d3d9.dll");
    print_dll_version("d3d11.dll");
#else
    trace("Running tests from a Unix build\n");

    trace("Compiling shaders with vkd3d-shader and executing with Vulkan\n");
    run_shader_tests_vulkan(func, argc, argv);

    trace("Compiling shaders with vkd3d-shader and executing with vkd3d\n");
    run_shader_tests_d3d12(func, argc, argv);
#endif
}