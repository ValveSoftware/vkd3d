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
