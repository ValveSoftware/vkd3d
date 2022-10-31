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
