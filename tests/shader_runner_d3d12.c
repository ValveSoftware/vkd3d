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

#ifdef __MINGW32__
# define _HRESULT_DEFINED
typedef int HRESULT;
#endif

#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
#include "vkd3d_windows.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3dcompiler.h"
#include "vkd3d_test.h"
#include "shader_runner.h"

static ID3D10Blob *d3d12_runner_compile_shader(const char *source, enum shader_model shader_model)
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

static const struct shader_runner_ops d3d12_runner_ops =
{
    .compile_shader = d3d12_runner_compile_shader,
};

void run_shader_tests_d3d12(int argc, char **argv)
{
    run_shader_tests(argc, argv, &d3d12_runner_ops);
}
