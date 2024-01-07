/*
 * Copyright (C) 2020 Zebediah Figura for CodeWeavers
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
#include "d3d12_crosstest.h"
#include "vkd3d_common.h"
#include "vkd3d_d3d12shader.h"

#ifndef D3DERR_INVALIDCALL
#define D3DERR_INVALIDCALL 0x8876086c
#endif

struct test_options test_options = {0};

#define check_preprocess(a, b, c, d, e) check_preprocess_(__LINE__, a, b, c, d, e)
static void check_preprocess_(int line, const char *source, const D3D_SHADER_MACRO *macros,
        ID3DInclude *include, const char *present, const char *absent)
{
    ID3D10Blob *blob, *errors;
    const char *code;
    SIZE_T size;
    HRESULT hr;

    hr = D3DPreprocess(source, strlen(source), NULL, macros, include, &blob, &errors);
    assert_that_(line)(hr == S_OK, "Failed to preprocess shader, hr %#x.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace_(line)("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    code = ID3D10Blob_GetBufferPointer(blob);
    size = ID3D10Blob_GetBufferSize(blob);
    if (present)
        ok_(line)(vkd3d_memmem(code, size, present, strlen(present)),
                "\"%s\" not found in preprocessed shader.\n", present);
    if (absent)
        ok_(line)(!vkd3d_memmem(code, size, absent, strlen(absent)),
                "\"%s\" found in preprocessed shader.\n", absent);
    ID3D10Blob_Release(blob);
}

static const char test_include_top[] =
    "#include \"file1\"\n"
    "#include < file2 >\n"
    "ARGES\n";

static const char test_include_file1[] =
    "#define BRONTES\n"
    "#include \"file3\"\n"
    "#ifndef BRONTES\n"
    "#define STEROPES\n"
    "#endif";

static const char test_include_file2[] =
    "#ifdef STEROPES\n"
    "#define ARGES pass\n"
    "#undef STEROPES\n"
    "#include < file2 >\n"
    "#endif";

static const char test_include_file3[] =
    "#undef BRONTES";

static const char test_include2_top[] =
    "#define FUNC(a, b) a ## b\n"
    "#include \"file4\"\n"
    ",ss)";

static const char test_include2_file4[] =
    "FUNC(pa";

static unsigned int refcount_file1, refcount_file2, refcount_file3, include_count_file2;

static HRESULT WINAPI test_include_Open(ID3DInclude *iface, D3D_INCLUDE_TYPE type,
        const char *filename, const void *parent_data, const void **code, UINT *size)
{
    ok(!*code, "Data pointer should be zeroed.\n");
    ok(!*size, "Size pointer should be zeroed.\n");
    if (!strcmp(filename, "file1"))
    {
        ok(type == D3D_INCLUDE_LOCAL, "Got type %#x.\n", type);
        ok(!parent_data, "Got parent data %p.\n", parent_data);
        *code = test_include_file1;
        *size = strlen(test_include_file1);
        ++refcount_file1;
    }
    else if (!strcmp(filename, " file2 "))
    {
        ok(type == D3D_INCLUDE_SYSTEM, "Got type %#x.\n", type);
        if (!include_count_file2++)
            ok(!parent_data, "Got parent data %p.\n", parent_data);
        else
            ok(parent_data == test_include_file2, "Got parent data %p.\n", parent_data);
        *code = test_include_file2;
        *size = strlen(test_include_file2);
        ++refcount_file2;
    }
    else if (!strcmp(filename, "file3"))
    {
        ok(type == D3D_INCLUDE_LOCAL, "Got type %#x.\n", type);
        ok(parent_data == test_include_file1, "Got parent data %p.\n", parent_data);
        *code = test_include_file3;
        *size = strlen(test_include_file3);
        ++refcount_file3;
    }
    else if (!strcmp(filename, "file4"))
    {
        ok(type == D3D_INCLUDE_LOCAL, "Got type %#x.\n", type);
        ok(!parent_data, "Got parent data %p.\n", parent_data);
        *code = test_include2_file4;
        *size = strlen(test_include2_file4);
    }
    else
    {
        ok(0, "Unexpected filename \"%s\".\n", filename);
    }
    return S_FALSE;
}

static HRESULT WINAPI test_include_Close(ID3DInclude *iface, const void *code)
{
    if (code == test_include_file1)
        --refcount_file1;
    else if (code == test_include_file2)
        --refcount_file2;
    else if (code == test_include_file3)
        --refcount_file3;
    return E_FAIL;
}

static const struct ID3DIncludeVtbl test_include_vtbl =
{
    test_include_Open,
    test_include_Close,
};

static HRESULT WINAPI test_include_fail_Open(ID3DInclude *iface, D3D_INCLUDE_TYPE type,
        const char *filename, const void *parent_data, const void **code, UINT *size)
{
    return 0xdeadbeef;
}

static HRESULT WINAPI test_include_fail_Close(ID3DInclude *iface, const void *code)
{
    ok(0, "Unexpected call.\n");
    return E_FAIL;
}

static const struct ID3DIncludeVtbl test_include_fail_vtbl =
{
    test_include_fail_Open,
    test_include_fail_Close,
};

static void test_preprocess(void)
{
    ID3DInclude test_include_fail = {&test_include_fail_vtbl};
    ID3DInclude test_include = {&test_include_vtbl};
    D3D_SHADER_MACRO macros[3];
    ID3D10Blob *blob, *errors;
    unsigned int i;
    HRESULT hr;

    static const struct
    {
        const char *source;
        const char *present;
        const char *absent;
    }
    tests[] =
    {
        /* Stringification. */
        {
            "#define KEY(a) #a\n"
            "KEY(apple)",

            "\"apple\"",
        },
        {
            "#define KEY(a) # a\n"
            "KEY(apple)",

            "\"apple\"",
        },
        {
            "#define KEY(a) #a\n"
            "KEY( \t\r\n apple \t\r\n )",

            "\"apple\"",
        },
        {
            "#define KEY(if) #if\n"
            "KEY(apple)",

            "\"apple\"",
        },
        {
            "#define KEY(a) #a\n"
            "KEY(\"apple\")",

            "\"\\\"apple\\\"\"",
        },
        {
            "#define KEY(a) #b\n"
            "KEY(apple)",

            "\"b\"",
        },
        {
            "#define KEY(a) a\n"
            "KEY(banana #apple)",

            "#",
            "\"apple\"",
        },
        {
            "#define KEY #apple\n"
            "KEY",

            "apple",
            "\"apple\"",
        },
        {
            "banana #apple\n",

            "apple",
            "\"apple\"",
        },
        {
            "banana #apple\n",

            "#",
        },
        {
            "#define KEY2(x) x\n"
            "#define KEY(a) KEY2(#a)\n"
            "KEY(apple)",

            "\"apple\"",
        },
        {
            "#define KEY2(x) #x\n"
            "#define KEY(a) KEY2(#a)\n"
            "KEY(apple)",

            "\"\\\"apple\\\"\"",
        },
        {
            "#define KEY2(x) #x\n"
            "#define KEY(a) KEY2(#x)\n"
            "KEY(apple)",

            "\"\\\"x\\\"\"",
        },

        /* #pragma is preserved. */
        {
            "#pragma pack_matrix(column_major)\n"
            "text",

            "#pragma pack_matrix(column_major)\n",
        },

        /* DOS-style newlines. */
        {
            "#define KEY(a, b) \\\r\n"
            "        a ## b\r\n"
            "KEY(pa,\r\n"
            "ss\r\n"
            ")\r\n"
            "#ifndef KEY\r\n"
            "fail\r\n"
            "#endif\r\n",

            "pass",
            "fail",
        },
        {
            "#define KEY(a, b) \\\r\n"
            "        a ## b\n"
            "KEY(pa,\r\n"
            "ss\n"
            ")\r\n"
            "#ifndef KEY\n"
            "fail\r\n"
            "#endif\n",

            "pass",
            "fail",
        },

        /* Pre-defined macros. */
        {
            "__LINE__",
            "1",
            "__LINE__",
        },
        {
            "\n"
            "__LINE__",
            "2",
            "__LINE__",
        },
        {
            "#define KEY __LINE__\n"
            "KEY",
            "2",
        },

        /* Tokens which must be preserved verbatim for HLSL (i.e. which cannot
         * be broken up with spaces). */
        {"<<", "<<"},
        {">>", ">>"},
        {"++", "++"},
        {"--", "--"},
        {"+=", "+="},
        {"-=", "-="},
        {"*=", "*="},
        {"/=", "/="},
        {"%=", "%="},
        {"&=", "&="},
        {"|=", "|="},
        {"^=", "^="},
        {"<<=", "<<="},
        {">>=", ">>="},
        {"0.0", "0.0"},
        {".0", ".0"},
        {"0.", "0."},
        {"1e1", "1e1"},
        {"1E1", "1E1"},
        {"1e+1", "1e+1"},
        {"1e-1", "1e-1"},
        {".0f", ".0f"},
        {".0F", ".0F"},
        {".0h", ".0h"},
        {".0H", ".0H"},
        {"0.f", "0.f"},
        {"1.1e-1f", "1.1e-1f"},

        /* Parentheses are emitted for object-like macros invoked like
         * function-like macros. */
        {
            "#define KEY value\n"
            "KEY(apple)",

            "(",
        },

        /* Function-like macro with no parentheses and no following tokens (a
         * corner case in our implementation). */
        {
            "#define pass(a) fail\n"
            "pass",

            "pass",
            "fail",
        },

        /* A single-line comment not terminated by a newline. */
        {
            "pass // fail",

            "pass",
            "fail",
        },
    };

    for (i = 0; i < ARRAY_SIZE(tests); ++i)
    {
        vkd3d_test_push_context("Source \"%s\"", tests[i].source);
        check_preprocess(tests[i].source, NULL, NULL, tests[i].present, tests[i].absent);
        vkd3d_test_pop_context();
    }

    macros[0].Name = "KEY";
    macros[0].Definition = "value";
    macros[1].Name = NULL;
    macros[1].Definition = NULL;
    check_preprocess("KEY", macros, NULL, "value", "KEY");

    check_preprocess("#undef KEY\nKEY", macros, NULL, "KEY", "value");

    macros[0].Name = NULL;
    check_preprocess("KEY", macros, NULL, "KEY", "value");

    macros[0].Name = "KEY";
    macros[0].Definition = NULL;
    check_preprocess("KEY", macros, NULL, NULL, "KEY");

    macros[0].Name = "0";
    macros[0].Definition = "value";
    check_preprocess("0", macros, NULL, "0", "value");

    macros[0].Name = "KEY(a)";
    macros[0].Definition = "value";
    check_preprocess("KEY(a)", macros, NULL, "KEY", "value");

    macros[0].Name = "KEY";
    macros[0].Definition = "value1";
    macros[1].Name = "KEY";
    macros[1].Definition = "value2";
    macros[2].Name = NULL;
    macros[2].Definition = NULL;
    check_preprocess("KEY", macros, NULL, "value2", NULL);

    macros[0].Name = "KEY";
    macros[0].Definition = "KEY2";
    macros[1].Name = "KEY2";
    macros[1].Definition = "value";
    check_preprocess("KEY", macros, NULL, "value", NULL);

    macros[0].Name = "KEY2";
    macros[0].Definition = "value";
    macros[1].Name = "KEY";
    macros[1].Definition = "KEY2";
    check_preprocess("KEY", macros, NULL, "value", NULL);

    check_preprocess(test_include_top, NULL, &test_include, "pass", "fail");
    ok(!refcount_file1, "Got %d references to file1.\n", refcount_file1);
    ok(!refcount_file2, "Got %d references to file1.\n", refcount_file2);
    ok(!refcount_file3, "Got %d references to file1.\n", refcount_file3);
    ok(include_count_file2 == 2, "file2 was included %u times.\n", include_count_file2);

    /* Macro invocation spread across multiple files. */
    check_preprocess(test_include2_top, NULL, &test_include, "pass", NULL);

    blob = errors = (ID3D10Blob *)0xdeadbeef;
    hr = D3DPreprocess(test_include_top, strlen(test_include_top), NULL, NULL, &test_include_fail, &blob, &errors);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);
    ok(blob == (ID3D10Blob *)0xdeadbeef, "Expected no compiled shader blob.\n");
    ok(!!errors, "Expected non-NULL error blob.\n");
    if (vkd3d_test_state.debug_level)
        trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
    ID3D10Blob_Release(errors);
}

#define compile_shader(a, b) compile_shader_(__LINE__, a, b, 0)
#define compile_shader_flags(a, b, c) compile_shader_(__LINE__, a, b, c)
static ID3D10Blob *compile_shader_(unsigned int line, const char *source, const char *target, UINT flags)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    HRESULT hr;

    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", target, flags, 0, &blob, &errors);
    ok_(line)(hr == S_OK, "Failed to compile shader, hr %#x.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace_(line)("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

static void test_thread_id(void)
{
    ID3D12GraphicsCommandList *command_list;
    struct d3d12_resource_readback rb;
    struct test_context context;
    ID3D12Resource *textures[3];
    ID3D12DescriptorHeap *heap;
    unsigned int i, x, y, z;
    ID3D12Device *device;
    ID3D10Blob *cs_code;
    HRESULT hr;

    static const char cs_source[] =
        "RWTexture3D<uint4> group_uav, thread_uav, dispatch_uav;\n"
        "[numthreads(5, 3, 2)]\n"
        "void main(uint3 group : sv_groupid, uint3 thread : sv_groupthreadid, uint3 dispatch : sv_dispatchthreadid)\n"
        "{\n"
        "    group_uav[dispatch] = uint4(group, 1);\n"
        "    thread_uav[dispatch] = uint4(thread, 2);\n"
        "    dispatch_uav[dispatch] = uint4(dispatch, 3);\n"
        "}";

    static const D3D12_DESCRIPTOR_RANGE descriptor_range = {D3D12_DESCRIPTOR_RANGE_TYPE_UAV, 3, 0, 0, 0};

    static const D3D12_ROOT_PARAMETER root_parameter =
    {
        .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
        .DescriptorTable = {1, &descriptor_range},
    };

    static const D3D12_ROOT_SIGNATURE_DESC root_signature_desc =
    {
        .NumParameters = 1,
        .pParameters = &root_parameter,
    };

    if (!init_compute_test_context(&context))
        return;
    command_list = context.list;
    device = context.device;

    hr = create_root_signature(device, &root_signature_desc, &context.root_signature);
    ok(hr == S_OK, "Failed to create root signature, hr %#x.\n", hr);

    heap = create_gpu_descriptor_heap(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, 3);

    for (i = 0; i < 3; ++i)
    {
        const UINT clear_value[4] = {0};

        textures[i] = create_default_texture3d(device, 16, 8, 8, 1, DXGI_FORMAT_R32G32B32A32_UINT,
                D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
        ID3D12Device_CreateUnorderedAccessView(device, textures[i], NULL, NULL,
                get_cpu_descriptor_handle(&context, heap, i));
        ID3D12GraphicsCommandList_ClearUnorderedAccessViewUint(command_list,
                get_gpu_descriptor_handle(&context, heap, i),
                get_cpu_descriptor_handle(&context, heap, i),
                textures[i], clear_value, 0, NULL);
        uav_barrier(command_list, textures[i]);
    }

    cs_code = compile_shader(cs_source, "cs_5_0");
    context.pipeline_state = create_compute_pipeline_state(device, context.root_signature,
            shader_bytecode(ID3D10Blob_GetBufferPointer(cs_code), ID3D10Blob_GetBufferSize(cs_code)));
    ID3D10Blob_Release(cs_code);

    ID3D12GraphicsCommandList_SetPipelineState(command_list, context.pipeline_state);
    ID3D12GraphicsCommandList_SetComputeRootSignature(command_list, context.root_signature);
    ID3D12GraphicsCommandList_SetComputeRootDescriptorTable(command_list,
            0, get_gpu_descriptor_handle(&context, heap, 0));
    ID3D12GraphicsCommandList_Dispatch(command_list, 2, 2, 2);

    transition_resource_state(command_list, textures[0],
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE);
    get_resource_readback_with_command_list(textures[0], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb.rb, x, y, z, sizeof(struct uvec4));
                struct uvec4 expect = {x / 5, y / 3, z / 2, 1};

                if (x >= 10 || y >= 6 || z >= 4)
                    memset(&expect, 0, sizeof(expect));

                ok(compare_uvec4(v, &expect), "Got {%u, %u, %u, %u} at (%u, %u, %u).\n",
                        v->x, v->y, v->z, v->w, x, y, z);
            }
        }
    }
    release_resource_readback(&rb);
    reset_command_list(command_list, context.allocator);

    transition_resource_state(command_list, textures[1],
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE);
    get_resource_readback_with_command_list(textures[1], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb.rb, x, y, z, sizeof(struct uvec4));
                struct uvec4 expect = {x % 5, y % 3, z % 2, 2};

                if (x >= 10 || y >= 6 || z >= 4)
                    memset(&expect, 0, sizeof(expect));

                ok(compare_uvec4(v, &expect), "Got {%u, %u, %u, %u} at (%u, %u, %u).\n",
                        v->x, v->y, v->z, v->w, x, y, z);
            }
        }
    }
    release_resource_readback(&rb);
    reset_command_list(command_list, context.allocator);

    transition_resource_state(command_list, textures[2],
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE);
    get_resource_readback_with_command_list(textures[2], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb.rb, x, y, z, sizeof(struct uvec4));
                struct uvec4 expect = {x, y, z, 3};

                if (x >= 10 || y >= 6 || z >= 4)
                    memset(&expect, 0, sizeof(expect));

                ok(compare_uvec4(v, &expect), "Got {%u, %u, %u, %u} at (%u, %u, %u).\n",
                        v->x, v->y, v->z, v->w, x, y, z);
            }
        }
    }
    release_resource_readback(&rb);
    reset_command_list(command_list, context.allocator);

    for (i = 0; i < 3; ++i)
        ID3D12Resource_Release(textures[i]);
    ID3D12DescriptorHeap_Release(heap);
    destroy_test_context(&context);
}

static void test_create_blob(void)
{
    unsigned int refcount;
    ID3D10Blob *blob;
    HRESULT hr;

    hr = D3DCreateBlob(1, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DCreateBlob(0, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DCreateBlob(0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);
}

static void test_get_blob_part(void)
{
    static uint32_t test_blob_part[] =
    {
        /* test_blob_part - fxc.exe /E VS /Tvs_4_0_level_9_0 /Fx */
#if 0
        float4 VS(float4 position : POSITION, float4 pos : SV_POSITION) : SV_POSITION
        {
          return position;
        }
#endif
        0x43425844, 0x0ef2a70f, 0x6a548011, 0x91ff9409, 0x9973a43d, 0x00000001, 0x000002e0, 0x00000008,
        0x00000040, 0x0000008c, 0x000000d8, 0x0000013c, 0x00000180, 0x000001fc, 0x00000254, 0x000002ac,
        0x53414e58, 0x00000044, 0x00000044, 0xfffe0200, 0x00000020, 0x00000024, 0x00240000, 0x00240000,
        0x00240000, 0x00240000, 0x00240000, 0xfffe0200, 0x0200001f, 0x80000005, 0x900f0000, 0x02000001,
        0xc00f0000, 0x80e40000, 0x0000ffff, 0x50414e58, 0x00000044, 0x00000044, 0xfffe0200, 0x00000020,
        0x00000024, 0x00240000, 0x00240000, 0x00240000, 0x00240000, 0x00240000, 0xfffe0200, 0x0200001f,
        0x80000005, 0x900f0000, 0x02000001, 0xc00f0000, 0x80e40000, 0x0000ffff, 0x396e6f41, 0x0000005c,
        0x0000005c, 0xfffe0200, 0x00000034, 0x00000028, 0x00240000, 0x00240000, 0x00240000, 0x00240000,
        0x00240001, 0x00000000, 0xfffe0200, 0x0200001f, 0x80000005, 0x900f0000, 0x04000004, 0xc0030000,
        0x90ff0000, 0xa0e40000, 0x90e40000, 0x02000001, 0xc00c0000, 0x90e40000, 0x0000ffff, 0x52444853,
        0x0000003c, 0x00010040, 0x0000000f, 0x0300005f, 0x001010f2, 0x00000000, 0x04000067, 0x001020f2,
        0x00000000, 0x00000001, 0x05000036, 0x001020f2, 0x00000000, 0x00101e46, 0x00000000, 0x0100003e,
        0x54415453, 0x00000074, 0x00000002, 0x00000000, 0x00000000, 0x00000002, 0x00000000, 0x00000000,
        0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x46454452,
        0x00000050, 0x00000000, 0x00000000, 0x00000000, 0x0000001c, 0xfffe0400, 0x00000100, 0x0000001c,
        0x7263694d, 0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d,
        0x39207265, 0x2e39322e, 0x2e323539, 0x31313133, 0xababab00, 0x4e475349, 0x00000050, 0x00000002,
        0x00000008, 0x00000038, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x00000041,
        0x00000000, 0x00000000, 0x00000003, 0x00000001, 0x0000000f, 0x49534f50, 0x4e4f4954, 0x5f565300,
        0x49534f50, 0x4e4f4954, 0xababab00, 0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020,
        0x00000000, 0x00000001, 0x00000003, 0x00000000, 0x0000000f, 0x505f5653, 0x5449534f, 0x004e4f49,
    };

    static uint32_t test_blob_part2[] =
    {
        /* test_blob_part2 - fxc.exe /E BHS /Ths_5_0 /Zi */
#if 0
        struct VSO { float3 p : POSITION; };
        struct HSDO { float e[4] : SV_TessFactor; float i[2] : SV_InsideTessFactor; };
        struct HSO { float3 p : BEZIERPOS; };
        HSDO BCHS( InputPatch<VSO, 8> ip, uint PatchID : SV_PrimitiveID )
        {
            HSDO res;
            res.e[0] = res.e[1] = res.e[2] = res.e[3] = 1.0f;
            res.i[0] = res.i[1] = 1.0f;
            return res;
        }
        [domain("quad")]
        [partitioning("integer")]
        [outputtopology("triangle_cw")]
        [outputcontrolpoints(8)]
        [patchconstantfunc("BCHS")]
        HSO BHS( InputPatch<VSO, 8> p, uint i : SV_OutputControlPointID, uint PatchID : SV_PrimitiveID )
        {
            HSO res;
            res.p = p[i].p;
            return res;
        }
#endif
        0x43425844, 0xa9d455ae, 0x4cf9c0df, 0x4cf806dc, 0xc57a8c2c, 0x00000001, 0x0000139b, 0x00000007,
        0x0000003c, 0x000000b4, 0x000000e8, 0x0000011c, 0x000001e0, 0x00000320, 0x000003bc, 0x46454452,
        0x00000070, 0x00000000, 0x00000000, 0x00000000, 0x0000003c, 0x48530500, 0x00000101, 0x0000003c,
        0x31314452, 0x0000003c, 0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000,
        0x7263694d, 0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d,
        0x39207265, 0x2e39322e, 0x2e323539, 0x31313133, 0xababab00, 0x4e475349, 0x0000002c, 0x00000001,
        0x00000008, 0x00000020, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000707, 0x49534f50,
        0x4e4f4954, 0xababab00, 0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000,
        0x00000000, 0x00000003, 0x00000000, 0x00000807, 0x495a4542, 0x4f505245, 0xabab0053, 0x47534350,
        0x000000bc, 0x00000006, 0x00000008, 0x00000098, 0x00000000, 0x0000000b, 0x00000003, 0x00000000,
        0x00000e01, 0x00000098, 0x00000001, 0x0000000b, 0x00000003, 0x00000001, 0x00000e01, 0x00000098,
        0x00000002, 0x0000000b, 0x00000003, 0x00000002, 0x00000e01, 0x00000098, 0x00000003, 0x0000000b,
        0x00000003, 0x00000003, 0x00000e01, 0x000000a6, 0x00000000, 0x0000000c, 0x00000003, 0x00000004,
        0x00000e01, 0x000000a6, 0x00000001, 0x0000000c, 0x00000003, 0x00000005, 0x00000e01, 0x545f5653,
        0x46737365, 0x6f746361, 0x56530072, 0x736e495f, 0x54656469, 0x46737365, 0x6f746361, 0xabab0072,
        0x58454853, 0x00000138, 0x00030050, 0x0000004e, 0x01000071, 0x01004093, 0x01004094, 0x01001895,
        0x01000896, 0x01001897, 0x0100086a, 0x01000073, 0x02000099, 0x00000004, 0x0200005f, 0x00017000,
        0x04000067, 0x00102012, 0x00000000, 0x0000000b, 0x04000067, 0x00102012, 0x00000001, 0x0000000c,
        0x04000067, 0x00102012, 0x00000002, 0x0000000d, 0x04000067, 0x00102012, 0x00000003, 0x0000000e,
        0x02000068, 0x00000001, 0x0400005b, 0x00102012, 0x00000000, 0x00000004, 0x04000036, 0x00100012,
        0x00000000, 0x0001700a, 0x06000036, 0x00902012, 0x0010000a, 0x00000000, 0x00004001, 0x3f800000,
        0x0100003e, 0x01000073, 0x02000099, 0x00000002, 0x0200005f, 0x00017000, 0x04000067, 0x00102012,
        0x00000004, 0x0000000f, 0x04000067, 0x00102012, 0x00000005, 0x00000010, 0x02000068, 0x00000001,
        0x0400005b, 0x00102012, 0x00000004, 0x00000002, 0x04000036, 0x00100012, 0x00000000, 0x0001700a,
        0x07000036, 0x00d02012, 0x00000004, 0x0010000a, 0x00000000, 0x00004001, 0x3f800000, 0x0100003e,
        0x54415453, 0x00000094, 0x00000006, 0x00000001, 0x00000000, 0x00000005, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x0000000f, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000008, 0x00000003, 0x00000001, 0x00000003, 0x00000000, 0x00000000, 0x00000000, 0x47424453,
        0x00000fd7, 0x00000054, 0x000002d5, 0x00000306, 0x0000030a, 0x00000101, 0x00000001, 0x00000000,
        0x00000006, 0x00000010, 0x00000006, 0x00000958, 0x00000000, 0x000009e8, 0x00000008, 0x000009e8,
        0x00000006, 0x00000a88, 0x00000007, 0x00000b00, 0x00000c34, 0x00000c64, 0x00000000, 0x0000004a,
        0x0000004a, 0x0000026a, 0x00000000, 0x00000036, 0x00000001, 0x00000004, 0x00000000, 0xffffffff,
        0x00000000, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00000000, 0x00000003, 0x00000000,
        0x00000003, 0x80000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0xffffffff, 0xffffffff, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000007,
        0x00000000, 0x00000003, 0x00000024, 0x00000000, 0x00000000, 0x00000001, 0x00000036, 0x00000001,
        0x00000001, 0x00000000, 0xffffffff, 0x00000000, 0xffffffff, 0xffffffff, 0xffffffff, 0x00000000,
        0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x00000000, 0x00000000,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000007, 0x00000000, 0x00000003, 0x00000024, 0x00000000, 0x00000000,
        0x00000002, 0x0000003e, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000007, 0x00000000, 0x00000003,
        0x00000024, 0x00000000, 0x00000000, 0x00000003, 0x00000036, 0x00000001, 0x00000004, 0x00000000,
        0xffffffff, 0x00000000, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00000000, 0x00000001,
        0x00000000, 0x00000001, 0x80000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0xffffffff, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000007, 0x00000000, 0x00000003, 0x00000024, 0x00000000, 0x00000000, 0x00000004, 0x00000036,
        0x00000001, 0x00000001, 0x00000004, 0xffffffff, 0x00000000, 0xffffffff, 0xffffffff, 0xffffffff,
        0x00000004, 0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x3f800000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000007, 0x00000000, 0x00000003, 0x00000024, 0x00000000,
        0x00000000, 0x00000005, 0x0000003e, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000007, 0x00000000,
        0x00000003, 0x00000024, 0x00000000, 0x00000000, 0x00000006, 0x00000003, 0xffffffff, 0xffffffff,
        0x00000003, 0x00000000, 0x00000006, 0x00000003, 0xffffffff, 0xffffffff, 0x00000003, 0x00000001,
        0x00000006, 0x00000003, 0xffffffff, 0xffffffff, 0x00000003, 0x00000002, 0x00000006, 0x00000003,
        0xffffffff, 0xffffffff, 0x00000003, 0x00000003, 0x00000006, 0x00000003, 0xffffffff, 0xffffffff,
        0x00000003, 0x00000004, 0x00000006, 0x00000003, 0xffffffff, 0xffffffff, 0x00000003, 0x00000005,
        0x00000000, 0x00000002, 0x00000014, 0x00000007, 0x000002c1, 0x00000000, 0x00000002, 0x00000030,
        0x00000007, 0x000002c8, 0x00000000, 0x00000004, 0x0000001e, 0x00000002, 0x00000102, 0x00000000,
        0x00000004, 0x00000027, 0x00000007, 0x0000010b, 0x00000000, 0x00000006, 0x00000009, 0x00000003,
        0x00000131, 0x00000000, 0x00000001, 0x00000014, 0x00000006, 0x000002cf, 0x00000000, 0x00000004,
        0x00000005, 0x00000004, 0x000000e9, 0x00000000, 0x00000009, 0x00000004, 0x00000000, 0x00000000,
        0x00000003, 0x00000051, 0x00000003, 0x00000001, 0x00000000, 0x00000003, 0x00000076, 0x00000004,
        0x00000002, 0x00000004, 0x00000000, 0x000002b4, 0x00000007, 0x00000001, 0x0000000c, 0x00000003,
        0x00000076, 0x00000004, 0x00000002, 0x00000004, 0x00000001, 0x000002bb, 0x00000006, 0x00000001,
        0x00000010, 0x00000004, 0x000000e9, 0x00000004, 0x00000003, 0x00000014, 0x00000005, 0x00000000,
        0x00000001, 0x00000001, 0x00000003, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000003,
        0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000001, 0xffffffff, 0x00000001,
        0x00000014, 0x00000004, 0x00000004, 0xffffffff, 0x00000001, 0x00000000, 0x00000000, 0x00000001,
        0x00000001, 0xffffffff, 0x00000001, 0x00000008, 0x00000004, 0x00000002, 0xffffffff, 0x00000006,
        0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000,
        0x00000006, 0x00000000, 0x00000002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000001, 0x00000020, 0x0000000c, 0x00000018, 0xffffffff, 0x00000003, 0x00000000, 0x00000000,
        0x00000001, 0x00000001, 0xffffffff, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0xffffffff,
        0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000000,
        0x00000000, 0x00000006, 0xffffffff, 0x00000000, 0x00000001, 0x00000002, 0x00000003, 0x00000006,
        0x00000004, 0x00000005, 0x00000006, 0x00000008, 0x00000004, 0x00000005, 0x00000002, 0x505c3a43,
        0x72676f72, 0x656d6d61, 0x63694d5c, 0x6f736f72, 0x44207466, 0x63657269, 0x53205874, 0x28204b44,
        0x656e754a, 0x31303220, 0x555c2930, 0x696c6974, 0x73656974, 0x6e69625c, 0x3638785c, 0x6165685c,
        0x2e726564, 0x74737866, 0x74637572, 0x4f535620, 0x66207b20, 0x74616f6c, 0x20702033, 0x4f50203a,
        0x49544953, 0x203b4e4f, 0x730a3b7d, 0x63757274, 0x53482074, 0x7b204f44, 0x6f6c6620, 0x65207461,
        0x205d345b, 0x5653203a, 0x7365545f, 0x63614673, 0x3b726f74, 0x6f6c6620, 0x69207461, 0x205d325b,
        0x5653203a, 0x736e495f, 0x54656469, 0x46737365, 0x6f746361, 0x7d203b72, 0x74730a3b, 0x74637572,
        0x4f534820, 0x66207b20, 0x74616f6c, 0x20702033, 0x4542203a, 0x5245495a, 0x3b534f50, 0x0a3b7d20,
        0x4f445348, 0x48434220, 0x49202853, 0x7475706e, 0x63746150, 0x53563c68, 0x38202c4f, 0x7069203e,
        0x6975202c, 0x5020746e, 0x68637461, 0x3a204449, 0x5f565320, 0x6d697250, 0x76697469, 0x20444965,
        0x0a7b0a29, 0x20202020, 0x4f445348, 0x73657220, 0x20200a3b, 0x65722020, 0x5b652e73, 0x3d205d30,
        0x73657220, 0x315b652e, 0x203d205d, 0x2e736572, 0x5d325b65, 0x72203d20, 0x652e7365, 0x205d335b,
        0x2e31203d, 0x0a3b6630, 0x20202020, 0x2e736572, 0x5d305b69, 0x72203d20, 0x692e7365, 0x205d315b,
        0x2e31203d, 0x0a3b6630, 0x20202020, 0x75746572, 0x72206e72, 0x0a3b7365, 0x645b0a7d, 0x69616d6f,
        0x7122286e, 0x22646175, 0x5b0a5d29, 0x74726170, 0x6f697469, 0x676e696e, 0x6e692228, 0x65676574,
        0x5d292272, 0x756f5b0a, 0x74757074, 0x6f706f74, 0x79676f6c, 0x72742228, 0x676e6169, 0x635f656c,
        0x5d292277, 0x756f5b0a, 0x74757074, 0x746e6f63, 0x706c6f72, 0x746e696f, 0x29382873, 0x705b0a5d,
        0x68637461, 0x736e6f63, 0x746e6174, 0x636e7566, 0x43422228, 0x29225348, 0x53480a5d, 0x4842204f,
        0x49202853, 0x7475706e, 0x63746150, 0x53563c68, 0x38202c4f, 0x2c70203e, 0x6e697520, 0x20692074,
        0x5653203a, 0x74754f5f, 0x43747570, 0x72746e6f, 0x6f506c6f, 0x49746e69, 0x75202c44, 0x20746e69,
        0x63746150, 0x20444968, 0x5653203a, 0x6972505f, 0x6974696d, 0x44496576, 0x7b0a2920, 0x2020200a,
        0x4f534820, 0x73657220, 0x20200a3b, 0x65722020, 0x20702e73, 0x5b70203d, 0x702e5d69, 0x20200a3b,
        0x65722020, 0x6e727574, 0x73657220, 0x0a7d0a3b, 0x626f6c47, 0x4c736c61, 0x6c61636f, 0x44534873,
        0x653a3a4f, 0x4f445348, 0x56693a3a, 0x3a3a4f53, 0x63694d70, 0x6f736f72, 0x28207466, 0x48202952,
        0x204c534c, 0x64616853, 0x43207265, 0x69706d6f, 0x2072656c, 0x39322e39, 0x3235392e, 0x3131332e,
        0x48420031, 0x73680053, 0x305f355f, 0x6e6f6320, 0x6c6f7274, 0x696f7020, 0x0000746e,
    };

    static const D3D_BLOB_PART parts[] =
    {
       D3D_BLOB_INPUT_SIGNATURE_BLOB,
       D3D_BLOB_OUTPUT_SIGNATURE_BLOB,
       D3D_BLOB_INPUT_AND_OUTPUT_SIGNATURE_BLOB,
       D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB,
       D3D_BLOB_ALL_SIGNATURE_BLOB,
       D3D_BLOB_DEBUG_INFO,
       D3D_BLOB_LEGACY_SHADER,
       D3D_BLOB_XNA_PREPASS_SHADER,
       D3D_BLOB_XNA_SHADER,
       D3D_BLOB_TEST_ALTERNATE_SHADER,
       D3D_BLOB_TEST_COMPILE_DETAILS,
       D3D_BLOB_TEST_COMPILE_PERF,
    };

    unsigned int refcount, size, i;
    ID3DBlob *blob, *blob2;
    uint32_t *u32;
    HRESULT hr;

    hr = D3DCreateBlob(1, &blob2);
    ok(hr == S_OK, "Got hr %#x.\n", hr);
    blob = blob2;

    /* Invalid cases. */
    hr = D3DGetBlobPart(NULL, test_blob_part[6], D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    hr = D3DGetBlobPart(NULL, 0, D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    hr = D3DGetBlobPart(NULL, test_blob_part[6], D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DGetBlobPart(NULL, 0, D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DGetBlobPart(test_blob_part, 0, D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    hr = D3DGetBlobPart(test_blob_part, 7 * sizeof(DWORD), D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    hr = D3DGetBlobPart(test_blob_part, 8 * sizeof(DWORD), D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DGetBlobPart(test_blob_part, 0, D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, NULL);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_INPUT_SIGNATURE_BLOB, 1, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob2 == blob, "D3DGetBlobPart failed got %p, expected %p\n", blob, blob2);

    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], 0xffffffff, 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
    ok(blob == blob2, "Got blob %p, expected %p.\n", blob, blob2);

    refcount = ID3D10Blob_Release(blob2);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_INPUT_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_INPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 124, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[9] == TAG_ISGN, "Got u32[9] 0x%08x, expected 0x%08x.\n", u32[9], TAG_ISGN);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);

        if (parts[i] == D3D_BLOB_INPUT_SIGNATURE_BLOB)
        {
            ok(hr == S_OK, "Got hr %#x.\n", hr);

            refcount = ID3D10Blob_Release(blob2);
            ok(!refcount, "Got refcount %u.\n", refcount);
        }
        else
        {
            ok(hr == E_FAIL, "Got hr %#x.\n", hr);
        }
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_OUTPUT_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_OUTPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 88, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[9] == TAG_OSGN, "Got u32[9] 0x%08x, expected 0x%08x.\n", u32[9], TAG_OSGN);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);

        if (parts[i] == D3D_BLOB_OUTPUT_SIGNATURE_BLOB)
        {
            ok(hr == S_OK, "Got hr %#x.\n", hr);

            refcount = ID3D10Blob_Release(blob2);
            ok(!refcount, "Got refcount %u.\n", refcount);
        }
        else
        {
            ok(hr == E_FAIL, "Got hr %#x.\n", hr);
        }
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_INPUT_AND_OUTPUT_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_INPUT_AND_OUTPUT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 180, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[10] == TAG_ISGN, "Got u32[10] 0x%08x, expected 0x%08x.\n", u32[10], TAG_ISGN);
    ok(u32[32] == TAG_OSGN, "Got u32[32] 0x%08x, expected 0x%08x.\n", u32[10], TAG_OSGN);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);

        if (parts[i] == D3D_BLOB_INPUT_AND_OUTPUT_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_INPUT_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_OUTPUT_SIGNATURE_BLOB)
        {
            ok(hr == S_OK, "Got hr %#x.\n", hr);

            refcount = ID3D10Blob_Release(blob2);
            ok(!refcount, "Got refcount %u.\n", refcount);
        }
        else
        {
            ok(hr == E_FAIL, "Got hr %#x.\n", hr);
        }
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3D_BLOB_ALL_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_ALL_SIGNATURE_BLOB, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3D_BLOB_DEBUG_INFO */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_DEBUG_INFO, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3D_BLOB_LEGACY_SHADER */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_LEGACY_SHADER, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 92, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] != TAG_DXBC, "Got u32[0] 0x%08x.\n", u32[0]);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        /* D3D_BLOB_LEGACY_SHADER doesn't return a full DXBC blob. */
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);
        ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_XNA_PREPASS_SHADER */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_XNA_PREPASS_SHADER, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 68, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] != TAG_DXBC, "Got u32[0] 0x%08x.\n", u32[0]);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        /* D3D_BLOB_XNA_PREPASS_SHADER doesn't return a full DXBC blob. */
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);
        ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_XNA_SHADER */
    hr = D3DGetBlobPart(test_blob_part, test_blob_part[6], D3D_BLOB_XNA_SHADER, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 68, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] != TAG_DXBC, "Got u32[0] 0x%08x.\n", u32[0]);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        /* D3D_BLOB_XNA_SHADER doesn't return a full DXBC blob. */
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);
        ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3DStripShader() corner cases. */
    hr = D3DStripShader(test_blob_part, test_blob_part[6], 0xffffffff, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    hr = D3DStripShader(test_blob_part, test_blob_part[6], 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    hr = D3DStripShader(NULL, test_blob_part[6], 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DStripShader(test_blob_part, 7 * sizeof(DWORD), 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DStripShader(test_blob_part, 8 * sizeof(DWORD), 0, &blob);
    ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);

    hr = D3DStripShader(test_blob_part, test_blob_part[6], 0, NULL);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    hr = D3DStripShader(NULL, test_blob_part[6], 0, NULL);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    hr = D3DStripShader(test_blob_part, 0, 0, NULL);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3DCOMPILER_STRIP_DEBUG_INFO */
    hr = D3DStripShader(test_blob_part, test_blob_part[6], D3DCOMPILER_STRIP_DEBUG_INFO, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 736, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[16] == TAG_XNAS, "Got u32[16] 0x%08x, expected 0x%08x.\n", u32[16], TAG_XNAS);
    ok(u32[35] == TAG_XNAP, "Got u32[35] 0x%08x, expected 0x%08x.\n", u32[35], TAG_XNAP);
    ok(u32[54] == TAG_AON9, "Got u32[54] 0x%08x, expected 0x%08x.\n", u32[54], TAG_AON9);
    ok(u32[79] == TAG_SHDR, "Got u32[79] 0x%08x, expected 0x%08x.\n", u32[79], TAG_SHDR);
    ok(u32[96] == TAG_STAT, "Got u32[96] 0x%08x, expected 0x%08x.\n", u32[96], TAG_STAT);
    ok(u32[127] == TAG_RDEF, "Got u32[127] 0x%08x, expected 0x%08x.\n", u32[127], TAG_RDEF);
    ok(u32[149] == TAG_ISGN, "Got u32[149] 0x%08x, expected 0x%08x.\n", u32[149], TAG_ISGN);
    ok(u32[171] == TAG_OSGN, "Got u32[171] 0x%08x, expected 0x%08x.\n", u32[171], TAG_OSGN);

    hr = D3DGetBlobPart(u32, size, D3D_BLOB_DEBUG_INFO, 0, &blob2);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3DCOMPILER_STRIP_REFLECTION_DATA */
    hr = D3DStripShader(test_blob_part, test_blob_part[6], D3DCOMPILER_STRIP_REFLECTION_DATA, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 516, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[14] == TAG_XNAS, "Got u32[14] 0x%08x, expected 0x%08x.\n", u32[14], TAG_XNAS);
    ok(u32[33] == TAG_XNAP, "Got u32[33] 0x%08x, expected 0x%08x.\n", u32[33], TAG_XNAP);
    ok(u32[52] == TAG_AON9, "Got u32[52] 0x%08x, expected 0x%08x.\n", u32[52], TAG_AON9);
    ok(u32[77] == TAG_SHDR, "Got u32[77] 0x%08x, expected 0x%08x.\n", u32[77], TAG_SHDR);
    ok(u32[94] == TAG_ISGN, "Got u32[94] 0x%08x, expected 0x%08x.\n", u32[94], TAG_ISGN);
    ok(u32[116] == TAG_OSGN, "Got u32[116] 0x%08x, expected 0x%08x.\n", u32[116], TAG_OSGN);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 232, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[9] == TAG_PCSG, "Got u32[9] 0x%08x, expected 0x%08x.\n", u32[9], TAG_PCSG);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);

        if (parts[i] == D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB)
        {
            ok(hr == S_OK, "Got hr %#x.\n", hr);

            refcount = ID3D10Blob_Release(blob2);
            ok(!refcount, "Got refcount %u.\n", refcount);
        }
        else
        {
            ok(hr == E_FAIL, "Got hr %#x.\n", hr);
        }
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_ALL_SIGNATURE_BLOB */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_ALL_SIGNATURE_BLOB, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 344, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[11] == TAG_ISGN, "Got u32[11] 0x%08x, expected 0x%08x.\n", u32[11], TAG_ISGN);
    ok(u32[24] == TAG_OSGN, "Got u32[24] 0x%08x, expected 0x%08x.\n", u32[24], TAG_OSGN);
    ok(u32[37] == TAG_PCSG, "Got u32[37] 0x%08x, expected 0x%08x.\n", u32[37], TAG_PCSG);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);

        if (parts[i] == D3D_BLOB_ALL_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_PATCH_CONSTANT_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_INPUT_AND_OUTPUT_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_INPUT_SIGNATURE_BLOB
                || parts[i] == D3D_BLOB_OUTPUT_SIGNATURE_BLOB)
        {
            ok(hr == S_OK, "Got hr %#x.\n", hr);

            refcount = ID3D10Blob_Release(blob2);
            ok(!refcount, "Got refcount %u.\n", refcount);
        }
        else
        {
            ok(hr == E_FAIL, "Got hr %#x.\n", hr);
        }
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_DEBUG_INFO */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_DEBUG_INFO, 0, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 4055, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] != TAG_DXBC, "Got u32[0] 0x%08x.\n", u32[0]);

    for (i = 0; i < ARRAY_SIZE(parts); ++i)
    {
        vkd3d_test_push_context("Part %#x", parts[i]);
        /* D3D_BLOB_DEBUG_INFO doesn't return a full DXBC blob. */
        hr = D3DGetBlobPart(u32, size, parts[i], 0, &blob2);
        ok(hr == D3DERR_INVALIDCALL, "Got hr %#x.\n", hr);
        vkd3d_test_pop_context();
    }

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3D_BLOB_LEGACY_SHADER */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_LEGACY_SHADER, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3D_BLOB_XNA_PREPASS_SHADER */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_XNA_PREPASS_SHADER, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3D_BLOB_XNA_SHADER */
    hr = D3DGetBlobPart(test_blob_part2, test_blob_part2[6], D3D_BLOB_XNA_SHADER, 0, &blob);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    /* D3DCOMPILER_STRIP_DEBUG_INFO */
    hr = D3DStripShader(test_blob_part2, test_blob_part2[6], D3DCOMPILER_STRIP_DEBUG_INFO, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 952, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[14] == TAG_RDEF, "Got u32[14] 0x%08x, expected 0x%08x.\n", u32[14], TAG_RDEF);
    ok(u32[44] == TAG_ISGN, "Got u32[44] 0x%08x, expected 0x%08x.\n", u32[44], TAG_ISGN);
    ok(u32[57] == TAG_OSGN, "Got u32[57] 0x%08x, expected 0x%08x.\n", u32[57], TAG_OSGN);
    ok(u32[70] == TAG_PCSG, "Got u32[70] 0x%08x, expected 0x%08x.\n", u32[70], TAG_PCSG);
    ok(u32[119] == TAG_SHEX, "Got u32[119] 0x%08x, expected 0x%08x.\n", u32[119], TAG_SHEX);
    ok(u32[199] == TAG_STAT, "Got u32[199] 0x%08x, expected 0x%08x.\n", u32[199], TAG_STAT);

    hr = D3DGetBlobPart(u32, size, D3D_BLOB_DEBUG_INFO, 0, &blob2);
    ok(hr == E_FAIL, "Got hr %#x.\n", hr);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);

    /* D3DCOMPILER_STRIP_REFLECTION_DATA */
    hr = D3DStripShader(test_blob_part2, test_blob_part2[6], D3DCOMPILER_STRIP_REFLECTION_DATA, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);

    size = ID3D10Blob_GetBufferSize(blob);
    ok(size == 4735, "Got size %u.\n", size);

    u32 = ID3D10Blob_GetBufferPointer(blob);
    ok(u32[0] == TAG_DXBC, "Got u32[0] 0x%08x, expected 0x%08x.\n", u32[0], TAG_DXBC);
    ok(u32[13] == TAG_ISGN, "Got u32[13] 0x%08x, expected 0x%08x.\n", u32[13], TAG_ISGN);
    ok(u32[26] == TAG_OSGN, "Got u32[26] 0x%08x, expected 0x%08x.\n", u32[26], TAG_OSGN);
    ok(u32[39] == TAG_PCSG, "Got u32[39] 0x%08x, expected 0x%08x.\n", u32[39], TAG_PCSG);
    ok(u32[88] == TAG_SHEX, "Got u32[88] 0x%08x, expected 0x%08x.\n", u32[88], TAG_SHEX);
    ok(u32[168] == TAG_SDBG, "Got u32[168] 0x%08x, expected 0x%08x.\n", u32[168], TAG_SDBG);

    refcount = ID3D10Blob_Release(blob);
    ok(!refcount, "Got refcount %u.\n", refcount);
}

static void check_type_desc_(int line, const D3D12_SHADER_TYPE_DESC *type, const D3D12_SHADER_TYPE_DESC *expect)
{
    ok_(line)(type->Class == expect->Class, "Got class %#x.\n", type->Class);
    ok_(line)(type->Type == expect->Type, "Got type %#x.\n", type->Type);
    ok_(line)(type->Rows == expect->Rows, "Got %u rows.\n", type->Rows);
    ok_(line)(type->Columns == expect->Columns, "Got %u columns.\n", type->Columns);
    ok_(line)(type->Elements == expect->Elements, "Got %u elements.\n", type->Elements);
    ok_(line)(type->Members == expect->Members, "Got %u members.\n", type->Members);
    ok_(line)(type->Offset == expect->Offset, "Got %u members.\n", type->Members);
    ok_(line)(!strcmp(type->Name, expect->Name), "Got name \"%s\".\n", type->Name);
}
#define check_type_desc(a, b) check_type_desc_(__LINE__, a, b)

static void test_reflection(void)
{
    unsigned int refcount;
    HRESULT hr;

    static const char vs_source[] =
        "typedef uint uint_t;\n"
        "float m;\n"
        "\n"
        "cbuffer b1\n"
        "{\n"
        "    float a;\n"
        "    float2 b;\n"
        "    float4 c;\n"
        "    float d;\n"
        "    struct\n"
        "    {\n"
        "        float4 a;\n"
        "        float b;\n"
        "        float c;\n"
        "    } s;\n"
        /* In direct contradiction to the documentation, this does not align. */
        "    bool g;\n"
        "    float h[2];\n"
        "    int i;\n"
        "    uint_t j;\n"
        "    float3x1 k;\n"
        "    row_major float3x1 l;\n"
        "#pragma pack_matrix(row_major)\n"
        "    float3x1 o;\n"
        "    float4 p;\n"
        "    float q;\n"
        "    struct r_name {float a;} r;\n"
        "    column_major float3x1 t;\n"
        "};\n"
        "\n"
        "cbuffer b5 : register(b5)\n"
        "{\n"
        "    float4 u;\n"
        "}\n"
        "\n"
        "float4 main(uniform float4 n) : SV_POSITION\n"
        "{\n"
        "    return o._31 + m + n + u;\n"
        "}";

    struct shader_variable
    {
        D3D12_SHADER_VARIABLE_DESC var_desc;
        D3D12_SHADER_TYPE_DESC type_desc;
        const D3D12_SHADER_TYPE_DESC *field_types;
    };

    struct shader_buffer
    {
        D3D12_SHADER_BUFFER_DESC desc;
        const struct shader_variable *vars;
    };

    static const D3D12_SHADER_TYPE_DESC s_field_types[] =
    {
        {D3D_SVC_VECTOR, D3D_SVT_FLOAT, 1, 4, 0, 0, 0, "float4"},
        {D3D_SVC_SCALAR, D3D_SVT_FLOAT, 1, 1, 0, 0, 16, "float"},
        {D3D_SVC_SCALAR, D3D_SVT_FLOAT, 1, 1, 0, 0, 20, "float"},
    };

    static const D3D12_SHADER_TYPE_DESC r_field_types[] =
    {
        {D3D_SVC_SCALAR, D3D_SVT_FLOAT, 1, 1, 0, 0, 0, "float"},
    };

    static const struct shader_variable globals_vars =
        {{"m",   0,  4, D3D_SVF_USED, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_FLOAT, 1, 1, 0, 0, 0, "float"}};
    static const struct shader_variable params_vars =
        {{"n",   0, 16, D3D_SVF_USED, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_VECTOR,         D3D_SVT_FLOAT, 1, 4, 0, 0, 0, "float4"}};
    static const struct shader_variable buffer_vars[] =
    {
        {{"a",   0,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_FLOAT, 1, 1, 0, 0, 0, "float"}},
        {{"b",   4,  8,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_VECTOR,         D3D_SVT_FLOAT, 1, 2, 0, 0, 0, "float2"}},
        {{"c",  16, 16,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_VECTOR,         D3D_SVT_FLOAT, 1, 4, 0, 0, 0, "float4"}},
        {{"d",  32,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_FLOAT, 1, 1, 0, 0, 0, "float"}},
        {{"s",  48, 24,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_STRUCT,         D3D_SVT_VOID,  1, 6, 0, ARRAY_SIZE(s_field_types), 0, "<unnamed>"}, s_field_types},
        {{"g",  72,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_BOOL,  1, 1, 0, 0, 0, "bool"}},
        {{"h",  80, 20,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_FLOAT, 1, 1, 2, 0, 0, "float"}},
        {{"i", 100,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_INT,   1, 1, 0, 0, 0, "int"}},
        {{"j", 104,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_UINT,  1, 1, 0, 0, 0, "uint_t"}},
        {{"k", 112, 12,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_MATRIX_COLUMNS, D3D_SVT_FLOAT, 3, 1, 0, 0, 0, "float3x1"}},
        {{"l", 128, 36,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_MATRIX_ROWS,    D3D_SVT_FLOAT, 3, 1, 0, 0, 0, "float3x1"}},
        {{"o", 176, 36, D3D_SVF_USED, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_MATRIX_ROWS,    D3D_SVT_FLOAT, 3, 1, 0, 0, 0, "float3x1"}},
        {{"p", 224, 16,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_VECTOR,         D3D_SVT_FLOAT, 1, 4, 0, 0, 0, "float4"}},
        {{"q", 240,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_SCALAR,         D3D_SVT_FLOAT, 1, 1, 0, 0, 0, "float"}},
        {{"r", 256,  4,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_STRUCT,         D3D_SVT_VOID,  1, 1, 0, ARRAY_SIZE(r_field_types), 0, "r_name"}, r_field_types},
        {{"t", 260, 12,            0, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_MATRIX_COLUMNS, D3D_SVT_FLOAT, 3, 1, 0, 0, 0, "float3x1"}},
    };
    static const struct shader_variable b5_vars =
        {{"u",   0, 16, D3D_SVF_USED, NULL, ~0u, 0, ~0u, 0}, {D3D_SVC_VECTOR,         D3D_SVT_FLOAT, 1, 4, 0, 0, 0, "float4"}};

    static const struct shader_buffer vs_buffers[] =
    {
        {{"$Globals", D3D_CT_CBUFFER, 1, 16}, &globals_vars},
        {{"$Params", D3D_CT_CBUFFER, 1, 16}, &params_vars},
        {{"b1", D3D_CT_CBUFFER, ARRAY_SIZE(buffer_vars), 272}, buffer_vars},
        {{"b5", D3D_CT_CBUFFER, 1, 16}, &b5_vars},
    };

    static const D3D12_SHADER_INPUT_BIND_DESC vs_bindings[] =
    {
        {"$Globals", D3D_SIT_CBUFFER, 0, 1},
        {"$Params", D3D_SIT_CBUFFER, 1, 1},
        {"b1", D3D_SIT_CBUFFER, 2, 1},
        {"b5", D3D_SIT_CBUFFER, 5, 1, D3D_SIF_USERPACKED},
    };

    static const char ps_source[] =
        "texture2D a;\n"
        "sampler c {};\n"
        "SamplerState d {};\n"
        "sampler e\n"
        "{\n"
        "    Texture = a;\n"
        "    foo = bar + 2;\n"
        "};\n"
        "SamplerState f\n"
        "{\n"
        "    Texture = a;\n"
        "    foo = bar + 2;\n"
        "};\n"
        "sampler2D g;\n"
        "sampler b : register(s5);\n"
        "float4 main(float2 pos : texcoord) : SV_TARGET\n"
        "{\n"
        "    return a.Sample(b, pos) + a.Sample(c, pos) + a.Sample(d, pos) + tex2D(f, pos) + tex2D(e, pos)"
        "            + tex2D(g, pos);\n"
        "}";

    static const D3D12_SHADER_INPUT_BIND_DESC ps_bindings[] =
    {
        {"c", D3D_SIT_SAMPLER, 0, 1},
        {"d", D3D_SIT_SAMPLER, 1, 1},
        {"e", D3D_SIT_SAMPLER, 2, 1},
        {"f", D3D_SIT_SAMPLER, 3, 1},
        {"g", D3D_SIT_SAMPLER, 4, 1},
        {"b", D3D_SIT_SAMPLER, 5, 1, D3D_SIF_USERPACKED},
        {"f", D3D_SIT_TEXTURE, 0, 1, D3D_SIF_TEXTURE_COMPONENTS, D3D_RETURN_TYPE_FLOAT, D3D_SRV_DIMENSION_TEXTURE2D, ~0u},
        {"e", D3D_SIT_TEXTURE, 1, 1, D3D_SIF_TEXTURE_COMPONENTS, D3D_RETURN_TYPE_FLOAT, D3D_SRV_DIMENSION_TEXTURE2D, ~0u},
        {"g", D3D_SIT_TEXTURE, 2, 1, D3D_SIF_TEXTURE_COMPONENTS, D3D_RETURN_TYPE_FLOAT, D3D_SRV_DIMENSION_TEXTURE2D, ~0u},
        {"a", D3D_SIT_TEXTURE, 3, 1, D3D_SIF_TEXTURE_COMPONENTS, D3D_RETURN_TYPE_FLOAT, D3D_SRV_DIMENSION_TEXTURE2D, ~0u},
    };

    static const struct
    {
        const char *source;
        const char *profile;
        const D3D12_SHADER_INPUT_BIND_DESC *bindings;
        size_t binding_count;
        const struct shader_buffer *buffers;
        size_t buffer_count;
    }
    tests[] =
    {
        {vs_source, "vs_5_0", vs_bindings, ARRAY_SIZE(vs_bindings), vs_buffers, ARRAY_SIZE(vs_buffers)},
        {ps_source, "ps_5_0", ps_bindings, ARRAY_SIZE(ps_bindings)},
    };

    for (unsigned int t = 0; t < ARRAY_SIZE(tests); ++t)
    {
        ID3D10Blob *code = compile_shader_flags(tests[t].source,
                tests[t].profile, D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY);
        ID3D12ShaderReflection *reflection;
        D3D12_SHADER_DESC shader_desc;

        hr = D3DReflect(ID3D10Blob_GetBufferPointer(code), ID3D10Blob_GetBufferSize(code),
                &IID_ID3D12ShaderReflection, (void **)&reflection);
        ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);

        hr = ID3D12ShaderReflection_GetDesc(reflection, &shader_desc);
        ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
        ok(shader_desc.ConstantBuffers == tests[t].buffer_count, "Got %u buffers.\n", shader_desc.ConstantBuffers);
        todo ok(shader_desc.BoundResources == tests[t].binding_count, "Got %u resources.\n", shader_desc.BoundResources);

        for (unsigned int i = 0; i < shader_desc.ConstantBuffers; ++i)
        {
            const struct shader_buffer *expect_buffer = &tests[t].buffers[i];
            ID3D12ShaderReflectionConstantBuffer *cbuffer;
            D3D12_SHADER_BUFFER_DESC buffer_desc;

            vkd3d_test_push_context("Buffer %u", i);

            cbuffer = ID3D12ShaderReflection_GetConstantBufferByIndex(reflection, i);
            hr = ID3D12ShaderReflectionConstantBuffer_GetDesc(cbuffer, &buffer_desc);
            ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
            ok(!strcmp(buffer_desc.Name, expect_buffer->desc.Name), "Got name \"%s\".\n", buffer_desc.Name);
            ok(buffer_desc.Type == expect_buffer->desc.Type, "Got type %#x.\n", buffer_desc.Type);
            ok(buffer_desc.Variables == expect_buffer->desc.Variables, "Got %u variables.\n", buffer_desc.Variables);
            ok(buffer_desc.Size == expect_buffer->desc.Size, "Got size %u.\n", buffer_desc.Size);
            ok(buffer_desc.uFlags == expect_buffer->desc.uFlags, "Got flags %#x.\n", buffer_desc.uFlags);

            for (unsigned int j = 0; j < buffer_desc.Variables; ++j)
            {
                const struct shader_variable *expect = &expect_buffer->vars[j];
                ID3D12ShaderReflectionType *type, *field;
                ID3D12ShaderReflectionVariable *var;
                D3D12_SHADER_VARIABLE_DESC var_desc;
                D3D12_SHADER_TYPE_DESC type_desc;

                vkd3d_test_push_context("Variable %u", j);

                var = ID3D12ShaderReflectionConstantBuffer_GetVariableByIndex(cbuffer, j);
                hr = ID3D12ShaderReflectionVariable_GetDesc(var, &var_desc);
                ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
                ok(!strcmp(var_desc.Name, expect->var_desc.Name), "Got name \"%s\".\n", var_desc.Name);
                ok(var_desc.StartOffset == expect->var_desc.StartOffset, "Got offset %u.\n", var_desc.StartOffset);
                ok(var_desc.Size == expect->var_desc.Size, "Got size %u.\n", var_desc.Size);
                ok(var_desc.uFlags == expect->var_desc.uFlags, "Got flags %#x.\n", var_desc.uFlags);
                ok(!var_desc.DefaultValue, "Got default value %p.\n", var_desc.DefaultValue);
                todo ok(var_desc.StartTexture == expect->var_desc.StartTexture,
                        "Got texture offset %u.\n", var_desc.StartTexture);
                ok(var_desc.TextureSize == expect->var_desc.TextureSize,
                        "Got texture size %u.\n", var_desc.TextureSize);
                todo ok(var_desc.StartSampler == expect->var_desc.StartSampler,
                        "Got sampler offset %u.\n", var_desc.StartSampler);
                ok(var_desc.SamplerSize == expect->var_desc.SamplerSize,
                        "Got sampler size %u.\n", var_desc.SamplerSize);

                type = ID3D12ShaderReflectionVariable_GetType(var);
                hr = ID3D12ShaderReflectionType_GetDesc(type, &type_desc);
                ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
                check_type_desc(&type_desc, &expect->type_desc);

                for (unsigned int k = 0; k < type_desc.Members; ++k)
                {
                    vkd3d_test_push_context("Field %u", k);

                    field = ID3D12ShaderReflectionType_GetMemberTypeByIndex(type, k);
                    hr = ID3D12ShaderReflectionType_GetDesc(field, &type_desc);
                    ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
                    check_type_desc(&type_desc, &expect->field_types[k]);

                    vkd3d_test_pop_context();
                }

                vkd3d_test_pop_context();
            }

            vkd3d_test_pop_context();
        }

        for (unsigned int i = 0; i < shader_desc.BoundResources; ++i)
        {
            const D3D12_SHADER_INPUT_BIND_DESC *expect = &tests[t].bindings[i];
            D3D12_SHADER_INPUT_BIND_DESC desc;

            vkd3d_test_push_context("Binding %u", i);

            hr = ID3D12ShaderReflection_GetResourceBindingDesc(reflection, i, &desc);
            ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);

            ok(!strcmp(desc.Name, expect->Name), "Got name \"%s\".\n", desc.Name);
            ok(desc.Type == expect->Type, "Got type %#x.\n", desc.Type);
            ok(desc.BindPoint == expect->BindPoint, "Got bind point %u.\n", desc.BindPoint);
            ok(desc.BindCount == expect->BindCount, "Got bind count %u.\n", desc.BindCount);
            todo_if ((expect->uFlags & D3D_SIF_USERPACKED) && expect->Type != D3D_SIT_CBUFFER)
                ok(desc.uFlags == expect->uFlags, "Got flags %#x.\n", desc.uFlags);
            ok(desc.ReturnType == expect->ReturnType, "Got return type %#x.\n", desc.ReturnType);
            ok(desc.Dimension == expect->Dimension, "Got dimension %#x.\n", desc.Dimension);
            ok(desc.NumSamples == expect->NumSamples, "Got multisample count %u.\n", desc.NumSamples);

            vkd3d_test_pop_context();
        }

        ID3D10Blob_Release(code);
        refcount = ID3D12ShaderReflection_Release(reflection);
        ok(!refcount, "Got unexpected refcount %u.\n", refcount);
    }
}

static void check_signature_element_(int line, const D3D12_SIGNATURE_PARAMETER_DESC *desc,
        const D3D12_SIGNATURE_PARAMETER_DESC *expect)
{
    ok_(line)(!strcmp(desc->SemanticName, expect->SemanticName), "Got name \"%s\".\n", desc->SemanticName);
    ok_(line)(desc->SemanticIndex == expect->SemanticIndex, "Got index %u.\n", desc->SemanticIndex);
    ok_(line)(desc->Register == expect->Register, "Got register %u.\n", desc->Register);
    ok_(line)(desc->SystemValueType == expect->SystemValueType, "Got sysval %u.\n", desc->SystemValueType);
    ok_(line)(desc->ComponentType == expect->ComponentType, "Got data type %u.\n", desc->ComponentType);
    ok_(line)(desc->Mask == expect->Mask, "Got mask %#x.\n", desc->Mask);
    todo_if(desc->ReadWriteMask != expect->ReadWriteMask)
        ok_(line)(desc->ReadWriteMask == expect->ReadWriteMask, "Got used mask %#x.\n", desc->ReadWriteMask);
    ok_(line)(desc->Stream == expect->Stream, "Got stream %u.\n", desc->Stream);
}
#define check_signature_element(a, b) check_signature_element_(__LINE__, a, b)

static void test_signature_reflection(void)
{
    D3D12_SIGNATURE_PARAMETER_DESC desc;
    ID3D12ShaderReflection *reflection;
    D3D12_SHADER_DESC shader_desc;
    ID3D10Blob *code = NULL;
    unsigned int refcount;
    HRESULT hr;

    static const char vs1_source[] =
        "void main(\n"
        "        in float4 a : apple,\n"
        "        out float4 b : banana2,\n"
        "        inout float4 c : color,\n"
        "        inout float4 d : depth,\n"
        "        inout float4 e : sv_position,\n"
        "        in uint3 f : fruit,\n"
        "        inout bool2 g : grape,\n"
        "        in int h : honeydew,\n"
        "        in uint i : sv_vertexid)\n"
        "{\n"
        "    b.yw = a.xz;\n"
        "}";

    static const D3D12_SIGNATURE_PARAMETER_DESC vs1_inputs[] =
    {
        {"apple",       0, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0x5},
        {"color",       0, 1, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
        {"depth",       0, 2, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
        {"sv_position", 0, 3, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
        {"fruit",       0, 4, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_UINT32, 0x7},
        {"grape",       0, 5, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_UINT32, 0x3, 0x3},
        {"honeydew",    0, 6, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_SINT32, 0x1},
        {"sv_vertexid", 0, 7, D3D_NAME_VERTEX_ID, D3D_REGISTER_COMPONENT_UINT32, 0x1},
    };

    static const D3D12_SIGNATURE_PARAMETER_DESC vs1_outputs[] =
    {
        {"banana",      2, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0x5},
        {"color",       0, 1, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
        {"depth",       0, 2, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
        {"sv_position", 0, 3, D3D_NAME_POSITION,  D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
        {"grape",       0, 4, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_UINT32, 0x3, 0xc},
    };

    static const char vs2_source[] =
        "void main(inout float4 pos : position)\n"
        "{\n"
        "}";

    static const D3D12_SIGNATURE_PARAMETER_DESC vs2_inputs[] =
    {
        {"position",    0, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
    };

    static const D3D12_SIGNATURE_PARAMETER_DESC vs2_outputs[] =
    {
        {"position",    0, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
    };

    static const D3D12_SIGNATURE_PARAMETER_DESC vs2_legacy_outputs[] =
    {
        {"SV_Position", 0, 0, D3D_NAME_POSITION, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
    };

    static const char ps1_source[] =
        "void main(\n"
        "        in float2 a : apple,\n"
        "        out float4 b : sv_target2,\n"
        "        out float c : sv_depth,\n"
        "        in float4 d : position,\n"
        "        in float4 e : sv_position)\n"
        "{\n"
        "    b = d;\n"
        "    c = 0;\n"
        "}";

    static const D3D12_SIGNATURE_PARAMETER_DESC ps1_inputs[] =
    {
        {"apple",       0, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0x3},
        {"position",    0, 1, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
        {"sv_position", 0, 2, D3D_NAME_POSITION,  D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
    };

    static const D3D12_SIGNATURE_PARAMETER_DESC ps1_outputs[] =
    {
        {"sv_target",   2, 2,   D3D_NAME_TARGET, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
        {"sv_depth",    0, ~0u, D3D_NAME_DEPTH,  D3D_REGISTER_COMPONENT_FLOAT32, 0x1, 0xe},
    };

    static const char ps2_source[] =
        "void main(\n"
        "        inout float4 a : color2,\n"
        "        inout float b : depth,\n"
        "        in float4 c : position)\n"
        "{\n"
        "}";

    static const D3D12_SIGNATURE_PARAMETER_DESC ps2_inputs[] =
    {
        {"color",       2, 0, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0xf, 0xf},
        {"depth",       0, 1, D3D_NAME_UNDEFINED, D3D_REGISTER_COMPONENT_FLOAT32, 0x1, 0x1},
        {"SV_Position", 0, 2, D3D_NAME_POSITION,  D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
    };

    static const D3D12_SIGNATURE_PARAMETER_DESC ps2_outputs[] =
    {
        {"SV_Target",   2, 2,   D3D_NAME_TARGET, D3D_REGISTER_COMPONENT_FLOAT32, 0xf},
        {"SV_Depth",    0, ~0u, D3D_NAME_DEPTH,  D3D_REGISTER_COMPONENT_FLOAT32, 0x1, 0xe},
    };

    static const char cs1_source[] =
        "[numthreads(1, 1, 1)]\n"
        "void main(in uint a : sv_dispatchthreadid)\n"
        "{\n"
        "}";

    static const struct
    {
        const char *source;
        const char *target;
        bool compat;
        const D3D12_SIGNATURE_PARAMETER_DESC *inputs;
        unsigned int input_count;
        const D3D12_SIGNATURE_PARAMETER_DESC *outputs;
        unsigned int output_count;
    }
    tests[] =
    {
        {vs1_source,  "vs_4_0", false, vs1_inputs, ARRAY_SIZE(vs1_inputs), vs1_outputs, ARRAY_SIZE(vs1_outputs)},
        {vs1_source,  "vs_4_0", true,  vs1_inputs, ARRAY_SIZE(vs1_inputs), vs1_outputs, ARRAY_SIZE(vs1_outputs)},
        {vs2_source,  "vs_4_0", false, vs2_inputs, ARRAY_SIZE(vs2_inputs), vs2_outputs, ARRAY_SIZE(vs2_outputs)},
        {vs2_source,  "vs_4_0", true,  vs2_inputs, ARRAY_SIZE(vs2_inputs), vs2_legacy_outputs, ARRAY_SIZE(vs2_legacy_outputs)},
        {ps1_source,  "ps_4_0", false, ps1_inputs, ARRAY_SIZE(ps1_inputs), ps1_outputs, ARRAY_SIZE(ps1_outputs)},
        {ps2_source,  "ps_4_0", true,  ps2_inputs, ARRAY_SIZE(ps2_inputs), ps2_outputs, ARRAY_SIZE(ps2_outputs)},
        {cs1_source,  "cs_5_0", false, NULL, 0, NULL, 0},
    };

    for (unsigned int i = 0; i < ARRAY_SIZE(tests); ++i)
    {
        vkd3d_test_push_context("Test %u", i);

        code = compile_shader_flags(tests[i].source, tests[i].target,
                tests[i].compat ? D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY : 0);

        hr = D3DReflect(ID3D10Blob_GetBufferPointer(code), ID3D10Blob_GetBufferSize(code),
                &IID_ID3D12ShaderReflection, (void **)&reflection);
        ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);

        hr = reflection->lpVtbl->GetDesc(reflection, &shader_desc);
        ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
        ok(shader_desc.InputParameters == tests[i].input_count,
                "Got %u input parameters.\n", shader_desc.InputParameters);
        ok(shader_desc.OutputParameters == tests[i].output_count,
                "Got %u output parameters.\n", shader_desc.OutputParameters);

        for (unsigned int j = 0; j < shader_desc.InputParameters; ++j)
        {
            vkd3d_test_push_context("Input %u", j);
            hr = reflection->lpVtbl->GetInputParameterDesc(reflection, j, &desc);
            ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
            check_signature_element(&desc, &tests[i].inputs[j]);
            vkd3d_test_pop_context();
        }

        for (unsigned int j = 0; j < shader_desc.OutputParameters; ++j)
        {
            vkd3d_test_push_context("Output %u", j);
            hr = reflection->lpVtbl->GetOutputParameterDesc(reflection, j, &desc);
            ok(hr == S_OK, "Got unexpected hr %#x.\n", hr);
            check_signature_element(&desc, &tests[i].outputs[j]);
            vkd3d_test_pop_context();
        }

        ID3D10Blob_Release(code);
        refcount = reflection->lpVtbl->Release(reflection);
        ok(!refcount, "Got unexpected refcount %u.\n", refcount);

        vkd3d_test_pop_context();
    }
}

static void test_disassemble_shader(void)
{
    ID3DBlob *blob;
    int hr;

    /* A Direct3D 8 vertex shader without dcl_ instructions. */
    static const uint32_t vs_1_1[] =
    {
        0xfffe0101,                                                 /* vs_1_1                 */
        0x00000005, 0x800f0000, 0x90000000, 0xa0e40000,             /* mul r0, v0.x, c0       */
        0x00000004, 0x800f0000, 0x90550000, 0xa0e40001, 0x80e40000, /* mad r0, v0.y, c1, r0   */
        0x00000004, 0x800f0000, 0x90aa0000, 0xa0e40002, 0x80e40000, /* mad r0, v0.z, c2, r0   */
        0x00000004, 0xc00f0000, 0x90ff0000, 0xa0e40003, 0x80e40000, /* mad oPos, v0.w, c3, r0 */
        0x0000ffff,                                                 /* end                    */
    };

    static const uint32_t vs_2_0[] =
    {
        0xfffe0200,                                                 /* vs_2_0                 */
        0x0200001f, 0x80000000, 0x900f0000,                         /* dcl_position v0        */
        0x0200001f, 0x80000003, 0x900f0001,                         /* dcl_normal v1          */
        0x0200001f, 0x8001000a, 0x900f0002,                         /* dcl_color1 v2          */
        0x0200001f, 0x80000005, 0x900f0003,                         /* dcl_texcoord0 v3       */
        0x02000001, 0xc00f0000, 0x90e40000,                         /* mov oPos, v0           */
        0x02000001, 0xd00f0001, 0x90e40002,                         /* mov oD1, v2            */
        0x02000001, 0xe0070000, 0x90e40003,                         /* mov oT0.xyz, v3        */
        0x02000001, 0xc00f0001, 0x90ff0002,                         /* mov oFog, v2.w         */
        0x02000001, 0xc00f0002, 0x90ff0001,                         /* mov oPts, v1.w         */
        0x0000ffff,                                                 /* end                    */
    };

    /* A shader model 3 vertex shader without dcl_ instructions. */
    static const uint32_t vs_3_0[] =
    {
        0xfffe0300,                                                 /* vs_3_0                  */
        0x02000001, 0xe00f0000, 0x90e40000,                         /* mov o0, v0              */
        0x0000ffff,                                                 /* end                     */
    };

    /* A "shader model 4" d3dbc vertex shader. */
    static const uint32_t vs_4_0[] =
    {
        0xfffe0400,                                                 /* vs_4_0                  */
        0x02000001, 0xe00f0000, 0x90e40000,                         /* mov o0, v0              */
        0x0000ffff,                                                 /* end                     */
    };

    /* An actual shader model 4 dxbc-tpf vertex shader. */
    static const uint32_t vs_4_0_dxbc[] =
    {
#if 0
        float4 main(float4 position : POSITION) : SV_POSITION
        {
            return position;
        }
#endif
        0x43425844, 0xa7a2f22d, 0x83ff2560, 0xe61638bd, 0x87e3ce90, 0x00000001, 0x000000d8, 0x00000003,
        0x0000002c, 0x00000060, 0x00000094, 0x4e475349, 0x0000002c, 0x00000001, 0x00000008, 0x00000020,
        0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x49534f50, 0x4e4f4954, 0xababab00,
        0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000001, 0x00000003,
        0x00000000, 0x0000000f, 0x505f5653, 0x5449534f, 0x004e4f49, 0x52444853, 0x0000003c, 0x00010040,
        0x0000000f, 0x0300005f, 0x001010f2, 0x00000000, 0x04000067, 0x001020f2, 0x00000000, 0x00000001,
        0x05000036, 0x001020f2, 0x00000000, 0x00101e46, 0x00000000, 0x0100003e,
    };

    hr = D3DDisassemble(vs_1_1, 0, 0, NULL, &blob);
    ok(hr == E_INVALIDARG, "Got hr %#x.\n", hr);

    hr = D3DDisassemble(vs_1_1, sizeof(vs_1_1), 0, NULL, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);
    ID3D10Blob_Release(blob);

    hr = D3DDisassemble(vs_2_0, sizeof(vs_2_0), 0, NULL, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);
    ID3D10Blob_Release(blob);

    hr = D3DDisassemble(vs_3_0, sizeof(vs_3_0), 0, NULL, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);
    ID3D10Blob_Release(blob);

    hr = D3DDisassemble(vs_4_0, sizeof(vs_4_0), 0, NULL, &blob);
    todo ok(hr == S_OK, "Got hr %#x.\n", hr);
    if (SUCCEEDED(hr))
        ID3D10Blob_Release(blob);

    hr = D3DDisassemble(vs_4_0_dxbc, sizeof(vs_4_0_dxbc), 0, NULL, &blob);
    ok(hr == S_OK, "Got hr %#x.\n", hr);
    ID3D10Blob_Release(blob);
}

START_TEST(hlsl_d3d12)
{
    parse_args(argc, argv);
    enable_d3d12_debug_layer();
    init_adapter_info();

    run_test(test_preprocess);
    run_test(test_thread_id);
    run_test(test_create_blob);
    run_test(test_get_blob_part);
    run_test(test_reflection);
    run_test(test_signature_reflection);
    run_test(test_disassemble_shader);
}
