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
        vkd3d_test_set_context("Source \"%s\"", tests[i].source);
        check_preprocess(tests[i].source, NULL, NULL, tests[i].present, tests[i].absent);
    }
    vkd3d_test_set_context(NULL);

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
    struct resource_readback rb;
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
        textures[i] = create_default_texture3d(device, 16, 8, 8, 1, DXGI_FORMAT_R32G32B32A32_UINT,
                D3D12_RESOURCE_FLAG_ALLOW_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_UNORDERED_ACCESS);
        ID3D12Device_CreateUnorderedAccessView(device, textures[i], NULL, NULL,
                get_cpu_descriptor_handle(&context, heap, i));
    }

    cs_code = compile_shader(cs_source, "cs_5_0");
    context.pipeline_state = create_compute_pipeline_state(device, context.root_signature,
            shader_bytecode(ID3D10Blob_GetBufferPointer(cs_code), ID3D10Blob_GetBufferSize(cs_code)));

    ID3D12GraphicsCommandList_SetPipelineState(command_list, context.pipeline_state);
    ID3D12GraphicsCommandList_SetComputeRootSignature(command_list, context.root_signature);
    ID3D12GraphicsCommandList_SetComputeRootDescriptorTable(command_list,
            0, get_gpu_descriptor_handle(&context, heap, 0));
    ID3D12GraphicsCommandList_Dispatch(command_list, 2, 2, 2);

    transition_resource_state(command_list, textures[0],
            D3D12_RESOURCE_STATE_UNORDERED_ACCESS, D3D12_RESOURCE_STATE_COPY_SOURCE);
    get_texture_readback_with_command_list(textures[0], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb, x, y, z, sizeof(struct uvec4));
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
    get_texture_readback_with_command_list(textures[1], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb, x, y, z, sizeof(struct uvec4));
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
    get_texture_readback_with_command_list(textures[2], 0, &rb, context.queue, command_list);
    for (x = 0; x < 16; ++x)
    {
        for (y = 0; y < 8; ++y)
        {
            for (z = 0; z < 8; ++z)
            {
                const struct uvec4 *v = get_readback_data(&rb, x, y, z, sizeof(struct uvec4));
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

START_TEST(hlsl_d3d12)
{
    parse_args(argc, argv);
    enable_d3d12_debug_layer(argc, argv);
    init_adapter_info();

    run_test(test_preprocess);
    run_test(test_thread_id);
}
