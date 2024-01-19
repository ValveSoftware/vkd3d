/*
 * Copyright 2018 Józef Kucia for CodeWeavers
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

#include "vkd3d_test.h"
#include <vkd3d_shader.h>

#include <locale.h>

static void test_invalid_shaders(void)
{
    struct vkd3d_shader_compile_info info;
    struct vkd3d_shader_code spirv;
    int rc;

    static const DWORD ps_break_code[] =
    {
#if 0
        ps_4_0
        dcl_constantbuffer cb0[1], immediateIndexed
        dcl_output o0.xyzw
        if_z cb0[0].x
            mov o0.xyzw, l(1.000000,1.000000,1.000000,1.000000)
            break
        endif
        mov o0.xyzw, l(0,0,0,0)
        ret
#endif
        0x43425844, 0x1316702a, 0xb1a7ebfc, 0xf477753e, 0x72605647, 0x00000001, 0x000000f8, 0x00000003,
        0x0000002c, 0x0000003c, 0x00000070, 0x4e475349, 0x00000008, 0x00000000, 0x00000008, 0x4e47534f,
        0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000000, 0x00000001, 0x00000000,
        0x0000000f, 0x545f5653, 0x65677261, 0xabab0074, 0x52444853, 0x00000080, 0x00000040, 0x00000020,
        0x04000059, 0x00208e46, 0x00000000, 0x00000001, 0x03000065, 0x001020f2, 0x00000000, 0x0400001f,
        0x0020800a, 0x00000000, 0x00000000, 0x08000036, 0x001020f2, 0x00000000, 0x00004002, 0x3f800000,
        0x3f800000, 0x3f800000, 0x3f800000, 0x01000002, 0x01000015, 0x08000036, 0x001020f2, 0x00000000,
        0x00004002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0100003e,
    };
    static const struct vkd3d_shader_compile_option option =
    {
        .name = VKD3D_SHADER_COMPILE_OPTION_STRIP_DEBUG,
        .value = 1,
    };

    info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    info.next = NULL;
    info.source.code = ps_break_code;
    info.source.size = sizeof(ps_break_code);
    info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    info.target_type = VKD3D_SHADER_TARGET_SPIRV_BINARY;
    info.options = &option;
    info.option_count = 1;
    info.log_level = VKD3D_SHADER_LOG_NONE;
    info.source_name = NULL;

    rc = vkd3d_shader_compile(&info, &spirv, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);

    rc = vkd3d_shader_scan(&info, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);
}

static void test_vkd3d_shader_pfns(void)
{
    PFN_vkd3d_shader_get_supported_source_types pfn_vkd3d_shader_get_supported_source_types;
    PFN_vkd3d_shader_get_supported_target_types pfn_vkd3d_shader_get_supported_target_types;
    PFN_vkd3d_shader_free_scan_descriptor_info pfn_vkd3d_shader_free_scan_descriptor_info;
    PFN_vkd3d_shader_serialize_root_signature pfn_vkd3d_shader_serialize_root_signature;
    PFN_vkd3d_shader_find_signature_element pfn_vkd3d_shader_find_signature_element;
    PFN_vkd3d_shader_free_shader_signature pfn_vkd3d_shader_free_shader_signature;
    PFN_vkd3d_shader_parse_input_signature pfn_vkd3d_shader_parse_input_signature;
    PFN_vkd3d_shader_parse_root_signature pfn_vkd3d_shader_parse_root_signature;
    PFN_vkd3d_shader_free_root_signature pfn_vkd3d_shader_free_root_signature;
    PFN_vkd3d_shader_free_shader_code pfn_vkd3d_shader_free_shader_code;
    PFN_vkd3d_shader_get_version pfn_vkd3d_shader_get_version;
    PFN_vkd3d_shader_compile pfn_vkd3d_shader_compile;
    PFN_vkd3d_shader_scan pfn_vkd3d_shader_scan;

    struct vkd3d_shader_versioned_root_signature_desc root_signature_desc;
    unsigned int major, minor, expected_major, expected_minor;
    struct vkd3d_shader_scan_descriptor_info descriptor_info;
    const enum vkd3d_shader_source_type *source_types;
    const enum vkd3d_shader_target_type *target_types;
    struct vkd3d_shader_signature_element *element;
    struct vkd3d_shader_compile_info compile_info;
    unsigned int i, j, source_count, target_count;
    struct vkd3d_shader_signature signature;
    struct vkd3d_shader_code dxbc, spirv;
    const char *version, *p;
    bool b;
    int rc;

    static const struct vkd3d_shader_versioned_root_signature_desc empty_rs_desc =
    {
        .version = VKD3D_SHADER_ROOT_SIGNATURE_VERSION_1_0,
    };
    static const DWORD vs_code[] =
    {
#if 0
        float4 main(int4 p : POSITION) : SV_Position
        {
            return p;
        }
#endif
        0x43425844, 0x3fd50ab1, 0x580a1d14, 0x28f5f602, 0xd1083e3a, 0x00000001, 0x000000d8, 0x00000003,
        0x0000002c, 0x00000060, 0x00000094, 0x4e475349, 0x0000002c, 0x00000001, 0x00000008, 0x00000020,
        0x00000000, 0x00000000, 0x00000002, 0x00000000, 0x00000f0f, 0x49534f50, 0x4e4f4954, 0xababab00,
        0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000001, 0x00000003,
        0x00000000, 0x0000000f, 0x505f5653, 0x7469736f, 0x006e6f69, 0x52444853, 0x0000003c, 0x00010040,
        0x0000000f, 0x0300005f, 0x001010f2, 0x00000000, 0x04000067, 0x001020f2, 0x00000000, 0x00000001,
        0x0500002b, 0x001020f2, 0x00000000, 0x00101e46, 0x00000000, 0x0100003e,
    };
    static const struct vkd3d_shader_code vs = {vs_code, sizeof(vs_code)};

    pfn_vkd3d_shader_get_supported_source_types = vkd3d_shader_get_supported_source_types;
    pfn_vkd3d_shader_get_supported_target_types = vkd3d_shader_get_supported_target_types;
    pfn_vkd3d_shader_free_scan_descriptor_info = vkd3d_shader_free_scan_descriptor_info;
    pfn_vkd3d_shader_serialize_root_signature = vkd3d_shader_serialize_root_signature;
    pfn_vkd3d_shader_find_signature_element = vkd3d_shader_find_signature_element;
    pfn_vkd3d_shader_free_shader_signature = vkd3d_shader_free_shader_signature;
    pfn_vkd3d_shader_parse_input_signature = vkd3d_shader_parse_input_signature;
    pfn_vkd3d_shader_parse_root_signature = vkd3d_shader_parse_root_signature;
    pfn_vkd3d_shader_free_root_signature = vkd3d_shader_free_root_signature;
    pfn_vkd3d_shader_free_shader_code = vkd3d_shader_free_shader_code;
    pfn_vkd3d_shader_get_version = vkd3d_shader_get_version;
    pfn_vkd3d_shader_compile = vkd3d_shader_compile;
    pfn_vkd3d_shader_scan = vkd3d_shader_scan;

    sscanf(PACKAGE_VERSION, "%d.%d", &expected_major, &expected_minor);
    version = pfn_vkd3d_shader_get_version(&major, &minor);
    p = strstr(version, "vkd3d-shader " PACKAGE_VERSION);
    ok(p == version, "Got unexpected version string \"%s\"\n", version);
    ok(major == expected_major, "Got unexpected major version %u.\n", major);
    ok(minor == expected_minor, "Got unexpected minor version %u.\n", minor);

    source_types = pfn_vkd3d_shader_get_supported_source_types(&source_count);
    ok(source_types, "Got unexpected source types array %p.\n", source_types);
    ok(source_count, "Got unexpected source type count %u.\n", source_count);

    b = false;
    for (i = 0; i < source_count; ++i)
    {
        target_types = pfn_vkd3d_shader_get_supported_target_types(source_types[i], &target_count);
        ok(target_types, "Got unexpected target types array %p.\n", target_types);
        ok(target_count, "Got unexpected target type count %u.\n", target_count);

        for (j = 0; j < target_count; ++j)
        {
            if (source_types[i] == VKD3D_SHADER_SOURCE_DXBC_TPF
                    && target_types[j] == VKD3D_SHADER_TARGET_SPIRV_BINARY)
                b = true;
        }
    }
    ok(b, "The dxbc-tpf source type with spirv-binary target type is not supported.\n");

    rc = pfn_vkd3d_shader_serialize_root_signature(&empty_rs_desc, &dxbc, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    rc = pfn_vkd3d_shader_parse_root_signature(&dxbc, &root_signature_desc, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    pfn_vkd3d_shader_free_root_signature(&root_signature_desc);
    pfn_vkd3d_shader_free_shader_code(&dxbc);

    rc = pfn_vkd3d_shader_parse_input_signature(&vs, &signature, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    element = pfn_vkd3d_shader_find_signature_element(&signature, "position", 0, 0);
    ok(element, "Could not find shader signature element.\n");
    pfn_vkd3d_shader_free_shader_signature(&signature);

    compile_info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    compile_info.next = NULL;
    compile_info.source = vs;
    compile_info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    compile_info.target_type = VKD3D_SHADER_TARGET_SPIRV_BINARY;
    compile_info.options = NULL;
    compile_info.option_count = 0;
    compile_info.log_level = VKD3D_SHADER_LOG_NONE;
    compile_info.source_name = NULL;

    rc = pfn_vkd3d_shader_compile(&compile_info, &spirv, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    pfn_vkd3d_shader_free_shader_code(&spirv);

    memset(&descriptor_info, 0, sizeof(descriptor_info));
    descriptor_info.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_DESCRIPTOR_INFO;
    compile_info.next = &descriptor_info;

    rc = pfn_vkd3d_shader_scan(&compile_info, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    pfn_vkd3d_shader_free_scan_descriptor_info(&descriptor_info);
}

static void test_version(void)
{
    unsigned int major, minor, expected_major, expected_minor;
    const char *version, *p;

    sscanf(PACKAGE_VERSION, "%d.%d", &expected_major, &expected_minor);

    version = vkd3d_shader_get_version(NULL, NULL);
    p = strstr(version, "vkd3d-shader " PACKAGE_VERSION);
    ok(p == version, "Got unexpected version string \"%s\"\n", version);

    major = ~0u;
    vkd3d_shader_get_version(&major, NULL);
    ok(major == expected_major, "Got unexpected major version %u.\n", major);

    minor = ~0u;
    vkd3d_shader_get_version(NULL, &minor);
    ok(minor == expected_minor, "Got unexpected minor version %u.\n", minor);

    major = minor = ~0u;
    vkd3d_shader_get_version(&major, &minor);
    ok(major == expected_major, "Got unexpected major version %u.\n", major);
    ok(minor == expected_minor, "Got unexpected minor version %u.\n", minor);
}

static void test_d3dbc(void)
{
    struct vkd3d_shader_compile_info info;
    struct vkd3d_shader_code d3d_asm;
    int rc;

    static const uint32_t vs_minimal[] =
    {
        0xfffe0100, /* vs_1_0 */
        0x0000ffff, /* end */
    };
    static const uint32_t vs_dcl_def[] =
    {
        0xfffe0101,                                                             /* vs_1_1 */
        0x0000001f, 0x80000000, 0x900f0000,                                     /* dcl_position v0  */
        0x00000051, 0xa00f0000, 0x3f800000, 0x00000000, 0x00000000, 0x3f800000, /* def c0, 1.0, 0.0, 0.0, 1.0   */
        0x0000ffff,                                                             /* end */
    };
    static const uint32_t invalid_type[] =
    {
        0x00010100, /* <invalid>_1_0 */
        0x0000ffff, /* end */
    };
    static const uint32_t invalid_version[] =
    {
        0xfffe0400, /* vs_4_0 */
        0x0000ffff, /* end */
    };
    static const uint32_t ps[] =
    {
        0xffff0101,                         /* ps_1_1      */
        0x00000001, 0x800f0000, 0x90e40000, /* mov r0, v0 */
        0x0000ffff,                         /* end */
    };
    static const char expected[] = "vs_1_0\n";
    static const char expected_dcl_def[] =
        "vs_1_1\n"
        "dcl_position0 v0\n"
        "def c0 = 1.00000000e+00, 0.00000000e+00, 0.00000000e+00, 1.00000000e+00\n";

    info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    info.next = NULL;
    info.source.code = vs_minimal;
    info.source.size = sizeof(vs_minimal);
    info.source_type = VKD3D_SHADER_SOURCE_D3D_BYTECODE;
    info.target_type = VKD3D_SHADER_TARGET_D3D_ASM;
    info.options = NULL;
    info.option_count = 0;
    info.log_level = VKD3D_SHADER_LOG_NONE;
    info.source_name = NULL;

    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    ok(d3d_asm.size == strlen(expected), "Got unexpected size %zu.\n", d3d_asm.size);
    ok(!memcmp(d3d_asm.code, expected, d3d_asm.size), "Got unexpected code \"%.*s\"\n",
            (int)d3d_asm.size, (char *)d3d_asm.code);
    vkd3d_shader_free_shader_code(&d3d_asm);

    rc = vkd3d_shader_scan(&info, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);

    info.source.size = sizeof(vs_minimal) + 1;
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    vkd3d_shader_free_shader_code(&d3d_asm);

    info.source.size = ~(size_t)0;
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    vkd3d_shader_free_shader_code(&d3d_asm);

    info.source.size = sizeof(vs_minimal) - 1;
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);

    info.source.code = vs_dcl_def;
    info.source.size = sizeof(vs_dcl_def);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    ok(d3d_asm.size == strlen(expected_dcl_def), "Got unexpected size %zu.\n", d3d_asm.size);
    ok(!memcmp(d3d_asm.code, expected_dcl_def, d3d_asm.size), "Got unexpected code \"%.*s\"\n",
            (int)d3d_asm.size, (char *)d3d_asm.code);
    vkd3d_shader_free_shader_code(&d3d_asm);

    info.source.code = invalid_type;
    info.source.size = sizeof(invalid_type);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);

    info.source.code = invalid_version;
    info.source.size = sizeof(invalid_version);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);

    info.source.code = ps;
    info.source.size = sizeof(ps);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);
    vkd3d_shader_free_shader_code(&d3d_asm);

    /* Truncated before the destination parameter. */
    info.source.size = sizeof(ps) - 3 * sizeof(*ps);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);

    /* Truncated before the source parameter. */
    info.source.size = sizeof(ps) - 2 * sizeof(*ps);
    rc = vkd3d_shader_compile(&info, &d3d_asm, NULL);
    ok(rc == VKD3D_ERROR_INVALID_SHADER, "Got unexpected error code %d.\n", rc);
}

static void test_dxbc(void)
{
    PFN_vkd3d_shader_serialize_dxbc pfn_vkd3d_shader_serialize_dxbc;
    PFN_vkd3d_shader_parse_dxbc pfn_vkd3d_shader_parse_dxbc;
    PFN_vkd3d_shader_free_dxbc pfn_vkd3d_shader_free_dxbc;
    struct vkd3d_shader_dxbc_desc dxbc_desc;
    const uint8_t *dxbc_start, *dxbc_end;
    struct vkd3d_shader_code dxbc;
    size_t expected_size;
    unsigned int i;
    int ret;

    static const uint32_t section_0[] =
    {
        0x00000000, 0x00000001, 0x00000001, 0x00000002,
        0x00000003, 0x00000005, 0x00000008, 0x0000000d,
        0x00000015, 0x00000022, 0x00000037, 0x00000059,
        0x00000090, 0x000000e9, 0x00000179, 0x00000262,
    };
    static const uint8_t section_1[] =
    {
        0x1, 0x4, 0x1, 0x5, 0x9, 0x2, 0x6, 0x5, 0x3, 0x5, 0x8, 0x9, 0x7, 0x9, 0x3, 0x2,
        0x3, 0x8, 0x4, 0x6, 0x2, 0x6, 0x4, 0x3, 0x3, 0x8, 0x3, 0x2, 0x7, 0x9, 0x5, 0x0,
        0x2, 0x8, 0x8, 0x4, 0x1, 0x9, 0x7, 0x1, 0x6, 0x9, 0x3, 0x9, 0x9, 0x3, 0x7, 0x5,
        0x1, 0x0, 0x5, 0x8, 0x2, 0x0, 0x9, 0x7, 0x4, 0x9, 0x4, 0x4, 0x5, 0x9, 0x2, 0x3,
    };
    static const struct vkd3d_shader_dxbc_section_desc sections[] =
    {
        {.tag = 0x00424946, .data = {.code = section_0, .size = sizeof(section_0)}},
        {.tag = 0x49504950, .data = {.code = section_1, .size = sizeof(section_1)}},
    };
    static const uint32_t checksum[] = {0x7cfc687d, 0x7e8f4cff, 0x72a4739a, 0xd75c3703};

    pfn_vkd3d_shader_serialize_dxbc = vkd3d_shader_serialize_dxbc;
    pfn_vkd3d_shader_parse_dxbc = vkd3d_shader_parse_dxbc;
    pfn_vkd3d_shader_free_dxbc = vkd3d_shader_free_dxbc;

    /* 8 * u32 for the DXBC header */
    expected_size = 8 * sizeof(uint32_t);
    for (i = 0; i < ARRAY_SIZE(sections); ++i)
    {
        /* 1 u32 for the section offset + 2 u32 for the section header. */
        expected_size += 3 * sizeof(uint32_t) + sections[i].data.size;
    }

    ret = pfn_vkd3d_shader_serialize_dxbc(ARRAY_SIZE(sections), sections, &dxbc, NULL);
    ok(ret == VKD3D_OK, "Got unexpected ret %d.\n", ret);
    ok(dxbc.size == expected_size, "Got unexpected size %zu, expected %zu.\n", dxbc_desc.size, expected_size);

    ret = pfn_vkd3d_shader_parse_dxbc(&dxbc, 0, &dxbc_desc, NULL);
    ok(ret == VKD3D_OK, "Got unexpected ret %d.\n", ret);
    ok(dxbc_desc.tag == 0x43425844, "Got unexpected tag 0x%08x.\n", dxbc_desc.tag);
    ok(!memcmp(dxbc_desc.checksum, checksum, sizeof(dxbc_desc.checksum)),
            "Got unexpected checksum 0x%08x 0x%08x 0x%08x 0x%08x.\n",
            dxbc_desc.checksum[0], dxbc_desc.checksum[1], dxbc_desc.checksum[2], dxbc_desc.checksum[3]);
    ok(dxbc_desc.version == 1, "Got unexpected version %#x.\n", dxbc_desc.version);
    ok(dxbc_desc.size == expected_size, "Got unexpected size %zu, expected %zu.\n", dxbc_desc.size, expected_size);
    ok(dxbc_desc.section_count == ARRAY_SIZE(sections), "Got unexpected section count %zu, expected %zu.\n",
            dxbc_desc.section_count, ARRAY_SIZE(sections));

    dxbc_start = dxbc.code;
    dxbc_end = dxbc_start + dxbc.size;
    for (i = 0; i < dxbc_desc.section_count; ++i)
    {
        const struct vkd3d_shader_dxbc_section_desc *section = &dxbc_desc.sections[i];
        const uint8_t *data = section->data.code;

        vkd3d_test_push_context("Section %u", i);
        ok(section->tag == sections[i].tag, "Got unexpected tag 0x%08x, expected 0x%08x.\n",
                section->tag, sections[i].tag);
        ok(section->data.size == sections[i].data.size, "Got unexpected size %zu, expected %zu.\n",
                section->data.size, sections[i].data.size);
        ok(!memcmp(data, sections[i].data.code, section->data.size), "Got unexpected section data.\n");
        ok(data > dxbc_start && data <= dxbc_end - section->data.size,
                "Data {%p, %zu} is not contained within blob {%p, %zu}.\n",
                data, section->data.size, dxbc_start, dxbc_end - dxbc_start);
        vkd3d_test_pop_context();
    }

    pfn_vkd3d_shader_free_dxbc(&dxbc_desc);
    vkd3d_shader_free_shader_code(&dxbc);
}

static void check_signature_element(const struct vkd3d_shader_signature_element *element,
        const struct vkd3d_shader_signature_element *expect)
{
    ok(!strcmp(element->semantic_name, expect->semantic_name), "Got semantic name %s.\n", element->semantic_name);
    ok(element->semantic_index == expect->semantic_index, "Got semantic index %u.\n", element->semantic_index);
    ok(element->stream_index == expect->stream_index, "Got stream index %u.\n", element->stream_index);
    ok(element->sysval_semantic == expect->sysval_semantic, "Got sysval semantic %#x.\n", element->sysval_semantic);
    ok(element->component_type == expect->component_type, "Got component type %#x.\n", element->component_type);
    ok(element->register_index == expect->register_index, "Got register index %u.\n", element->register_index);
    ok(element->mask == expect->mask, "Got mask %#x.\n", element->mask);
    todo_if (expect->used_mask != expect->mask)
        ok(element->used_mask == expect->used_mask, "Got used mask %#x.\n", element->used_mask);
    ok(element->min_precision == expect->min_precision, "Got minimum precision %#x.\n", element->min_precision);
}

static void test_scan_signatures(void)
{
    struct vkd3d_shader_scan_signature_info signature_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_SIGNATURE_INFO};
    struct vkd3d_shader_hlsl_source_info hlsl_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO};
    struct vkd3d_shader_compile_info compile_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_compile_option options[1];
    struct vkd3d_shader_code dxbc;
    size_t i, j;
    int rc;

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

    static const struct vkd3d_shader_signature_element vs1_inputs[] =
    {
        {"apple",       0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0x5},
        {"color",       0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"depth",       0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"sv_position", 0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"fruit",       0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_UINT,  4, 0x7},
        {"grape",       0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_UINT,  5, 0x3, 0x3},
        {"honeydew",    0, 0, VKD3D_SHADER_SV_NONE,      VKD3D_SHADER_COMPONENT_INT,   6, 0x1},
        {"sv_vertexid", 0, 0, VKD3D_SHADER_SV_VERTEX_ID, VKD3D_SHADER_COMPONENT_UINT,  7, 0x1},
    };

    static const struct vkd3d_shader_signature_element vs1_outputs[] =
    {
        {"banana",      2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xa},
        {"color",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"depth",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"sv_position", 0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"grape",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_UINT,  4, 0x3, 0x3},
    };

    static const char vs2_source[] =
        "void main(inout float4 pos : position)\n"
        "{\n"
        "}";

    static const struct vkd3d_shader_signature_element vs2_inputs[] =
    {
        {"position",    0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element vs2_outputs[] =
    {
        {"position",    0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element vs2_legacy_outputs[] =
    {
        {"SV_Position", 0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
    };

    static const char vs3_source[] =
        "void main(\n"
        "        in float4 c : position,\n"
        "        out float4 b : position,\n"
        "        in float4 a : binormal,\n"
        "        in float4 d : blendindices,\n"
        "        inout float4 e : texcoord2,\n"
        "        inout float4 f : color,\n"
        "        inout float g : fog,\n"
        "        inout float h : psize)\n"
        "{\n"
        "    b = a + c + d;\n"
        "}";

    static const struct vkd3d_shader_signature_element vs3_inputs[] =
    {
        {"POSITION",        0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"BINORMAL",        0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"BLENDINDICES",    0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 4, 0xf, 0xf},
        {"FOG",             0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 5, 0xf, 0xf},
        {"PSIZE",           0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 6, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element vs3_outputs[] =
    {
        {"POSITION",        0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 10, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT,  2, 0xf, 0xf},
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT,  8, 0xf, 0xf},
        {"FOG",             0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 11, 0x1, 0x1},
        {"PSIZE",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 12, 0x1, 0x1},
    };

    static const char vs4_source[] =
        "void main(\n"
        "        inout float4 c : position,\n"
        "        inout float4 a : binormal,\n"
        "        inout float4 d : blendindices,\n"
        "        inout float4 e : texcoord2,\n"
        "        inout float4 f : color,\n"
        "        inout float4 g : fog,\n"
        "        inout float h : psize)\n"
        "{\n"
        "}";

    static const struct vkd3d_shader_signature_element vs4_inputs[] =
    {
        {"POSITION",        0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"BINORMAL",        0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"BLENDINDICES",    0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 4, 0xf, 0xf},
        {"FOG",             0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 5, 0xf, 0xf},
        {"PSIZE",           0, 0, VKD3D_SHADER_SV_NONE, VKD3D_SHADER_COMPONENT_FLOAT, 6, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element vs4_outputs[] =
    {
        {"POSITION",        0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"BINORMAL",        0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"BLENDINDICES",    0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 4, 0xf, 0xf},
        {"FOG",             0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 5, 0xf, 0xf},
        /* FIXME: This doesn't match native, which always declares and writes all 4 components. */
        {"PSIZE",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 6, 0x1, 0x1},
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

    static const struct vkd3d_shader_signature_element ps1_inputs[] =
    {
        {"apple",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0x3},
        {"position",    0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"sv_position", 0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf},
    };

    static const struct vkd3d_shader_signature_element ps1_outputs[] =
    {
        {"sv_target",   2, 0, VKD3D_SHADER_SV_TARGET,   VKD3D_SHADER_COMPONENT_FLOAT, 2,   0xf, 0xf},
        {"sv_depth",    0, 0, VKD3D_SHADER_SV_DEPTH,    VKD3D_SHADER_COMPONENT_FLOAT, ~0u, 0x1, 0x1},
    };

    static const char ps2_source[] =
        "void main(\n"
                "in float4 c : color,\n"
                "in float4 a : texcoord2,\n"
                "out float4 b : color)\n"
        "{\n"
            "b = a.x + c;\n"
        "}";

    static const struct vkd3d_shader_signature_element ps2_inputs[] =
    {
        {"TEXCOORD",    2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"COLOR",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 8, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element ps2_outputs[] =
    {
        {"COLOR",       0, 0, VKD3D_SHADER_SV_TARGET,   VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
    };

    static const char ps3_source[] =
        "void main(\n"
                "in float4 c : color,\n"
                "in float4 a : texcoord2,\n"
                "out float4 b : color,\n"
                "out float d : depth)\n"
        "{\n"
            "b = c;\n"
            "d = a;\n"
        "}";

    static const struct vkd3d_shader_signature_element ps3_inputs[] =
    {
        {"COLOR",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 8, 0xf, 0xf},
        {"TEXCOORD",    2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
    };

    static const struct vkd3d_shader_signature_element ps3_outputs[] =
    {
        {"COLOR",       0, 0, VKD3D_SHADER_SV_TARGET,   VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"DEPTH",       0, 0, VKD3D_SHADER_SV_DEPTH,    VKD3D_SHADER_COMPONENT_FLOAT, 0, 0x1, 0x1},
    };

    static const char ps4_source[] =
        "void main(\n"
        "        in float4 c : color,\n"
        "        in float4 a : texcoord2,\n"
        "        out float4 b : color,\n"
        "        inout float d : depth,\n"
        "        in float4 e : blendindices,\n"
        "        in float4 f : vpos,\n"
        "        in float g : vface)\n"
        "{\n"
        "    b = c + a + e + f + g;\n"
        "}";

    static const struct vkd3d_shader_signature_element ps4_inputs[] =
    {
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE,             VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE,             VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"DEPTH",           0, 0, VKD3D_SHADER_SV_NONE,             VKD3D_SHADER_COMPONENT_FLOAT, 2, 0x1, 0x1},
        {"BLENDINDICES",    0, 0, VKD3D_SHADER_SV_NONE,             VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"VPOS",            0, 0, VKD3D_SHADER_SV_POSITION,         VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"VFACE",           0, 0, VKD3D_SHADER_SV_IS_FRONT_FACE,    VKD3D_SHADER_COMPONENT_FLOAT, 1, 0x1, 0x1},
    };

    static const char ps5_source[] =
        "void main(\n"
        "        inout float4 a : color2,\n"
        "        inout float b : depth,\n"
        "        in float4 c : position)\n"
        "{\n"
        "}";

    static const struct vkd3d_shader_signature_element ps5_inputs[] =
    {
        {"color",       2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"depth",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0x1, 0x1},
        {"SV_Position", 0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf},
    };

    static const struct vkd3d_shader_signature_element ps5_outputs[] =
    {
        {"SV_Target",   2, 0, VKD3D_SHADER_SV_TARGET,   VKD3D_SHADER_COMPONENT_FLOAT, 2,   0xf, 0xf},
        {"SV_Depth",    0, 0, VKD3D_SHADER_SV_DEPTH,    VKD3D_SHADER_COMPONENT_FLOAT, ~0u, 0x1, 0x1},
    };

    static const char cs1_source[] =
        "[numthreads(1, 1, 1)]\n"
        "void main(\n"
        "        in uint a : sv_dispatchthreadid,\n"
        "        in uint b : sv_groupid,\n"
        "        in uint c : sv_groupthreadid)\n"
        "{\n"
        "}";

    static const struct
    {
        const char *source;
        bool sm4;
        const char *profile;
        bool compat;
        const struct vkd3d_shader_signature_element *inputs;
        size_t input_count;
        const struct vkd3d_shader_signature_element *outputs;
        size_t output_count;
        const struct vkd3d_shader_signature_element *patch_constants;
        size_t patch_constant_count;
    }
    tests[] =
    {
        {vs1_source, true,  "vs_4_0", false, vs1_inputs, ARRAY_SIZE(vs1_inputs), vs1_outputs, ARRAY_SIZE(vs1_outputs)},
        {vs1_source, true,  "vs_4_0", true,  vs1_inputs, ARRAY_SIZE(vs1_inputs), vs1_outputs, ARRAY_SIZE(vs1_outputs)},
        {vs2_source, true,  "vs_4_0", false, vs2_inputs, ARRAY_SIZE(vs2_inputs), vs2_outputs, ARRAY_SIZE(vs2_outputs)},
        {vs2_source, true,  "vs_4_0", true,  vs2_inputs, ARRAY_SIZE(vs2_inputs), vs2_legacy_outputs, ARRAY_SIZE(vs2_legacy_outputs)},
        {ps1_source, true,  "ps_4_0", false, ps1_inputs, ARRAY_SIZE(ps1_inputs), ps1_outputs, ARRAY_SIZE(ps1_outputs)},
        {ps5_source, true,  "ps_4_0", true,  ps5_inputs, ARRAY_SIZE(ps5_inputs), ps5_outputs, ARRAY_SIZE(ps5_outputs)},
        {cs1_source, true,  "cs_5_0", false, NULL, 0, NULL, 0},

        {vs3_source, false, "vs_1_1", false, vs3_inputs, ARRAY_SIZE(vs3_inputs), vs3_outputs, ARRAY_SIZE(vs3_outputs)},
        {vs3_source, false, "vs_2_0", false, vs3_inputs, ARRAY_SIZE(vs3_inputs), vs3_outputs, ARRAY_SIZE(vs3_outputs)},
        {vs4_source, false, "vs_3_0", false, vs4_inputs, ARRAY_SIZE(vs4_inputs), vs4_outputs, ARRAY_SIZE(vs4_outputs)},
        {ps2_source, false, "ps_1_1", false, ps2_inputs, ARRAY_SIZE(ps2_inputs), ps2_outputs, ARRAY_SIZE(ps2_outputs)},
        {ps3_source, false, "ps_2_0", false, ps3_inputs, ARRAY_SIZE(ps3_inputs), ps3_outputs, ARRAY_SIZE(ps3_outputs)},
        {ps4_source, false, "ps_3_0", false, ps4_inputs, ARRAY_SIZE(ps4_inputs), ps3_outputs, ARRAY_SIZE(ps3_outputs)},
    };

    for (i = 0; i < ARRAY_SIZE(tests); ++i)
    {
        vkd3d_test_push_context("test %u", i);

        compile_info.source.code = tests[i].source;
        compile_info.source.size = strlen(tests[i].source);
        compile_info.source_type = VKD3D_SHADER_SOURCE_HLSL;
        compile_info.target_type = tests[i].sm4 ? VKD3D_SHADER_TARGET_DXBC_TPF : VKD3D_SHADER_TARGET_D3D_BYTECODE;
        compile_info.log_level = VKD3D_SHADER_LOG_INFO;
        compile_info.options = options;
        compile_info.option_count = 1;

        options[0].name = VKD3D_SHADER_COMPILE_OPTION_BACKWARD_COMPATIBILITY;
        options[0].value = 0;
        if (tests[i].compat)
            options[0].value = VKD3D_SHADER_COMPILE_OPTION_BACKCOMPAT_MAP_SEMANTIC_NAMES;

        compile_info.next = &hlsl_info;
        hlsl_info.profile = tests[i].profile;

        rc = vkd3d_shader_compile(&compile_info, &dxbc, NULL);
        ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);

        compile_info.source_type = tests[i].sm4 ? VKD3D_SHADER_SOURCE_DXBC_TPF : VKD3D_SHADER_SOURCE_D3D_BYTECODE;
        compile_info.source = dxbc;

        compile_info.next = &signature_info;

        rc = vkd3d_shader_scan(&compile_info, NULL);
        ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);

        ok(signature_info.input.element_count == tests[i].input_count,
                "Got input count %u.\n", signature_info.input.element_count);
        for (j = 0; j < signature_info.input.element_count; ++j)
        {
            vkd3d_test_push_context("input %u", j);
            check_signature_element(&signature_info.input.elements[j], &tests[i].inputs[j]);
            vkd3d_test_pop_context();
        }

        ok(signature_info.output.element_count == tests[i].output_count,
                "Got output count %u.\n", signature_info.output.element_count);
        for (j = 0; j < signature_info.output.element_count; ++j)
        {
            vkd3d_test_push_context("output %u", j);
            check_signature_element(&signature_info.output.elements[j], &tests[i].outputs[j]);
            vkd3d_test_pop_context();
        }

        ok(signature_info.patch_constant.element_count == tests[i].patch_constant_count,
                "Got patch constant count %u.\n", signature_info.patch_constant.element_count);
        for (j = 0; j < signature_info.patch_constant.element_count; ++j)
        {
            vkd3d_test_push_context("patch constant %u", j);
            check_signature_element(&signature_info.patch_constant.elements[j], &tests[i].patch_constants[j]);
            vkd3d_test_pop_context();
        }

        vkd3d_shader_free_scan_signature_info(&signature_info);
        vkd3d_shader_free_shader_code(&dxbc);

        vkd3d_test_pop_context();
    }
}

static void test_scan_descriptors(void)
{
    struct vkd3d_shader_scan_descriptor_info descriptor_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_DESCRIPTOR_INFO};
    struct vkd3d_shader_hlsl_source_info hlsl_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO};
    struct vkd3d_shader_compile_info compile_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_code dxbc;
    size_t i, j;
    int rc;

    static const char ps1_source[] =
        "float4 main(uniform float4 u, uniform float4 v) : sv_target\n"
        "{\n"
        "    return u * v + 1.0;\n"
        "}";

    static const char ps2_source[] =
        "float4 main() : sv_target\n"
        "{\n"
        "    return 1.0;\n"
        "}";

    static const struct vkd3d_shader_descriptor_info ps1_descriptors[] =
    {
        {VKD3D_SHADER_DESCRIPTOR_TYPE_CBV, 0, VKD3D_SHADER_D3DBC_FLOAT_CONSTANT_REGISTER,
                VKD3D_SHADER_RESOURCE_BUFFER, VKD3D_SHADER_RESOURCE_DATA_UINT, 0, 1},
    };

    static const struct
    {
        const char *source;
        bool sm4;
        const char *profile;
        const struct vkd3d_shader_descriptor_info *descriptors;
        size_t descriptor_count;
    }
    tests[] =
    {
        {ps1_source, false, "ps_2_0", ps1_descriptors, ARRAY_SIZE(ps1_descriptors)},
        {ps2_source, false, "ps_2_0", NULL, 0},
    };

    for (i = 0; i < ARRAY_SIZE(tests); ++i)
    {
        vkd3d_test_push_context("test %u", i);

        compile_info.source.code = tests[i].source;
        compile_info.source.size = strlen(tests[i].source);
        compile_info.source_type = VKD3D_SHADER_SOURCE_HLSL;
        compile_info.target_type = tests[i].sm4 ? VKD3D_SHADER_TARGET_DXBC_TPF : VKD3D_SHADER_TARGET_D3D_BYTECODE;
        compile_info.log_level = VKD3D_SHADER_LOG_INFO;

        compile_info.next = &hlsl_info;
        hlsl_info.profile = tests[i].profile;

        rc = vkd3d_shader_compile(&compile_info, &dxbc, NULL);
        ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);

        compile_info.source_type = tests[i].sm4 ? VKD3D_SHADER_SOURCE_DXBC_TPF : VKD3D_SHADER_SOURCE_D3D_BYTECODE;
        compile_info.source = dxbc;

        compile_info.next = &descriptor_info;

        rc = vkd3d_shader_scan(&compile_info, NULL);
        ok(rc == VKD3D_OK, "Got unexpected error code %d.\n", rc);

        ok(descriptor_info.descriptor_count == tests[i].descriptor_count,
                "Got descriptor count %u.\n", descriptor_info.descriptor_count);
        for (j = 0; j < descriptor_info.descriptor_count; ++j)
        {
            const struct vkd3d_shader_descriptor_info *descriptor = &descriptor_info.descriptors[j];
            const struct vkd3d_shader_descriptor_info *expect = &tests[i].descriptors[j];

            vkd3d_test_push_context("descriptor %u", j);

            ok(descriptor->type == expect->type, "Got type %#x.\n", descriptor->type);
            ok(descriptor->register_space == expect->register_space, "Got space %u.\n", descriptor->register_space);
            ok(descriptor->register_index == expect->register_index, "Got index %u.\n", descriptor->register_index);
            ok(descriptor->resource_type == expect->resource_type,
                    "Got resource type %#x.\n", descriptor->resource_type);
            ok(descriptor->resource_data_type == expect->resource_data_type,
                    "Got data type %#x.\n", descriptor->resource_data_type);
            ok(descriptor->flags == expect->flags, "Got flags %#x.\n", descriptor->flags);
            ok(descriptor->count == expect->count, "Got count %u.\n", descriptor->count);

            vkd3d_test_pop_context();
        }

        vkd3d_shader_free_scan_descriptor_info(&descriptor_info);
        vkd3d_shader_free_shader_code(&dxbc);

        vkd3d_test_pop_context();
    }
}

static void test_build_varying_map(void)
{
    struct vkd3d_shader_signature_element output_elements[] =
    {
        {"position", 0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"texcoord", 2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0xf, 0xf},
        {"colour",   0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
    };
    struct vkd3d_shader_signature_element input_elements[] =
    {
        {"colour",   0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 3, 0xf, 0xf},
        {"texcoord", 2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 4, 0x3, 0x3},
    };
    struct vkd3d_shader_signature output = {output_elements, ARRAY_SIZE(output_elements)};
    struct vkd3d_shader_signature input = {input_elements, ARRAY_SIZE(input_elements)};
    PFN_vkd3d_shader_build_varying_map pfn_vkd3d_shader_build_varying_map;
    struct vkd3d_shader_varying_map map[ARRAY_SIZE(input_elements)];
    unsigned int count;

    pfn_vkd3d_shader_build_varying_map = vkd3d_shader_build_varying_map;
    pfn_vkd3d_shader_build_varying_map(&output, &input, &count, map);
    ok(count == ARRAY_SIZE(input_elements), "Got count %u.\n", count);
    ok(map[0].output_signature_index == 2, "Got map[0].output_signature_index %u.\n", map[0].output_signature_index);
    ok(map[0].input_register_index == 3, "Got map[0].input_register_index %u.\n", map[0].input_register_index);
    ok(map[0].input_mask == 0xf, "Got map[0].input_mask %#x.\n", map[0].input_mask);
    ok(map[1].output_signature_index == 1, "Got map[1].output_signature_index %u.\n", map[1].output_signature_index);
    ok(map[1].input_register_index == 4, "Got map[1].input_register_index %u.\n", map[1].input_register_index);
    ok(map[1].input_mask == 0x3, "Got map[1].input_mask %#x.\n", map[1].input_mask);
}

static void test_scan_combined_resource_samplers(void)
{
    struct vkd3d_shader_hlsl_source_info hlsl_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO};
    struct vkd3d_shader_compile_info info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    PFN_vkd3d_shader_free_scan_combined_resource_sampler_info pfn_free_combined_sampler_info;
    struct vkd3d_shader_scan_combined_resource_sampler_info combined_sampler_info;
    struct vkd3d_shader_combined_resource_sampler_info *s;
    struct vkd3d_shader_code d3dbc;
    int rc;

    static const char ps_3_0_source[] =
        "sampler s[3];\n"
        "\n"
        "float4 main(float4 coord : TEXCOORD) : COLOR\n"
        "{\n"
        "    float4 r;\n"
        "\n"
        "    r = tex2D(s[0], coord.xy);\n"
        "    r += tex2D(s[2], coord.xy);\n"
        "\n"
        "    return r;\n"
        "}\n";

    static const uint32_t ps_5_1[] =
    {
#if 0
        Texture2D<float4> t0[8] : register(t8, space4);
        Texture2D<float4> t1[4] : register(t9, space5);
        SamplerState s2[4] : register(s10, space6);
        SamplerState s3[8] : register(s11, space7);

        float4 main(float4 coord : TEXCOORD) : SV_TARGET
        {
            float4 r;

            r = t0[7].Sample(s2[3], coord.xy);
            r += t1[2].Sample(s3[6], coord.xy);
            r += t0[4].Load(coord.xyz);

            return r;
        }
#endif
        0x43425844, 0x743f5994, 0x0c6d43cf, 0xde114c10, 0xc1adc69a, 0x00000001, 0x000001f8, 0x00000003,
        0x0000002c, 0x00000060, 0x00000094, 0x4e475349, 0x0000002c, 0x00000001, 0x00000008, 0x00000020,
        0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x0000070f, 0x43584554, 0x44524f4f, 0xababab00,
        0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000000, 0x00000003,
        0x00000000, 0x0000000f, 0x545f5653, 0x45475241, 0xabab0054, 0x58454853, 0x0000015c, 0x00000051,
        0x00000057, 0x0100086a, 0x0600005a, 0x00306e46, 0x00000000, 0x0000000a, 0x0000000d, 0x00000006,
        0x0600005a, 0x00306e46, 0x00000001, 0x0000000b, 0x00000012, 0x00000007, 0x07001858, 0x00307e46,
        0x00000000, 0x00000008, 0x0000000f, 0x00005555, 0x00000004, 0x07001858, 0x00307e46, 0x00000001,
        0x00000009, 0x0000000c, 0x00005555, 0x00000005, 0x03001062, 0x00101072, 0x00000000, 0x03000065,
        0x001020f2, 0x00000000, 0x02000068, 0x00000002, 0x0b000045, 0x001000f2, 0x00000000, 0x00101046,
        0x00000000, 0x00207e46, 0x00000000, 0x0000000f, 0x00206000, 0x00000000, 0x0000000d, 0x0b000045,
        0x001000f2, 0x00000001, 0x00101046, 0x00000000, 0x00207e46, 0x00000001, 0x0000000b, 0x00206000,
        0x00000001, 0x00000011, 0x07000000, 0x001000f2, 0x00000000, 0x00100e46, 0x00000000, 0x00100e46,
        0x00000001, 0x0500001b, 0x001000f2, 0x00000001, 0x00101a46, 0x00000000, 0x0800002d, 0x001000f2,
        0x00000001, 0x00100e46, 0x00000001, 0x00207e46, 0x00000000, 0x0000000c, 0x07000000, 0x001020f2,
        0x00000000, 0x00100e46, 0x00000000, 0x00100e46, 0x00000001, 0x0100003e,
    };

    pfn_free_combined_sampler_info = vkd3d_shader_free_scan_combined_resource_sampler_info;

    hlsl_info.profile = "ps_3_0";

    info.next = &hlsl_info;
    info.source.code = ps_3_0_source;
    info.source.size = ARRAY_SIZE(ps_3_0_source);
    info.source_type = VKD3D_SHADER_SOURCE_HLSL;
    info.target_type = VKD3D_SHADER_TARGET_D3D_BYTECODE;
    info.log_level = VKD3D_SHADER_LOG_INFO;

    rc = vkd3d_shader_compile(&info, &d3dbc, NULL);
    ok(rc == VKD3D_OK, "Got rc %d.\n", rc);

    combined_sampler_info.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_COMBINED_RESOURCE_SAMPLER_INFO;
    combined_sampler_info.next = NULL;

    info.next = &combined_sampler_info;
    info.source = d3dbc;
    info.source_type = VKD3D_SHADER_SOURCE_D3D_BYTECODE;
    info.target_type = VKD3D_SHADER_TARGET_NONE;

    rc = vkd3d_shader_scan(&info, NULL);
    ok(rc == VKD3D_OK, "Got rc %d.\n", rc);
    ok(combined_sampler_info.combined_sampler_count == 2, "Got combined_sampler_count %u.\n",
            combined_sampler_info.combined_sampler_count);
    s = &combined_sampler_info.combined_samplers[0];
    ok(s->resource_space == 0, "Got resource space %u.\n", s->resource_space);
    ok(s->resource_index == 0, "Got resource index %u.\n", s->resource_index);
    ok(s->sampler_space == 0, "Got sampler space %u.\n", s->sampler_space);
    ok(s->sampler_index == 0, "Got sampler index %u.\n", s->sampler_index);
    s = &combined_sampler_info.combined_samplers[1];
    ok(s->resource_space == 0, "Got resource space %u.\n", s->resource_space);
    ok(s->resource_index == 2, "Got resource index %u.\n", s->resource_index);
    ok(s->sampler_space == 0, "Got sampler space %u.\n", s->sampler_space);
    ok(s->sampler_index == 2, "Got sampler index %u.\n", s->sampler_index);
    pfn_free_combined_sampler_info(&combined_sampler_info);

    vkd3d_shader_free_shader_code(&d3dbc);

    info.source.code = ps_5_1;
    info.source.size = sizeof(ps_5_1);
    info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;

    rc = vkd3d_shader_scan(&info, NULL);
    ok(rc == VKD3D_OK, "Got rc %d.\n", rc);
    ok(combined_sampler_info.combined_sampler_count == 3, "Got combined_sampler_count %u.\n",
            combined_sampler_info.combined_sampler_count);
    s = &combined_sampler_info.combined_samplers[0];
    ok(s->resource_space == 4, "Got resource space %u.\n", s->resource_space);
    ok(s->resource_index == 15, "Got resource index %u.\n", s->resource_index);
    ok(s->sampler_space == 6, "Got sampler space %u.\n", s->sampler_space);
    ok(s->sampler_index == 13, "Got sampler index %u.\n", s->sampler_index);
    s = &combined_sampler_info.combined_samplers[1];
    ok(s->resource_space == 5, "Got resource space %u.\n", s->resource_space);
    ok(s->resource_index == 11, "Got resource index %u.\n", s->resource_index);
    ok(s->sampler_space == 7, "Got sampler space %u.\n", s->sampler_space);
    ok(s->sampler_index == 17, "Got sampler index %u.\n", s->sampler_index);
    s = &combined_sampler_info.combined_samplers[2];
    ok(s->resource_space == 4, "Got resource space %u.\n", s->resource_space);
    ok(s->resource_index == 12, "Got resource index %u.\n", s->resource_index);
    ok(s->sampler_space == 0, "Got sampler space %u.\n", s->sampler_space);
    ok(s->sampler_index == VKD3D_SHADER_DUMMY_SAMPLER_INDEX, "Got sampler index %u.\n", s->sampler_index);
    pfn_free_combined_sampler_info(&combined_sampler_info);
}

static void test_emit_signature(void)
{
    static const uint32_t dxbc_minprec[] =
    {
        0x43425844, 0xec41e0af, 0x4d0f3dea, 0x33b9c460, 0x178f7734, 0x00000001, 0x00000310, 0x00000006,
        0x00000038, 0x000000b0, 0x00000124, 0x00000160, 0x00000264, 0x00000274, 0x46454452, 0x00000070,
        0x00000000, 0x00000000, 0x00000000, 0x0000003c, 0xffff0500, 0x00000100, 0x0000003c, 0x31314452,
        0x0000003c, 0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d,
        0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265,
        0x2e302e30, 0x31303031, 0x36312e31, 0x00343833, 0x31475349, 0x0000006c, 0x00000003, 0x00000008,
        0x00000000, 0x00000068, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x00000002,
        0x00000000, 0x00000068, 0x00000001, 0x00000000, 0x00000002, 0x00000001, 0x00000f0f, 0x00000004,
        0x00000000, 0x00000068, 0x00000002, 0x00000000, 0x00000001, 0x00000002, 0x00000f0f, 0x00000005,
        0x006d6573, 0x3147534f, 0x00000034, 0x00000001, 0x00000008, 0x00000000, 0x00000028, 0x00000000,
        0x00000000, 0x00000003, 0x00000000, 0x0000000f, 0x00000001, 0x745f7673, 0x65677261, 0xabab0074,
        0x58454853, 0x000000fc, 0x00000050, 0x0000003f, 0x0101086a, 0x04001062, 0x801010f2, 0x00008001,
        0x00000000, 0x04000862, 0x801010f2, 0x00010001, 0x00000001, 0x04000862, 0x801010f2, 0x00014001,
        0x00000002, 0x04000065, 0x801020f2, 0x00004001, 0x00000000, 0x02000068, 0x00000002, 0x0700002b,
        0x801000f2, 0x00008001, 0x00000000, 0x80101e46, 0x00010001, 0x00000001, 0x0a000000, 0x801000f2,
        0x00008001, 0x00000000, 0x80100e46, 0x00008001, 0x00000000, 0x80101e46, 0x00008001, 0x00000000,
        0x07000056, 0x801000f2, 0x00008001, 0x00000001, 0x80101e46, 0x00014001, 0x00000002, 0x0a000000,
        0x801000f2, 0x00008001, 0x00000000, 0x80100e46, 0x00008001, 0x00000000, 0x80100e46, 0x00008001,
        0x00000001, 0x07000036, 0x801020f2, 0x00004001, 0x00000000, 0x80100e46, 0x00008001, 0x00000000,
        0x0100003e, 0x30494653, 0x00000008, 0x00000010, 0x00000000, 0x54415453, 0x00000094, 0x00000006,
        0x00000002, 0x00000000, 0x00000004, 0x00000002, 0x00000000, 0x00000000, 0x00000001, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000002, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000,
    };
    static const uint32_t dxbc_big[] =
    {
        0x43425844, 0xe2dff764, 0xaee92ec8, 0xefa3485b, 0x37f05d58, 0x00000001, 0x000005f4, 0x00000005,
        0x00000034, 0x000000ac, 0x00000234, 0x000002a0, 0x00000558, 0x46454452, 0x00000070, 0x00000000,
        0x00000000, 0x00000000, 0x0000003c, 0xffff0500, 0x00000100, 0x0000003c, 0x31314452, 0x0000003c,
        0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d, 0x666f736f,
        0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265, 0x2e302e30,
        0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000180, 0x0000000b, 0x00000008, 0x00000110,
        0x00000000, 0x00000001, 0x00000003, 0x00000000, 0x0000090f, 0x0000011c, 0x00000002, 0x00000000,
        0x00000003, 0x00000001, 0x00000607, 0x0000011c, 0x00000005, 0x00000000, 0x00000001, 0x00000002,
        0x00000103, 0x00000122, 0x00000000, 0x00000007, 0x00000001, 0x00000002, 0x00000404, 0x00000131,
        0x00000000, 0x00000000, 0x00000001, 0x00000002, 0x00000008, 0x0000013f, 0x00000000, 0x00000002,
        0x00000003, 0x00000003, 0x00000103, 0x0000014f, 0x00000000, 0x00000003, 0x00000003, 0x00000003,
        0x00000404, 0x0000014f, 0x00000001, 0x00000003, 0x00000003, 0x00000003, 0x00000808, 0x0000013f,
        0x00000001, 0x00000002, 0x00000003, 0x00000004, 0x00000507, 0x0000015f, 0x00000000, 0x00000009,
        0x00000001, 0x00000005, 0x00000101, 0x0000016e, 0x00000000, 0x0000000a, 0x00000001, 0x00000005,
        0x00000202, 0x505f5653, 0x7469736f, 0x006e6f69, 0x4f4c4f43, 0x56530052, 0x6972505f, 0x6974696d,
        0x44496576, 0x5f565300, 0x74736e49, 0x65636e61, 0x53004449, 0x6c435f56, 0x69447069, 0x6e617473,
        0x53006563, 0x75435f56, 0x69446c6c, 0x6e617473, 0x53006563, 0x73495f56, 0x6e6f7246, 0x63614674,
        0x56530065, 0x6d61535f, 0x49656c70, 0x7865646e, 0xababab00, 0x4e47534f, 0x00000064, 0x00000003,
        0x00000008, 0x00000050, 0x00000001, 0x00000000, 0x00000002, 0x00000001, 0x00000a07, 0x00000050,
        0x00000005, 0x00000000, 0x00000003, 0x00000005, 0x00000e01, 0x0000005a, 0x00000000, 0x00000000,
        0x00000003, 0xffffffff, 0x00000e01, 0x545f5653, 0x65677261, 0x56530074, 0x7065445f, 0xab006874,
        0x58454853, 0x000002b0, 0x00000050, 0x000000ac, 0x0100086a, 0x04002064, 0x00101092, 0x00000000,
        0x00000001, 0x03001062, 0x00101062, 0x00000001, 0x03000862, 0x00101012, 0x00000002, 0x04000863,
        0x00101042, 0x00000002, 0x00000007, 0x04001064, 0x00101012, 0x00000003, 0x00000002, 0x04001064,
        0x00101042, 0x00000003, 0x00000003, 0x04001064, 0x00101082, 0x00000003, 0x00000003, 0x04001064,
        0x00101052, 0x00000004, 0x00000002, 0x04000863, 0x00101012, 0x00000005, 0x00000009, 0x04000863,
        0x00101022, 0x00000005, 0x0000000a, 0x03000065, 0x00102052, 0x00000001, 0x03000065, 0x00102012,
        0x00000005, 0x02000065, 0x0000c001, 0x02000068, 0x00000001, 0x07000000, 0x00100012, 0x00000000,
        0x0010102a, 0x00000001, 0x0010101a, 0x00000001, 0x05000056, 0x00100022, 0x00000000, 0x0010100a,
        0x00000002, 0x07000000, 0x00100012, 0x00000000, 0x0010001a, 0x00000000, 0x0010000a, 0x00000000,
        0x07000000, 0x00100012, 0x00000000, 0x0010000a, 0x00000000, 0x0010100a, 0x00000000, 0x07000000,
        0x00100012, 0x00000000, 0x0010000a, 0x00000000, 0x0010103a, 0x00000000, 0x07000000, 0x00100012,
        0x00000000, 0x0010000a, 0x00000000, 0x0010100a, 0x00000003, 0x07000000, 0x00100012, 0x00000000,
        0x0010000a, 0x00000000, 0x0010100a, 0x00000004, 0x07000000, 0x00100012, 0x00000000, 0x0010000a,
        0x00000000, 0x0010102a, 0x00000004, 0x07000000, 0x00100012, 0x00000000, 0x0010000a, 0x00000000,
        0x0010102a, 0x00000003, 0x07000000, 0x00100012, 0x00000000, 0x0010000a, 0x00000000, 0x0010103a,
        0x00000003, 0x05000056, 0x00100022, 0x00000000, 0x0010102a, 0x00000002, 0x07000000, 0x00100012,
        0x00000000, 0x0010001a, 0x00000000, 0x0010000a, 0x00000000, 0x07000001, 0x00100022, 0x00000000,
        0x0010100a, 0x00000005, 0x00004001, 0x3f800000, 0x07000000, 0x00100012, 0x00000000, 0x0010001a,
        0x00000000, 0x0010000a, 0x00000000, 0x05000056, 0x00100022, 0x00000000, 0x0010101a, 0x00000005,
        0x07000000, 0x00100012, 0x00000000, 0x0010001a, 0x00000000, 0x0010000a, 0x00000000, 0x0500001b,
        0x00102052, 0x00000001, 0x00100006, 0x00000000, 0x05000036, 0x00102012, 0x00000005, 0x0010000a,
        0x00000000, 0x04000036, 0x0000c001, 0x0010000a, 0x00000000, 0x0100003e, 0x54415453, 0x00000094,
        0x00000014, 0x00000001, 0x00000000, 0x0000000d, 0x0000000c, 0x00000000, 0x00000001, 0x00000001,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000002, 0x00000000, 0x00000004, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
    };
    static const uint32_t dxbc_depthge[] =
    {
        0x43425844, 0x520689ae, 0xf26554e6, 0x8f1d15ed, 0xcdf56fa5, 0x00000001, 0x000002a4, 0x00000006,
        0x00000038, 0x000000b0, 0x000000c0, 0x0000016c, 0x000001f8, 0x00000208, 0x46454452, 0x00000070,
        0x00000000, 0x00000000, 0x00000000, 0x0000003c, 0xffff0500, 0x00000100, 0x0000003c, 0x31314452,
        0x0000003c, 0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d,
        0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265,
        0x2e302e30, 0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000008, 0x00000000, 0x00000008,
        0x4e47534f, 0x000000a4, 0x00000004, 0x00000008, 0x00000068, 0x00000000, 0x00000000, 0x00000003,
        0x00000000, 0x0000000f, 0x00000072, 0x00000000, 0x00000000, 0x00000003, 0xffffffff, 0x00000e01,
        0x00000087, 0x00000000, 0x00000000, 0x00000001, 0xffffffff, 0x00000e01, 0x00000093, 0x00000000,
        0x00000000, 0x00000003, 0xffffffff, 0x00000e01, 0x545f5653, 0x65677261, 0x56530074, 0x5045445f,
        0x72474854, 0x65746165, 0x75714572, 0x73006c61, 0x6f635f76, 0x41726576, 0x73004547, 0x74735f76,
        0x69636e65, 0x6665726c, 0xababab00, 0x58454853, 0x00000084, 0x00000050, 0x00000021, 0x0100086a,
        0x03000065, 0x001020f2, 0x00000000, 0x02000065, 0x00026001, 0x02000065, 0x0000f000, 0x02000065,
        0x00029001, 0x08000036, 0x001020f2, 0x00000000, 0x00004002, 0x3f800000, 0x40000000, 0x40400000,
        0x40800000, 0x04000036, 0x00026001, 0x00004001, 0x3f000000, 0x04000036, 0x0000f001, 0x00004001,
        0x00000003, 0x04000036, 0x00029001, 0x00004001, 0x3f333333, 0x0100003e, 0x30494653, 0x00000008,
        0x00000200, 0x00000000, 0x54415453, 0x00000094, 0x00000005, 0x00000000, 0x00000000, 0x00000004,
        0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000004,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000,
    };
    static const uint32_t dxbc_depthle[] =
    {
        0x43425844, 0x621ee545, 0x22c68205, 0x48945607, 0xc76952a8, 0x00000001, 0x00000238, 0x00000005,
        0x00000034, 0x000000ac, 0x000000bc, 0x00000118, 0x0000019c, 0x46454452, 0x00000070, 0x00000000,
        0x00000000, 0x00000000, 0x0000003c, 0xffff0500, 0x00000100, 0x0000003c, 0x31314452, 0x0000003c,
        0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d, 0x666f736f,
        0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265, 0x2e302e30,
        0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000008, 0x00000000, 0x00000008, 0x4e47534f,
        0x00000054, 0x00000002, 0x00000008, 0x00000038, 0x00000000, 0x00000000, 0x00000003, 0x00000000,
        0x0000000f, 0x00000042, 0x00000000, 0x00000000, 0x00000003, 0xffffffff, 0x00000e01, 0x545f5653,
        0x65677261, 0x56530074, 0x5045445f, 0x656c4854, 0x51457373, 0x004c4155, 0x58454853, 0x0000007c,
        0x00000050, 0x0000001f, 0x0100086a, 0x0200005f, 0x00023001, 0x03000065, 0x001020f2, 0x00000000,
        0x02000065, 0x00027001, 0x02000068, 0x00000001, 0x08000036, 0x001020f2, 0x00000000, 0x00004002,
        0x3f800000, 0x40000000, 0x40400000, 0x40800000, 0x04000056, 0x00100012, 0x00000000, 0x0002300a,
        0x06000000, 0x00027001, 0x0010000a, 0x00000000, 0x00004001, 0x3f000000, 0x0100003e, 0x54415453,
        0x00000094, 0x00000004, 0x00000001, 0x00000000, 0x00000003, 0x00000001, 0x00000000, 0x00000000,
        0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000001, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
    };
    static const uint32_t dxbc_hs[] =
    {
        0x43425844, 0x943f0a0f, 0x97e66e61, 0x82e4cc08, 0xce70cbd1, 0x00000001, 0x0000064c, 0x00000006,
        0x00000038, 0x000000b0, 0x000000e0, 0x00000110, 0x000001f0, 0x000005b0, 0x46454452, 0x00000070,
        0x00000000, 0x00000000, 0x00000000, 0x0000003c, 0x48530500, 0x00000100, 0x0000003c, 0x31314452,
        0x0000003c, 0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d,
        0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265,
        0x2e302e30, 0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000028, 0x00000001, 0x00000008,
        0x00000020, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x786d6573, 0xababab00,
        0x4e47534f, 0x00000028, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000000, 0x00000003,
        0x00000000, 0x0000000f, 0x786d6573, 0xababab00, 0x47534350, 0x000000d8, 0x00000007, 0x00000008,
        0x000000b0, 0x00000000, 0x0000000b, 0x00000003, 0x00000000, 0x00000e01, 0x000000b0, 0x00000001,
        0x0000000b, 0x00000003, 0x00000001, 0x00000e01, 0x000000b0, 0x00000002, 0x0000000b, 0x00000003,
        0x00000002, 0x00000e01, 0x000000b0, 0x00000003, 0x0000000b, 0x00000003, 0x00000003, 0x00000e01,
        0x000000be, 0x00000000, 0x0000000c, 0x00000003, 0x00000004, 0x00000e01, 0x000000be, 0x00000001,
        0x0000000c, 0x00000003, 0x00000005, 0x00000e01, 0x000000d2, 0x00000000, 0x00000000, 0x00000003,
        0x00000006, 0x0000000f, 0x545f5653, 0x46737365, 0x6f746361, 0x56530072, 0x736e495f, 0x54656469,
        0x46737365, 0x6f746361, 0x65730072, 0xab00786d, 0x58454853, 0x000003b8, 0x00030050, 0x000000ee,
        0x01000071, 0x0100b093, 0x01008094, 0x01001895, 0x01000896, 0x01001897, 0x0100086a, 0x00001835,
        0x00000012, 0x3f000000, 0x3dcccccd, 0x00000000, 0x00000000, 0x3f19999a, 0x3e4ccccd, 0x00000000,
        0x00000000, 0x00000000, 0x3e99999a, 0x00000000, 0x00000000, 0x00000000, 0x3ecccccd, 0x00000000,
        0x00000000, 0x01000072, 0x0200005f, 0x00016000, 0x0200005f, 0x0000b000, 0x0400005f, 0x002010f2,
        0x00000016, 0x00000000, 0x03000065, 0x001020f2, 0x00000000, 0x02000068, 0x00000002, 0x04000056,
        0x00100012, 0x00000000, 0x00016001, 0x04000056, 0x00100022, 0x00000000, 0x0000b001, 0x08000036,
        0x001000c2, 0x00000000, 0x00004002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x04000036,
        0x00100012, 0x00000001, 0x00016001, 0x09000000, 0x001020f2, 0x00000000, 0x00100e46, 0x00000000,
        0x00a01e46, 0x0010000a, 0x00000001, 0x00000000, 0x0100003e, 0x01000073, 0x02000099, 0x00000004,
        0x0200005f, 0x00017000, 0x04000067, 0x00102012, 0x00000000, 0x0000000b, 0x04000067, 0x00102012,
        0x00000001, 0x0000000c, 0x04000067, 0x00102012, 0x00000002, 0x0000000d, 0x04000067, 0x00102012,
        0x00000003, 0x0000000e, 0x02000068, 0x00000001, 0x0400005b, 0x00102012, 0x00000000, 0x00000004,
        0x04000036, 0x00100012, 0x00000000, 0x0001700a, 0x07000036, 0x00902012, 0x0010000a, 0x00000000,
        0x0090901a, 0x0010000a, 0x00000000, 0x0100003e, 0x01000073, 0x02000099, 0x00000002, 0x0200005f,
        0x00017000, 0x04000067, 0x00102012, 0x00000004, 0x0000000f, 0x04000067, 0x00102012, 0x00000005,
        0x00000010, 0x02000068, 0x00000001, 0x0400005b, 0x00102012, 0x00000004, 0x00000002, 0x04000036,
        0x00100012, 0x00000000, 0x0001700a, 0x08000036, 0x00d02012, 0x00000004, 0x0010000a, 0x00000000,
        0x0090900a, 0x0010000a, 0x00000000, 0x0100003e, 0x01000073, 0x0200005f, 0x0000b000, 0x0400005f,
        0x00219012, 0x00000016, 0x00000000, 0x03000065, 0x00102012, 0x00000006, 0x02000068, 0x00000001,
        0x04000036, 0x00100012, 0x00000000, 0x0000b001, 0x07000036, 0x00102012, 0x00000006, 0x00a1900a,
        0x0010000a, 0x00000000, 0x00000000, 0x0100003e, 0x01000073, 0x0200005f, 0x0000b000, 0x0400005f,
        0x00219022, 0x00000016, 0x00000000, 0x03000065, 0x00102022, 0x00000006, 0x02000068, 0x00000001,
        0x04000036, 0x00100012, 0x00000000, 0x0000b001, 0x07000036, 0x00102022, 0x00000006, 0x00a1901a,
        0x0010000a, 0x00000000, 0x00000000, 0x0100003e, 0x01000073, 0x0200005f, 0x0000b000, 0x0400005f,
        0x00219042, 0x00000016, 0x00000000, 0x03000065, 0x00102042, 0x00000006, 0x02000068, 0x00000001,
        0x04000036, 0x00100012, 0x00000000, 0x0000b001, 0x07000036, 0x00102042, 0x00000006, 0x00a1902a,
        0x0010000a, 0x00000000, 0x00000000, 0x0100003e, 0x01000073, 0x0200005f, 0x0000b000, 0x0400005f,
        0x00219082, 0x00000016, 0x00000000, 0x03000065, 0x00102082, 0x00000006, 0x02000068, 0x00000001,
        0x04000036, 0x00100012, 0x00000000, 0x0000b001, 0x07000036, 0x00102082, 0x00000006, 0x00a1903a,
        0x0010000a, 0x00000000, 0x00000000, 0x0100003e, 0x54415453, 0x00000094, 0x00000018, 0x00000002,
        0x00000004, 0x00000007, 0x00000001, 0x00000000, 0x00000000, 0x00000007, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x0000000e, 0x00000000, 0x00000002, 0x00000000, 0x0000001d, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000010, 0x00000003, 0x00000001, 0x00000003,
        0x00000000, 0x00000000, 0x00000000,
    };
    static const uint32_t dxbc_ds[] =
    {
        0x43425844, 0x3744324a, 0x2e7f6684, 0x41142245, 0x9e6ecf04, 0x00000001, 0x0000042c, 0x00000006,
        0x00000038, 0x000000b0, 0x000000e0, 0x000001c0, 0x000001f4, 0x00000390, 0x46454452, 0x00000070,
        0x00000000, 0x00000000, 0x00000000, 0x0000003c, 0x44530500, 0x00000100, 0x0000003c, 0x31314452,
        0x0000003c, 0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d,
        0x666f736f, 0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265,
        0x2e302e30, 0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000028, 0x00000001, 0x00000008,
        0x00000020, 0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x786d6573, 0xababab00,
        0x47534350, 0x000000d8, 0x00000007, 0x00000008, 0x000000b0, 0x00000000, 0x0000000b, 0x00000003,
        0x00000000, 0x00000001, 0x000000b0, 0x00000001, 0x0000000b, 0x00000003, 0x00000001, 0x00000001,
        0x000000b0, 0x00000002, 0x0000000b, 0x00000003, 0x00000002, 0x00000101, 0x000000b0, 0x00000003,
        0x0000000b, 0x00000003, 0x00000003, 0x00000001, 0x000000be, 0x00000000, 0x0000000c, 0x00000003,
        0x00000004, 0x00000101, 0x000000be, 0x00000001, 0x0000000c, 0x00000003, 0x00000005, 0x00000001,
        0x000000d2, 0x00000000, 0x00000000, 0x00000003, 0x00000006, 0x00000f0f, 0x545f5653, 0x46737365,
        0x6f746361, 0x56530072, 0x736e495f, 0x54656469, 0x46737365, 0x6f746361, 0x65730072, 0xab00786d,
        0x4e47534f, 0x0000002c, 0x00000001, 0x00000008, 0x00000020, 0x00000000, 0x00000001, 0x00000003,
        0x00000000, 0x0000000f, 0x705f5653, 0x7469736f, 0x006e6f69, 0x58454853, 0x00000194, 0x00040050,
        0x00000065, 0x01008093, 0x01001895, 0x0100086a, 0x04000061, 0x0011b012, 0x00000002, 0x0000000d,
        0x04000061, 0x0011b012, 0x00000004, 0x0000000f, 0x0300005f, 0x0011b0f2, 0x00000006, 0x0200005f,
        0x0001c032, 0x0400005f, 0x002190f2, 0x00000010, 0x00000000, 0x04000067, 0x001020f2, 0x00000000,
        0x00000001, 0x02000068, 0x00000002, 0x04000036, 0x00100032, 0x00000000, 0x0001c046, 0x05000036,
        0x001000c2, 0x00000000, 0x0011b006, 0x00000002, 0x07000000, 0x001000f2, 0x00000000, 0x00100e46,
        0x00000000, 0x0011be46, 0x00000006, 0x05000036, 0x00100012, 0x00000001, 0x0011b00a, 0x00000002,
        0x05000036, 0x00100042, 0x00000001, 0x0011b00a, 0x00000004, 0x07000000, 0x001000f2, 0x00000000,
        0x00100e46, 0x00000000, 0x00100a06, 0x00000001, 0x05000036, 0x00100012, 0x00000001, 0x0011b00a,
        0x00000004, 0x06000036, 0x001000c2, 0x00000001, 0x00219ea6, 0x00000004, 0x00000000, 0x07000000,
        0x001000f2, 0x00000000, 0x00100e46, 0x00000000, 0x00100e06, 0x00000001, 0x06000036, 0x00100032,
        0x00000001, 0x00219046, 0x00000004, 0x00000000, 0x08000036, 0x001000c2, 0x00000001, 0x00004002,
        0x00000000, 0x00000000, 0x40400000, 0x40800000, 0x07000000, 0x001020f2, 0x00000000, 0x00100e46,
        0x00000000, 0x00100e46, 0x00000001, 0x0100003e, 0x54415453, 0x00000094, 0x0000000d, 0x00000002,
        0x00000000, 0x00000006, 0x00000004, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000008, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000010, 0x00000000, 0x00000000, 0x00000003,
        0x00000000, 0x00000000, 0x00000000,
    };
    static const uint32_t dxbc_gs[] =
    {
        0x43425844, 0xe27a1766, 0x53623db9, 0x85065db9, 0x79f2e81e, 0x00000001, 0x00000324, 0x00000005,
        0x00000034, 0x000000ac, 0x000000dc, 0x00000174, 0x00000288, 0x46454452, 0x00000070, 0x00000000,
        0x00000000, 0x00000000, 0x0000003c, 0x47530500, 0x00000100, 0x0000003c, 0x31314452, 0x0000003c,
        0x00000018, 0x00000020, 0x00000028, 0x00000024, 0x0000000c, 0x00000000, 0x7263694d, 0x666f736f,
        0x52282074, 0x4c482029, 0x53204c53, 0x65646168, 0x6f432072, 0x6c69706d, 0x31207265, 0x2e302e30,
        0x31303031, 0x36312e31, 0x00343833, 0x4e475349, 0x00000028, 0x00000001, 0x00000008, 0x00000020,
        0x00000000, 0x00000000, 0x00000003, 0x00000000, 0x00000f0f, 0x7a6d6573, 0xababab00, 0x3547534f,
        0x00000090, 0x00000004, 0x00000008, 0x00000000, 0x00000078, 0x00000000, 0x00000001, 0x00000003,
        0x00000000, 0x0000000f, 0x00000000, 0x00000084, 0x00000000, 0x00000000, 0x00000003, 0x00000001,
        0x0000000f, 0x00000001, 0x00000078, 0x00000000, 0x00000001, 0x00000003, 0x00000000, 0x0000000f,
        0x00000001, 0x00000089, 0x00000000, 0x00000000, 0x00000003, 0x00000001, 0x0000000f, 0x505f5653,
        0x5449534f, 0x004e4f49, 0x786d6573, 0x6d657300, 0xabab0079, 0x58454853, 0x0000010c, 0x00020050,
        0x00000043, 0x0100086a, 0x0400005f, 0x002010f2, 0x00000003, 0x00000000, 0x0100185d, 0x0300008f,
        0x00110000, 0x00000000, 0x0100085c, 0x04000067, 0x001020f2, 0x00000000, 0x00000001, 0x03000065,
        0x001020f2, 0x00000001, 0x0300008f, 0x00110000, 0x00000001, 0x0100085c, 0x04000067, 0x001020f2,
        0x00000000, 0x00000001, 0x03000065, 0x001020f2, 0x00000001, 0x0200005e, 0x0000000c, 0x08000036,
        0x001020f2, 0x00000000, 0x00004002, 0x3f800000, 0x40000000, 0x40400000, 0x40800000, 0x06000036,
        0x001020f2, 0x00000001, 0x00201e46, 0x00000000, 0x00000000, 0x03000075, 0x00110000, 0x00000000,
        0x08000036, 0x001020f2, 0x00000000, 0x00004002, 0x41300000, 0x41400000, 0x41500000, 0x41600000,
        0x06000036, 0x001020f2, 0x00000001, 0x00201e46, 0x00000001, 0x00000000, 0x03000075, 0x00110000,
        0x00000001, 0x0100003e, 0x54415453, 0x00000094, 0x00000007, 0x00000000, 0x00000000, 0x00000005,
        0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000002, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000003, 0x00000001, 0x0000000c, 0x00000000, 0x00000000,
        0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        0x00000000,
    };

    static const struct emit_signature_test
    {
        const char *profile;
        const struct vkd3d_shader_code dxbc;
        const char *source;
        const char *signature;
        bool is_todo;
    } tests[] =
    {
        {
            "ps_5_0",
            {NULL, 0},
            "float4 main() : SV_Target\n"
            "{\n"
            "    return float4(1.0, 2.0, 3.0, 4.0);\n"
            "}\n",
            "ps_5_0\n"
            ".output\n"
            ".param SV_Target.xyzw, o0.xyzw, float, TARGET\n",
            false
        },
        {
            "ps_5_0",
            {dxbc_minprec, sizeof(dxbc_minprec)},
            "min16float4 main(min10float4 x : sem0, min16int4 y : sem1, min16uint4 z : sem2) : sv_target\n"
            "{\n"
            "    return x + y + z;\n"
            "}\n",
            "ps_5_0\n"
            ".input\n"
            ".param sem.xyzw, v0.xyzw, float, NONE, FIXED_8_2\n"
            ".param sem1.xyzw, v1.xyzw, int, NONE, INT_16\n"
            ".param sem2.xyzw, v2.xyzw, uint, NONE, UINT_16\n"
            ".output\n"
            ".param sv_target.xyzw, o0.xyzw, float, TARGET, FLOAT_16\n",
            false
        },
        {
            "ps_5_0",
            {NULL, 0},
            "void main(float4 pos : SV_POSITION, float3 color : COLOR2, uint2 color2 : COLOR5,\n"
            "        out int3 target : sv_target1, out float target2 : SV_TaRgEt5)\n"
            "{\n"
            "    float tmp = length(pos) + length(color) + length(color2);\n"
            "    target.xyz = tmp;\n"
            "    target2 = tmp;\n"
            "}\n",
            "ps_5_0\n"
            ".input\n"
            ".param SV_POSITION.xyzw, v0.xyzw, float, POS\n"
            ".param COLOR2.xyz, v1.xyz, float\n"
            ".param COLOR5.xy, v2.xy, uint\n"
            ".output\n"
            ".param sv_target1.xyz, o1.xyz, int, TARGET\n"
            ".param SV_TaRgEt5.x, o5.x, float, TARGET\n",
            false
        },
        {
            "ps_5_0",
            {NULL, 0},
            "void main(float4 pos : SV_Position, float3 color : COLOR2, uint2 color2 : COLOR5,\n"
            "        out int3 target : SV_Target1, out float target2 : SV_Target5)\n"
            "{\n"
            "    float tmp = pos.x + pos.w + color.y + color.z + color2.x;\n"
            "    target.xz = tmp;\n"
            "    target2 = tmp;\n"
            "}\n",
            "ps_5_0\n"
            ".input\n"
            ".param SV_Position.xyzw, v0.xw, float, POS\n"
            ".param COLOR2.xyz, v1.yz, float\n"
            ".param COLOR5.xy, v2.x, uint\n"
            ".output\n"
            ".param SV_Target1.xyz, o1.xz, int, TARGET\n"
            ".param SV_Target5.x, o5.x, float, TARGET\n",
            true
        },
        {
            "ps_5_0",
            {dxbc_big, sizeof(dxbc_big)},
            "void main(float4 pos : SV_Position, float3 color : COLOR2, uint2 color2 : COLOR5,\n"
            "        float2 clip : SV_ClipDistance0, float3 clip2 : SV_ClipDistance1,\n"
            "        float1 cull : SV_CullDistance0, float cull2 : SV_CullDistance1,\n"
            "        uint prim : SV_PrimitiveID, uint inst : SV_InstanceID,\n"
            "        bool front : SV_IsFrontFace0, uint sample : SV_SampleIndex,\n"
            "        out int3 target : SV_Target1, out float target2 : SV_Target5,\n"
            "        out float depth : SV_Depth)\n"
            "{\n"
            "    float tmp = color.y + color.z + color2 + pos.x + pos.w + clip.x + clip2.x + clip2.z\n"
            "            + cull.x + cull2 + prim + front + sample;\n"
            "    target.xz = tmp;\n"
            "    target2 = tmp;\n"
            "    depth = tmp;\n"
            "}\n",
            "ps_5_0\n"
            ".input\n"
            ".param SV_Position.xyzw, v0.xw, float, POS\n"
            ".param COLOR2.xyz, v1.yz, float\n"
            ".param COLOR5.xy, v2.x, uint\n"
            ".param SV_PrimitiveID.z, v2.z, uint, PRIMID\n"
            ".param SV_InstanceID.w, v2.w, uint\n"
            ".param SV_ClipDistance.xy, v3.x, float, CLIPDST\n"
            ".param SV_CullDistance.z, v3.z, float, CULLDST\n"
            ".param SV_CullDistance1.w, v3.w, float, CULLDST\n"
            ".param SV_ClipDistance1.xyz, v4.xz, float, CLIPDST\n"
            ".param SV_IsFrontFace.x, v5.x, uint, FFACE\n"
            ".param SV_SampleIndex.y, v5.y, uint, SAMPLE\n"
            ".output\n"
            ".param SV_Target1.xyz, o1.xz, int, TARGET\n"
            ".param SV_Target5.x, o5.x, float, TARGET\n"
            ".param SV_Depth, oDepth, float, DEPTH\n",
            true
        },
        {
            "ps_5_0",
            {dxbc_depthge, sizeof(dxbc_depthge)},
            "float4 main(out float depth : SV_DEPTHGreaterEqual, out uint cov : sv_coverAGE,\n"
            "        out float stref : sv_stencilref) : SV_Target\n"
            "{\n"
            "    depth = 0.5;\n"
            "    cov = 3;\n"
            "    stref = 0.7;\n"
            "    return float4(1.0, 2.0, 3.0, 4.0);\n"
            "}\n",
            "ps_5_0\n"
            ".output\n"
            ".param SV_Target.xyzw, o0.xyzw, float, TARGET\n"
            ".param SV_DEPTHGreaterEqual, oDepthGE, float, DEPTHGE\n"
            ".param sv_coverAGE, oMask, uint, COVERAGE\n"
            ".param sv_stencilref, oStencilRef, float, STENCILREF\n",
            false
        },
        {
            "ps_5_0",
            {dxbc_depthle, sizeof(dxbc_depthle)},
            "float4 main(out float depth : SV_DEPTHlessEQUAL, uint cov : sv_coverage) : SV_Target\n"
            "{\n"
            "    depth = 0.5 + cov;\n"
            "    return float4(1.0, 2.0, 3.0, 4.0);\n"
            "}\n",
            "ps_5_0\n"
            ".output\n"
            ".param SV_Target.xyzw, o0.xyzw, float, TARGET\n"
            ".param SV_DEPTHlessEQUAL, oDepthLE, float, DEPTHLE\n",
            false
        },
        {
            "hs_5_0",
            {dxbc_hs, sizeof(dxbc_hs)},
            "struct input_data\n"
            "{\n"
            "    float4 x : semx;\n"
            "};\n"
            "\n"
            "struct constant_data\n"
            "{\n"
            "    float edges[4] : SV_TessFactor;\n"
            "    float inside[2] : SV_InsideTessFactor;\n"
            "    float4 x : semx;\n"
            "};\n"
            "\n"
            "struct control_point_data\n"
            "{\n"
            "    float4 x : semx;\n"
            "};\n"
            "\n"
            "constant_data patch_constant(InputPatch<input_data, 22> ip, uint patch_id : SV_PrimitiveID)\n"
            "{\n"
            "    constant_data ret;\n"
            "    ret.edges[0] = 0.1;\n"
            "    ret.edges[1] = 0.2;\n"
            "    ret.edges[2] = 0.3;\n"
            "    ret.edges[3] = 0.4;\n"
            "    ret.inside[0] = 0.5;\n"
            "    ret.inside[1] = 0.6;\n"
            "    ret.x = ip[patch_id].x;\n"
            "    return ret;\n"
            "}\n"
            "\n"
            "[domain(\"quad\")]\n"
            "[partitioning(\"integer\")]\n"
            "[outputtopology(\"triangle_cw\")]\n"
            "[outputcontrolpoints(16)]\n"
            "[patchconstantfunc(\"patch_constant\")]\n"
            "control_point_data main(InputPatch<input_data, 22> ip, uint i : SV_OutputControlPointID, \n"
            "        uint patch_id : SV_PrimitiveID)\n"
            "{\n"
            "    control_point_data ret;\n"
            "    ret.x = ip[i].x + float4(i, patch_id, 0, 0);\n"
            "    return ret;\n"
            "}\n",
            "hs_5_0\n"
            ".input\n"
            ".param semx.xyzw, v0.xyzw, float\n"
            ".output\n"
            ".param semx.xyzw, o0.xyzw, float\n"
            ".patch_constant\n"
            ".param SV_TessFactor.x, o0.x, float, QUADEDGE\n"
            ".param SV_TessFactor1.x, o1.x, float, QUADEDGE\n"
            ".param SV_TessFactor2.x, o2.x, float, QUADEDGE\n"
            ".param SV_TessFactor3.x, o3.x, float, QUADEDGE\n"
            ".param SV_InsideTessFactor.x, o4.x, float, QUADINT\n"
            ".param SV_InsideTessFactor1.x, o5.x, float, QUADINT\n"
            ".param semx.xyzw, o6.xyzw, float\n",
            false
        },
        {
            "ds_5_0",
            {dxbc_ds, sizeof(dxbc_ds)},
            "struct constant_data\n"
            "{\n"
            "    float edges[4] : SV_TessFactor;\n"
            "    float inside[2] : SV_InsideTessFactor;\n"
            "    float4 x : semx;\n"
            "};\n"
            "\n"
            "struct control_point_data\n"
            "{\n"
            "    float4 x : semx;\n"
            "};\n"
            "\n"
            "[domain(\"quad\")]\n"
            "float4 main(constant_data input, float2 location : SV_DomainLocation,\n"
            "        const OutputPatch<control_point_data, 16> patch) : SV_position\n"
            "{\n"
            "    return float4(location, 3.0, 4.0) + input.x + input.edges[2] + input.inside[0] + patch[4].x;\n"
            "}\n",
            "ds_5_0\n"
            ".input\n"
            ".param semx.xyzw, vicp0.xyzw, float\n"
            ".output\n"
            ".param SV_position.xyzw, o0.xyzw, float, POS\n"
            ".patch_constant\n"
            ".param SV_TessFactor.x, vpc0, float, QUADEDGE\n"
            ".param SV_TessFactor1.x, vpc1, float, QUADEDGE\n"
            ".param SV_TessFactor2.x, vpc2.x, float, QUADEDGE\n"
            ".param SV_TessFactor3.x, vpc3, float, QUADEDGE\n"
            ".param SV_InsideTessFactor.x, vpc4.x, float, QUADINT\n"
            ".param SV_InsideTessFactor1.x, vpc5, float, QUADINT\n"
            ".param semx.xyzw, vpc6.xyzw, float\n",
            false
        },
        {
            "gs_5_0",
            {dxbc_gs, sizeof(dxbc_gs)},
            "struct input_data\n"
            "{\n"
            "    float4 z : semz;\n"
            "};\n"
            "\n"
            "struct output_data\n"
            "{\n"
            "    float4 pos : SV_POSITION;\n"
            "    float4 x : semx;\n"
            "};\n"
            "struct output_data2\n"
            "{\n"
            "    float4 pos : SV_POSITION;\n"
            "    float4 y : semy;\n"
            "};\n"
            "\n"
            "[maxvertexcount(12)]\n"
            "void main(triangle input_data input[3], inout PointStream<output_data> s, inout PointStream<output_data2> s2)\n"
            "{\n"
            "    output_data output;\n"
            "    output.pos = float4(1.0, 2.0, 3.0, 4.0);\n"
            "    output.x = input[0].z;\n"
            "    s.Append(output);\n"
            "    output_data2 output2;\n"
            "    output2.pos = float4(11.0, 12.0, 13.0, 14.0);\n"
            "    output2.y = input[1].z;\n"
            "    s2.Append(output2);\n"
            "}\n",
            "gs_5_0\n"
            ".input\n"
            ".param semz.xyzw, v0.xyzw, float\n"
            ".output\n"
            ".param SV_POSITION.xyzw, o0.xyzw, float, POS\n"
            ".param semx.xyzw, o1.xyzw, float\n"
            ".param SV_POSITION.xyzw, o0.xyzw, float, POS, NONE, m1\n"
            ".param semy.xyzw, o1.xyzw, float, NONE, NONE, m1\n",
            false
        },
    };

    struct vkd3d_shader_hlsl_source_info hlsl_info =
    {
        .type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO,
        .entry_point = "main",
    };
    struct vkd3d_shader_compile_info compile_info =
    {
        .type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO,
        .next = &hlsl_info,
        .source_type = VKD3D_SHADER_SOURCE_HLSL,
        .target_type = VKD3D_SHADER_TARGET_DXBC_TPF,
        .log_level = VKD3D_SHADER_LOG_NONE,
    };
    struct vkd3d_shader_compile_option disassemble_options[] =
    {
        {
            .name = VKD3D_SHADER_COMPILE_OPTION_FORMATTING,
            .value = VKD3D_SHADER_COMPILE_OPTION_FORMATTING_IO_SIGNATURES
        },
    };
    struct vkd3d_shader_compile_info disassemble_info =
    {
        .type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO,
        .source_type = VKD3D_SHADER_SOURCE_DXBC_TPF,
        .target_type = VKD3D_SHADER_TARGET_D3D_ASM,
        .options = disassemble_options,
        .option_count = ARRAY_SIZE(disassemble_options),
        .log_level = VKD3D_SHADER_LOG_NONE,
    };
    struct vkd3d_shader_code dxbc, disasm;
    unsigned int i;
    char *ptr;
    int rc;

    for (i = 0; i < ARRAY_SIZE(tests); ++i)
    {
        const struct emit_signature_test *test = &tests[i];

        vkd3d_test_push_context("%u", i);

        if (test->dxbc.size == 0)
        {
            hlsl_info.profile = test->profile;
            compile_info.source.code = test->source;
            compile_info.source.size = strlen(test->source);
            rc = vkd3d_shader_compile(&compile_info, &dxbc, NULL);
            ok(rc == VKD3D_OK, "Cannot compile HLSL shader, rc %d.\n", rc);
            disassemble_info.source = dxbc;
        }
        else
        {
            disassemble_info.source = test->dxbc;
        }

        rc = vkd3d_shader_compile(&disassemble_info, &disasm, NULL);
        ok(rc == VKD3D_OK, "Cannot disassemble shader, rc %d.\n", rc);

        ptr = strstr(disasm.code, ".text\n");
        ok(ptr, "Cannot find text marker in disassembled code.\n");
        *ptr = '\0';
        todo_if(test->is_todo)
        ok(strcmp(disasm.code, test->signature) == 0, "Unexpected signature description.\n");

        if (test->dxbc.size == 0)
            vkd3d_shader_free_shader_code(&dxbc);
        vkd3d_shader_free_shader_code(&disasm);

        vkd3d_test_pop_context();
    }
}

START_TEST(vkd3d_shader_api)
{
    setlocale(LC_ALL, "");

    run_test(test_invalid_shaders);
    run_test(test_vkd3d_shader_pfns);
    run_test(test_version);
    run_test(test_d3dbc);
    run_test(test_dxbc);
    run_test(test_scan_signatures);
    run_test(test_scan_descriptors);
    run_test(test_build_varying_map);
    run_test(test_scan_combined_resource_samplers);
    run_test(test_emit_signature);
}
