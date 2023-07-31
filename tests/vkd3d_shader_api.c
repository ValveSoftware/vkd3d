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
        {"POSITION",        0, 0, VKD3D_SHADER_SV_POSITION, VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"TEXCOORD",        2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0xf, 0xf},
        {"COLOR",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 8, 0xf, 0xf},
        {"FOG",             0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 1, 0x1, 0x1},
        {"PSIZE",           0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2, 0x1, 0x1},
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
        {"sv_target",   2, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 2,   0xf, 0xf},
        {"sv_depth",    0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, ~0u, 0x1, 0x1},
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
        {"COLOR",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
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
        {"COLOR",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0xf, 0xf},
        {"DEPTH",       0, 0, VKD3D_SHADER_SV_NONE,     VKD3D_SHADER_COMPONENT_FLOAT, 0, 0x1, 0x1},
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
        const struct vkd3d_shader_signature_element *inputs;
        size_t input_count;
        const struct vkd3d_shader_signature_element *outputs;
        size_t output_count;
        const struct vkd3d_shader_signature_element *patch_constants;
        size_t patch_constant_count;
    }
    tests[] =
    {
        {vs1_source, true,  "vs_4_0", vs1_inputs, ARRAY_SIZE(vs1_inputs), vs1_outputs, ARRAY_SIZE(vs1_outputs)},
        {vs2_source, true,  "vs_4_0", vs2_inputs, ARRAY_SIZE(vs2_inputs), vs2_outputs, ARRAY_SIZE(vs2_outputs)},
        {vs3_source, false, "vs_1_1", vs3_inputs, ARRAY_SIZE(vs3_inputs), vs3_outputs, ARRAY_SIZE(vs3_outputs)},
        {vs3_source, false, "vs_2_0", vs3_inputs, ARRAY_SIZE(vs3_inputs), vs3_outputs, ARRAY_SIZE(vs3_outputs)},
        {vs4_source, false, "vs_3_0", vs4_inputs, ARRAY_SIZE(vs4_inputs), vs4_outputs, ARRAY_SIZE(vs4_outputs)},
        {ps1_source, true,  "ps_4_0", ps1_inputs, ARRAY_SIZE(ps1_inputs), ps1_outputs, ARRAY_SIZE(ps1_outputs)},
        {ps2_source, false, "ps_1_1", ps2_inputs, ARRAY_SIZE(ps2_inputs), ps2_outputs, ARRAY_SIZE(ps2_outputs)},
        {ps3_source, false, "ps_2_0", ps3_inputs, ARRAY_SIZE(ps3_inputs), ps3_outputs, ARRAY_SIZE(ps3_outputs)},
        {ps4_source, false, "ps_3_0", ps4_inputs, ARRAY_SIZE(ps4_inputs), ps3_outputs, ARRAY_SIZE(ps3_outputs)},
        {cs1_source, true,  "cs_5_0", NULL, 0, NULL, 0},
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
}
