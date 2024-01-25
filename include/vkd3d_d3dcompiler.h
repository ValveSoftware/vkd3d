/*
 * Copyright 2010 Matteo Bruni for CodeWeavers
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

#ifndef __VKD3D_D3DCOMPILER_H
#define __VKD3D_D3DCOMPILER_H
#ifndef __D3DCOMPILER_H__

#include <vkd3d_d3dcompiler_types.h>

#define D3DCOMPILE_DEBUG                                0x00000001
#define D3DCOMPILE_SKIP_VALIDATION                      0x00000002
#define D3DCOMPILE_SKIP_OPTIMIZATION                    0x00000004
#define D3DCOMPILE_PACK_MATRIX_ROW_MAJOR                0x00000008
#define D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR             0x00000010
#define D3DCOMPILE_PARTIAL_PRECISION                    0x00000020
#define D3DCOMPILE_FORCE_VS_SOFTWARE_NO_OPT             0x00000040
#define D3DCOMPILE_FORCE_PS_SOFTWARE_NO_OPT             0x00000080
#define D3DCOMPILE_NO_PRESHADER                         0x00000100
#define D3DCOMPILE_AVOID_FLOW_CONTROL                   0x00000200
#define D3DCOMPILE_PREFER_FLOW_CONTROL                  0x00000400
#define D3DCOMPILE_ENABLE_STRICTNESS                    0x00000800
#define D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY       0x00001000
#define D3DCOMPILE_IEEE_STRICTNESS                      0x00002000
#define D3DCOMPILE_OPTIMIZATION_LEVEL0                  0x00004000
#define D3DCOMPILE_OPTIMIZATION_LEVEL1                  0x00000000
#define D3DCOMPILE_OPTIMIZATION_LEVEL2                  0x0000c000
#define D3DCOMPILE_OPTIMIZATION_LEVEL3                  0x00008000
#define D3DCOMPILE_RESERVED16                           0x00010000
#define D3DCOMPILE_RESERVED17                           0x00020000
#define D3DCOMPILE_WARNINGS_ARE_ERRORS                  0x00040000
#define D3DCOMPILE_RESOURCES_MAY_ALIAS                  0x00080000
#define D3DCOMPILE_ENABLE_UNBOUNDED_DESCRIPTOR_TABLES   0x00100000
#define D3DCOMPILE_ALL_RESOURCES_BOUND                  0x00200000
#define D3DCOMPILE_DEBUG_NAME_FOR_SOURCE                0x00400000
#define D3DCOMPILE_DEBUG_NAME_FOR_BINARY                0x00800000

#define D3DCOMPILE_EFFECT_CHILD_EFFECT                  0x00000001
#define D3DCOMPILE_EFFECT_ALLOW_SLOW_OPS                0x00000002

#define D3DCOMPILE_FLAGS2_FORCE_ROOT_SIGNATURE_LATEST   0x00000000
#define D3DCOMPILE_FLAGS2_FORCE_ROOT_SIGNATURE_1_0      0x00000010
#define D3DCOMPILE_FLAGS2_FORCE_ROOT_SIGNATURE_1_1      0x00000020

#define D3DCOMPILE_SECDATA_MERGE_UAV_SLOTS              0x00000001
#define D3DCOMPILE_SECDATA_PRESERVE_TEMPLATE_SLOTS      0x00000002
#define D3DCOMPILE_SECDATA_REQUIRE_TEMPLATE_MATCH       0x00000004

typedef enum D3DCOMPILER_STRIP_FLAGS
{
    D3DCOMPILER_STRIP_REFLECTION_DATA = 0x00000001,
    D3DCOMPILER_STRIP_DEBUG_INFO      = 0x00000002,
    D3DCOMPILER_STRIP_TEST_BLOBS      = 0x00000004,
    D3DCOMPILER_STRIP_PRIVATE_DATA    = 0x00000008,
    D3DCOMPILER_STRIP_ROOT_SIGNATURE  = 0x00000010,
    D3DCOMPILER_STRIP_FORCE_DWORD     = 0x7fffffff,
} D3DCOMPILER_STRIP_FLAGS;

HRESULT WINAPI D3DCompile(const void *data, SIZE_T data_size, const char *filename,
        const D3D_SHADER_MACRO *macros, ID3DInclude *include, const char *entrypoint,
        const char *profile, UINT flags, UINT effect_flags, ID3DBlob **shader, ID3DBlob **error_messages);
HRESULT WINAPI D3DCompile2(const void *data, SIZE_T data_size, const char *filename,
        const D3D_SHADER_MACRO *macros, ID3DInclude *include, const char *entrypoint,
        const char *profile, UINT flags, UINT effect_flags, UINT secondary_flags,
        const void *secondary_data, SIZE_T secondary_data_size, ID3DBlob **shader,
        ID3DBlob **error_messages);
HRESULT WINAPI D3DCreateBlob(SIZE_T size, ID3DBlob **blob);
HRESULT WINAPI D3DDisassemble(const void *data, SIZE_T data_size, UINT flags, const char *comments, ID3DBlob **blob);
HRESULT WINAPI D3DGetBlobPart(const void *data, SIZE_T data_size, D3D_BLOB_PART part, UINT flags, ID3DBlob **blob);
HRESULT WINAPI D3DGetDebugInfo(const void *data, SIZE_T data_size, ID3DBlob **blob);
HRESULT WINAPI D3DGetInputAndOutputSignatureBlob(const void *data, SIZE_T data_size, ID3DBlob **blob);
HRESULT WINAPI D3DGetInputSignatureBlob(const void *data, SIZE_T data_size, ID3DBlob **blob);
HRESULT WINAPI D3DGetOutputSignatureBlob(const void *data, SIZE_T data_size, ID3DBlob **blob);
HRESULT WINAPI D3DPreprocess(const void *data, SIZE_T size, const char *filename, const D3D_SHADER_MACRO *macros,
        ID3DInclude *include, ID3DBlob **shader, ID3DBlob **error_messages);
HRESULT WINAPI D3DReflect(const void *data, SIZE_T data_size, REFIID iid, void **reflection);
HRESULT WINAPI D3DStripShader(const void *data, SIZE_T data_size, UINT flags, ID3DBlob **blob);

typedef HRESULT (WINAPI *pD3DDisassemble)(const void *data, SIZE_T data_size,
        UINT flags, const char *comments, ID3DBlob **blob);

#endif /* __D3DCOMPILER_H__ */
#endif /* __VKD3D_D3DCOMPILER_H */
