/*
 * Copyright 2021 Zebediah Figura for CodeWeavers
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

#ifdef _WIN32

#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
#include <d3d9.h>
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3dcompiler.h"
#include "shader_runner.h"
#include "vkd3d_test.h"

struct d3d9_resource
{
    struct resource r;

    IDirect3DSurface9 *surface;
    IDirect3DTexture9 *texture;
    IDirect3DVertexBuffer9 *vb;
};

static struct d3d9_resource *d3d9_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct d3d9_resource, r);
}

struct d3d9_shader_runner
{
    struct shader_runner r;

    IDirect3DDevice9 *device;
    HWND window;
};

static struct d3d9_shader_runner *d3d9_shader_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct d3d9_shader_runner, r);
}

static IDirect3D9 *(WINAPI *pDirect3DCreate9)(UINT sdk_version);

static ID3D10Blob *compile_shader(const char *source, const char *type, enum shader_model shader_model)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    char profile[7];
    UINT flags = 0;
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "2_0",
        [SHADER_MODEL_3_0] = "3_0",
    };

    sprintf(profile, "%s_%s", type, shader_models[shader_model]);
    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", profile, flags, 0, &blob, &errors);
    ok(hr == S_OK, "Failed to compile shader, hr %#lx.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

static void init_adapter_info(void)
{
    D3DADAPTER_IDENTIFIER9 identifier;
    IDirect3D9 *d3d;
    HRESULT hr;

    d3d = pDirect3DCreate9(D3D_SDK_VERSION);
    ok(!!d3d, "Failed to create a D3D object.\n");

    hr = IDirect3D9_GetAdapterIdentifier(d3d, test_options.adapter_idx, 0, &identifier);
    ok(hr == S_OK, "Failed to get adapter identifier, hr %#lx.\n", hr);

    trace("Driver string: %s.\n", identifier.Driver);
    trace("Device: %s, %04lx:%04lx.\n", identifier.Description, identifier.VendorId, identifier.DeviceId);

    if (identifier.VendorId == 0x1414 && identifier.DeviceId == 0x008c)
        trace("Using WARP device.\n");

    IDirect3D9_Release(d3d);
}

static bool init_test_context(struct d3d9_shader_runner *runner)
{
    D3DPRESENT_PARAMETERS present_parameters =
    {
        .Windowed = TRUE,
        .SwapEffect = D3DSWAPEFFECT_DISCARD,
        .BackBufferWidth = RENDER_TARGET_WIDTH,
        .BackBufferHeight = RENDER_TARGET_HEIGHT,
        .BackBufferFormat = D3DFMT_A8R8G8B8,
    };
    RECT rect = {0, 0, RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT};
    IDirect3D9 *d3d;
    D3DCAPS9 caps;
    HRESULT hr;

    memset(runner, 0, sizeof(*runner));

    AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, FALSE);
    runner->window = CreateWindowA("static", "d3dcompiler_test", WS_OVERLAPPEDWINDOW,
            0, 0, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, NULL, NULL);
    ok(!!runner->window, "Failed to create a window.\n");

    d3d = pDirect3DCreate9(D3D_SDK_VERSION);
    ok(!!d3d, "Failed to create a D3D object.\n");

    present_parameters.hDeviceWindow = runner->window;

    hr = IDirect3D9_CreateDevice(d3d, D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, runner->window,
            D3DCREATE_HARDWARE_VERTEXPROCESSING, &present_parameters, &runner->device);
    IDirect3D9_Release(d3d);
    if (FAILED(hr))
    {
        skip("Failed to create a 3D device, hr %#lx.\n", hr);
        DestroyWindow(runner->window);
        return false;
    }

    hr = IDirect3DDevice9_GetDeviceCaps(runner->device, &caps);
    ok(hr == D3D_OK, "Failed to get device caps, hr %#lx.\n", hr);
    if (caps.PixelShaderVersion < D3DPS_VERSION(2, 0) || caps.VertexShaderVersion < D3DVS_VERSION(2, 0))
    {
        skip("No shader model 2 support.\n");
        IDirect3DDevice9_Release(runner->device);
        DestroyWindow(runner->window);
        return false;
    }

    return true;
}

static void destroy_test_context(struct d3d9_shader_runner *runner)
{
    ULONG ref;

    ref = IDirect3DDevice9_Release(runner->device);
    ok(!ref, "Device has %lu references left.\n", ref);
    DestroyWindow(runner->window);
}

static D3DTEXTUREADDRESS sampler_address_to_d3d9(D3D12_TEXTURE_ADDRESS_MODE address)
{
    switch (address)
    {
        case D3D12_TEXTURE_ADDRESS_MODE_WRAP:
            return D3DTADDRESS_WRAP;

        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR:
            return D3DTADDRESS_MIRROR;

        case D3D12_TEXTURE_ADDRESS_MODE_CLAMP:
            return D3DTADDRESS_CLAMP;

        case D3D12_TEXTURE_ADDRESS_MODE_BORDER:
            return D3DTADDRESS_BORDER;

        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR_ONCE:
            return D3DTADDRESS_MIRRORONCE;
    }

    vkd3d_unreachable();
}

static bool d3d9_runner_check_requirements(struct shader_runner *r)
{
    struct d3d9_shader_runner *runner = d3d9_shader_runner(r);

    if (runner->r.minimum_shader_model >= SHADER_MODEL_4_0)
        return false;

    return true;
}

static struct resource *d3d9_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct d3d9_shader_runner *runner = d3d9_shader_runner(r);
    IDirect3DDevice9 *device = runner->device;
    struct d3d9_resource *resource;
    D3DLOCKED_RECT map_desc;
    D3DFORMAT format;
    HRESULT hr;
    void *data;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->format)
    {
        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            format = D3DFMT_A32B32G32R32F;
            break;

        case DXGI_FORMAT_R32_FLOAT:
            format = D3DFMT_R32F;
            break;

        default:
            format = D3DFMT_UNKNOWN;
            break;
    }

    switch (params->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            hr = IDirect3DDevice9_CreateRenderTarget(device, params->width, params->height,
                    format, D3DMULTISAMPLE_NONE, 0, FALSE, &resource->surface, NULL);
            ok(hr == D3D_OK, "Failed to create render target, hr %#lx.\n", hr);
            break;

        case RESOURCE_TYPE_TEXTURE:
        {
            unsigned int src_buffer_offset = 0;

            hr = IDirect3DDevice9_CreateTexture(device, params->width, params->height,
                    params->level_count, 0, format, D3DPOOL_MANAGED, &resource->texture, NULL);
            ok(hr == D3D_OK, "Failed to create texture, hr %#lx.\n", hr);

            for (unsigned int level = 0; level < params->level_count; ++level)
            {
                unsigned int level_width = get_level_dimension(params->width, level);
                unsigned int level_height = get_level_dimension(params->height, level);
                unsigned int src_row_pitch = level_width * params->texel_size;
                unsigned int src_slice_pitch = level_height * src_row_pitch;

                hr = IDirect3DTexture9_LockRect(resource->texture, level, &map_desc, NULL, 0);
                ok(hr == D3D_OK, "Failed to map texture, hr %#lx.\n", hr);
                for (unsigned int y = 0; y < level_height; ++y)
                    memcpy(&((char *)map_desc.pBits)[y * map_desc.Pitch],
                            &params->data[src_buffer_offset + y * src_row_pitch], src_row_pitch);
                hr = IDirect3DTexture9_UnlockRect(resource->texture, level);
                ok(hr == D3D_OK, "Failed to unmap texture, hr %#lx.\n", hr);

                src_buffer_offset += src_slice_pitch;
            }
            break;
        }

        case RESOURCE_TYPE_UAV:
        case RESOURCE_TYPE_BUFFER_UAV:
            fatal_error("UAVs are not supported.\n");
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            hr = IDirect3DDevice9_CreateVertexBuffer(device, params->data_size,
                    D3DUSAGE_DYNAMIC, 0, D3DPOOL_DEFAULT, &resource->vb, NULL);
            ok(hr == D3D_OK, "Failed to create vertex buffer, hr %#lx.\n", hr);

            hr = IDirect3DVertexBuffer9_Lock(resource->vb, 0, 0, &data, D3DLOCK_DISCARD);
            ok(hr == D3D_OK, "Failed to map texture, hr %#lx.\n", hr);
            memcpy(data, params->data, params->data_size);
            hr = IDirect3DVertexBuffer9_Unlock(resource->vb);
            ok(hr == D3D_OK, "Failed to unmap texture, hr %#lx.\n", hr);
            break;
    }

    return &resource->r;
}

static void d3d9_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct d3d9_resource *resource = d3d9_resource(res);

    if (resource->surface)
        IDirect3DSurface9_Release(resource->surface);
    if (resource->texture)
        IDirect3DTexture9_Release(resource->texture);
    if (resource->vb)
        IDirect3DVertexBuffer9_Release(resource->vb);
    free(resource);
}

static D3DDECLTYPE vertex_decl_type_from_format(DXGI_FORMAT format)
{
    switch (format)
    {
        case DXGI_FORMAT_R32G32_FLOAT:
            return D3DDECLTYPE_FLOAT2;

        case DXGI_FORMAT_R32G32B32A32_FLOAT:
            return D3DDECLTYPE_FLOAT4;

        default:
            fatal_error("Cannot translate format %#x to a d3d9 vertex buffer format.\n", format);
    }
}

static D3DDECLUSAGE vertex_decl_usage_from_name(const char *name)
{
    if (!strcasecmp(name, "position") || !strcasecmp(name, "sv_position"))
        return D3DDECLUSAGE_POSITION;
    if (!strcasecmp(name, "texcoord"))
        return D3DDECLUSAGE_TEXCOORD;
    fatal_error("Cannot translate usage \"%s\" to a d3d9 usage.\n", name);
}

static bool d3d9_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    fatal_error("Compute shaders are not supported.\n");
}

static bool d3d9_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count)
{
    static const D3DVERTEXELEMENT9 decl_element_end = D3DDECL_END();
    struct d3d9_shader_runner *runner = d3d9_shader_runner(r);
    IDirect3DVertexDeclaration9 *vertex_declaration;
    IDirect3DDevice9 *device = runner->device;
    D3DVERTEXELEMENT9 *decl_elements;
    ID3D10Blob *vs_code, *ps_code;
    IDirect3DVertexShader9 *vs;
    IDirect3DPixelShader9 *ps;
    unsigned int i, j;
    HRESULT hr;

    if (!(vs_code = compile_shader(runner->r.vs_source, "vs", runner->r.minimum_shader_model)))
        return false;

    if (!(ps_code = compile_shader(runner->r.ps_source, "ps", runner->r.minimum_shader_model)))
    {
        ID3D10Blob_Release(vs_code);
        return false;
    }

    if (runner->r.uniform_count)
    {
        hr = IDirect3DDevice9_SetPixelShaderConstantF(device, 0,
                (const float *)runner->r.uniforms, runner->r.uniform_count / 4);
        ok(hr == D3D_OK, "Failed to set uniforms, hr %#lx.\n", hr);
    }

    decl_elements = calloc(runner->r.input_element_count + 1, sizeof(*decl_elements));
    for (i = 0; i < runner->r.input_element_count; ++i)
    {
        const struct input_element *src_element = &runner->r.input_elements[i];
        D3DVERTEXELEMENT9 *dst_element = &decl_elements[i];

        dst_element->Stream = src_element->slot;
        dst_element->Type = vertex_decl_type_from_format(src_element->format);
        dst_element->Method = D3DDECLMETHOD_DEFAULT;
        dst_element->Usage = vertex_decl_usage_from_name(src_element->name);
        dst_element->UsageIndex = src_element->index;

        /* The offset will be filled below. */
    }
    decl_elements[runner->r.input_element_count] = decl_element_end;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d9_resource *resource = d3d9_resource(runner->r.resources[i]);
        unsigned int stride = 0;

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                hr = IDirect3DDevice9_SetRenderTarget(device, resource->r.slot, resource->surface);
                ok(hr == D3D_OK, "Failed to set render target, hr %#lx.\n", hr);
                break;

            case RESOURCE_TYPE_TEXTURE:
                hr = IDirect3DDevice9_SetTexture(device, resource->r.slot, (IDirect3DBaseTexture9 *)resource->texture);
                ok(hr == D3D_OK, "Failed to set texture, hr %#lx.\n", hr);
                break;

            case RESOURCE_TYPE_UAV:
            case RESOURCE_TYPE_BUFFER_UAV:
                vkd3d_unreachable();

            case RESOURCE_TYPE_VERTEX_BUFFER:
                for (j = 0; j < runner->r.input_element_count; ++j)
                {
                    if (runner->r.input_elements[j].slot == resource->r.slot)
                    {
                        decl_elements[j].Offset = stride;
                        stride += runner->r.input_elements[j].texel_size;
                    }
                }

                hr = IDirect3DDevice9_SetStreamSource(device, resource->r.slot, resource->vb, 0, stride);
                ok(hr == D3D_OK, "Failed to set vertex buffer, hr %#lx.\n", hr);
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        const struct sampler *sampler = &runner->r.samplers[i];

        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot,
                D3DSAMP_ADDRESSU, sampler_address_to_d3d9(sampler->u_address));
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);
        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot,
                D3DSAMP_ADDRESSV, sampler_address_to_d3d9(sampler->v_address));
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);
        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot,
                D3DSAMP_ADDRESSW, sampler_address_to_d3d9(sampler->w_address));
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);

        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot, D3DSAMP_MINFILTER,
                (sampler->filter & 0x1) ? D3DTEXF_LINEAR : D3DTEXF_POINT);
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);
        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot, D3DSAMP_MAGFILTER,
                (sampler->filter & 0x4) ? D3DTEXF_LINEAR : D3DTEXF_POINT);
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);
        hr = IDirect3DDevice9_SetSamplerState(device, sampler->slot, D3DSAMP_MIPFILTER,
                (sampler->filter & 0x10) ? D3DTEXF_LINEAR : D3DTEXF_POINT);
        ok(hr == D3D_OK, "Failed to set sampler state, hr %#lx.\n", hr);
    }

    hr = IDirect3DDevice9_CreateVertexDeclaration(device, decl_elements, &vertex_declaration);
    ok(hr == D3D_OK, "Failed to create vertex declaration, hr %#lx.\n", hr);
    hr = IDirect3DDevice9_CreateVertexShader(device, ID3D10Blob_GetBufferPointer(vs_code), &vs);
    ok(hr == D3D_OK, "Failed to create vertex shader, hr %#lx.\n", hr);
    hr = IDirect3DDevice9_CreatePixelShader(device, ID3D10Blob_GetBufferPointer(ps_code), &ps);
    ok(hr == D3D_OK, "Failed to create pixel shader, hr %#lx.\n", hr);

    hr = IDirect3DDevice9_SetVertexDeclaration(device, vertex_declaration);
    ok(hr == D3D_OK, "Failed to set vertex declaration, hr %#lx.\n", hr);
    hr = IDirect3DDevice9_SetVertexShader(device, vs);
    ok(hr == D3D_OK, "Failed to set vertex shader, hr %#lx.\n", hr);
    hr = IDirect3DDevice9_SetPixelShader(device, ps);
    ok(hr == D3D_OK, "Failed to set pixel shader, hr %#lx.\n", hr);
    hr = IDirect3DDevice9_SetRenderState(device, D3DRS_CULLMODE, D3DCULL_NONE);
    ok(hr == D3D_OK, "Failed to set cull mode, hr %#lx.\n", hr);

    hr = IDirect3DDevice9_BeginScene(device);
    ok(hr == D3D_OK, "Failed to draw, hr %#lx.\n", hr);

    switch (primitive_topology)
    {
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST:
            hr = IDirect3DDevice9_DrawPrimitive(device, D3DPT_TRIANGLELIST, 0, vertex_count / 3);
            break;

        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP:
            hr = IDirect3DDevice9_DrawPrimitive(device, D3DPT_TRIANGLESTRIP, 0, vertex_count - 2);
            break;

        default:
            fatal_error("Cannot translate topology %#x to a d3d9 topology.\n", primitive_topology);
    }
    ok(hr == D3D_OK, "Failed to draw, hr %#lx.\n", hr);

    hr = IDirect3DDevice9_EndScene(device);
    ok(hr == D3D_OK, "Failed to draw, hr %#lx.\n", hr);

    IDirect3DVertexDeclaration9_Release(vertex_declaration);
    IDirect3DVertexShader9_Release(vs);
    IDirect3DPixelShader9_Release(ps);

    return true;
}

struct d3d9_resource_readback
{
    struct resource_readback rb;
    IDirect3DSurface9 *surface;
};

static struct resource_readback *d3d9_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct d3d9_shader_runner *runner = d3d9_shader_runner(r);
    struct d3d9_resource_readback *rb = malloc(sizeof(*rb));
    struct d3d9_resource *resource = d3d9_resource(res);
    D3DLOCKED_RECT map_desc;
    D3DSURFACE_DESC desc;
    HRESULT hr;

    assert(resource->r.type == RESOURCE_TYPE_RENDER_TARGET);

    hr = IDirect3DSurface9_GetDesc(resource->surface, &desc);
    ok(hr == D3D_OK, "Failed to get surface desc, hr %#lx.\n", hr);
    hr = IDirect3DDevice9Ex_CreateOffscreenPlainSurface(runner->device, desc.Width,
            desc.Height, desc.Format, D3DPOOL_SYSTEMMEM, &rb->surface, NULL);
    ok(hr == D3D_OK, "Failed to create surface, hr %#lx.\n", hr);

    hr = IDirect3DDevice9Ex_GetRenderTargetData(runner->device, resource->surface, rb->surface);
    ok(hr == D3D_OK, "Failed to get render target data, hr %#lx.\n", hr);

    hr = IDirect3DSurface9_LockRect(rb->surface, &map_desc, NULL, D3DLOCK_READONLY);
    ok(hr == D3D_OK, "Failed to lock surface, hr %#lx.\n", hr);

    rb->rb.data = map_desc.pBits;
    rb->rb.row_pitch = map_desc.Pitch;
    rb->rb.width = desc.Width;
    rb->rb.height = desc.Height;
    rb->rb.depth = 1;
    return &rb->rb;
}

static void d3d9_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct d3d9_resource_readback *d3d9_rb = CONTAINING_RECORD(rb, struct d3d9_resource_readback, rb);

    IDirect3DSurface9_UnlockRect(d3d9_rb->surface);
    IDirect3DSurface9_Release(d3d9_rb->surface);
    free(d3d9_rb);
}

static const struct shader_runner_ops d3d9_runner_ops =
{
    .check_requirements = d3d9_runner_check_requirements,
    .create_resource = d3d9_runner_create_resource,
    .destroy_resource = d3d9_runner_destroy_resource,
    .dispatch = d3d9_runner_dispatch,
    .draw = d3d9_runner_draw,
    .get_resource_readback = d3d9_runner_get_resource_readback,
    .release_readback = d3d9_runner_release_readback,
};

void run_shader_tests_d3d9(void)
{
    struct d3d9_shader_runner runner;
    HMODULE d3d9_module;

    d3d9_module = LoadLibraryA("d3d9.dll");
    if (d3d9_module)
    {
        pDirect3DCreate9 = (void *)GetProcAddress(d3d9_module, "Direct3DCreate9");

        init_adapter_info();
        init_test_context(&runner);
        run_shader_tests(&runner.r, &d3d9_runner_ops);
        destroy_test_context(&runner);
    }
    FreeLibrary(d3d9_module);
}

#endif
