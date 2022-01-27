/*
 * Copyright 2008 Henri Verbeet for CodeWeavers
 * Copyright 2015 Józef Kucia for CodeWeavers
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

#define COBJMACROS
#define CONST_VTABLE
#define VKD3D_TEST_NO_DEFS
#include <d3d11_4.h>
#define __vkd3d_d3dcommon_h__
#define __vkd3d_dxgibase_h__
#define __vkd3d_dxgiformat_h__
#include "vkd3d_d3dcompiler.h"
#include "shader_runner.h"
#include "vkd3d_test.h"

const GUID IID_IDXGIDevice = {0x54ec77fa, 0x1377, 0x44e6, {0x8c, 0x32, 0x88, 0xfd, 0x5f, 0x44, 0xc8, 0x4c}};

static HRESULT (WINAPI *pCreateDXGIFactory1)(REFIID iid, void **factory);

static HRESULT (WINAPI *pD3D11CreateDevice)(IDXGIAdapter *adapter, D3D_DRIVER_TYPE driver_type,
        HMODULE swrast, UINT flags, const D3D_FEATURE_LEVEL *feature_levels, UINT levels,
        UINT sdk_version, ID3D11Device **device_out, D3D_FEATURE_LEVEL *obtained_feature_level,
        ID3D11DeviceContext **immediate_context);

struct d3d11_texture
{
    struct texture t;

    ID3D11Texture2D *texture;
    ID3D11ShaderResourceView *srv;
};

static struct d3d11_texture *d3d11_texture(struct texture *t)
{
    return CONTAINING_RECORD(t, struct d3d11_texture, t);
}

struct d3d11_shader_context
{
    struct shader_context c;

    ID3D11Device *device;
    HWND window;
    IDXGISwapChain *swapchain;
    ID3D11Texture2D *rt;
    ID3D11RenderTargetView *rtv;
    ID3D11DeviceContext *immediate_context;

    ID3D11InputLayout *input_layout;
    ID3D11VertexShader *vs;
    ID3D11Buffer *vb;
};

static struct d3d11_shader_context *d3d11_shader_context(struct shader_context *c)
{
    return CONTAINING_RECORD(c, struct d3d11_shader_context, c);
}

static bool enable_debug_layer;
static bool use_warp_adapter;
static unsigned int use_adapter_idx;

static ID3D10Blob *compile_shader(const char *source, const char *profile)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    HRESULT hr;

    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", profile, 0, 0, &blob, &errors);
    ok(hr == S_OK, "Failed to compile shader, hr %#lx.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
}

static void parse_args(int argc, char **argv)
{
    unsigned int i;

    for (i = 1; i < argc; ++i)
    {
        if (!strcmp(argv[i], "--warp"))
            use_warp_adapter = true;
        else if (!strcmp(argv[i], "--adapter") && i + 1 < argc)
            use_adapter_idx = atoi(argv[++i]);
    }
}

static void enable_d3d11_debug_layer(int argc, char **argv)
{
    unsigned int i;

    for (i = 1; i < argc; ++i)
    {
        if (!strcmp(argv[i], "--validate"))
            enable_debug_layer = true;
    }
}

static IDXGIAdapter *create_adapter(void)
{
    IDXGIFactory4 *factory4;
    IDXGIFactory *factory;
    IDXGIAdapter *adapter;
    HRESULT hr;

    if (!pCreateDXGIFactory1)
    {
        trace("CreateDXGIFactory1() is not available.\n");
        return NULL;
    }

    if (FAILED(hr = pCreateDXGIFactory1(&IID_IDXGIFactory, (void **)&factory)))
    {
        trace("Failed to create IDXGIFactory, hr %#lx.\n", hr);
        return NULL;
    }

    adapter = NULL;
    if (use_warp_adapter)
    {
        if (SUCCEEDED(hr = IDXGIFactory_QueryInterface(factory, &IID_IDXGIFactory4, (void **)&factory4)))
        {
            hr = IDXGIFactory4_EnumWarpAdapter(factory4, &IID_IDXGIAdapter, (void **)&adapter);
            IDXGIFactory4_Release(factory4);
        }
        else
        {
            trace("Failed to get IDXGIFactory4, hr %#lx.\n", hr);
        }
    }
    else
    {
        hr = IDXGIFactory_EnumAdapters(factory, use_adapter_idx, &adapter);
    }
    IDXGIFactory_Release(factory);
    if (FAILED(hr))
        trace("Failed to get adapter, hr %#lx.\n", hr);
    return adapter;
}

static void init_adapter_info(void)
{
    char name[MEMBER_SIZE(DXGI_ADAPTER_DESC, Description)];
    IDXGIAdapter *adapter;
    DXGI_ADAPTER_DESC desc;
    unsigned int i;
    HRESULT hr;

    if (!(adapter = create_adapter()))
        return;

    hr = IDXGIAdapter_GetDesc(adapter, &desc);
    ok(hr == S_OK, "Failed to get adapter desc, hr %#lx.\n", hr);

    /* FIXME: Use debugstr_w(). */
    for (i = 0; i < ARRAY_SIZE(desc.Description) && isprint(desc.Description[i]); ++i)
        name[i] = desc.Description[i];
    name[min(i, ARRAY_SIZE(name) - 1)] = '\0';

    trace("Adapter: %s, %04x:%04x.\n", name, desc.VendorId, desc.DeviceId);

    if (desc.VendorId == 0x1414 && desc.DeviceId == 0x008c)
    {
        trace("Using WARP device.\n");
        use_warp_adapter = true;
    }

    IDXGIAdapter_Release(adapter);
}

static ID3D11Device *create_device(void)
{
    static const D3D_FEATURE_LEVEL feature_level[] =
    {
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
    };
    IDXGIAdapter *adapter;
    ID3D11Device *device;
    UINT flags = 0;
    HRESULT hr;

    if (enable_debug_layer)
        flags |= D3D11_CREATE_DEVICE_DEBUG;

    if ((adapter = create_adapter()))
    {
        hr = pD3D11CreateDevice(adapter, D3D_DRIVER_TYPE_UNKNOWN, NULL, flags,
                feature_level, ARRAY_SIZE(feature_level), D3D11_SDK_VERSION, &device, NULL, NULL);
        IDXGIAdapter_Release(adapter);
        return SUCCEEDED(hr) ? device : NULL;
    }

    if (SUCCEEDED(pD3D11CreateDevice(NULL, D3D_DRIVER_TYPE_HARDWARE, NULL, flags,
            feature_level, ARRAY_SIZE(feature_level), D3D11_SDK_VERSION, &device, NULL, NULL)))
        return device;
    if (SUCCEEDED(pD3D11CreateDevice(NULL, D3D_DRIVER_TYPE_WARP, NULL, flags,
            feature_level, ARRAY_SIZE(feature_level), D3D11_SDK_VERSION, &device, NULL, NULL)))
        return device;
    if (SUCCEEDED(pD3D11CreateDevice(NULL, D3D_DRIVER_TYPE_REFERENCE, NULL, flags,
            feature_level, ARRAY_SIZE(feature_level), D3D11_SDK_VERSION, &device, NULL, NULL)))
        return device;

    return NULL;
}

static IDXGISwapChain *create_swapchain(ID3D11Device *device, HWND window)
{
    DXGI_SWAP_CHAIN_DESC dxgi_desc;
    IDXGISwapChain *swapchain;
    IDXGIDevice *dxgi_device;
    IDXGIAdapter *adapter;
    IDXGIFactory *factory;
    HRESULT hr;

    hr = ID3D11Device_QueryInterface(device, &IID_IDXGIDevice, (void **)&dxgi_device);
    ok(hr == S_OK, "Failed to get DXGI device, hr %#lx.\n", hr);
    hr = IDXGIDevice_GetAdapter(dxgi_device, &adapter);
    ok(hr == S_OK, "Failed to get adapter, hr %#lx.\n", hr);
    IDXGIDevice_Release(dxgi_device);
    hr = IDXGIAdapter_GetParent(adapter, &IID_IDXGIFactory, (void **)&factory);
    ok(hr == S_OK, "Failed to get factory, hr %#lx.\n", hr);
    IDXGIAdapter_Release(adapter);

    dxgi_desc.BufferDesc.Width = 640;
    dxgi_desc.BufferDesc.Height = 480;
    dxgi_desc.BufferDesc.RefreshRate.Numerator = 60;
    dxgi_desc.BufferDesc.RefreshRate.Denominator = 1;
    dxgi_desc.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    dxgi_desc.BufferDesc.ScanlineOrdering = DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED;
    dxgi_desc.BufferDesc.Scaling = DXGI_MODE_SCALING_UNSPECIFIED;
    dxgi_desc.SampleDesc.Count = 1;
    dxgi_desc.SampleDesc.Quality = 0;
    dxgi_desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    dxgi_desc.BufferCount = 1;
    dxgi_desc.OutputWindow = window;
    dxgi_desc.Windowed = TRUE;
    dxgi_desc.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
    dxgi_desc.Flags = 0;

    hr = IDXGIFactory_CreateSwapChain(factory, (IUnknown *)device, &dxgi_desc, &swapchain);
    ok(hr == S_OK, "Failed to create swapchain, hr %#lx.\n", hr);
    IDXGIFactory_Release(factory);

    return swapchain;
}

static BOOL init_test_context(struct d3d11_shader_context *context)
{
    const D3D11_TEXTURE2D_DESC texture_desc =
    {
        .Width = 640,
        .Height = 480,
        .MipLevels = 1,
        .ArraySize = 1,
        .Format = DXGI_FORMAT_R32G32B32A32_FLOAT,
        .SampleDesc.Count = 1,
        .Usage = D3D11_USAGE_DEFAULT,
        .BindFlags = D3D11_BIND_RENDER_TARGET,
    };
    unsigned int rt_width, rt_height;
    D3D11_VIEWPORT vp;
    HRESULT hr;
    RECT rect;

    memset(context, 0, sizeof(*context));

    if (!(context->device = create_device()))
    {
        skip("Failed to create device.\n");
        return FALSE;
    }

    rt_width = 640;
    rt_height = 480;
    SetRect(&rect, 0, 0, rt_width, rt_height);
    AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, FALSE);
    context->window = CreateWindowA("static", "d3dcompiler_test", WS_OVERLAPPEDWINDOW,
            0, 0, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, NULL, NULL);
    context->swapchain = create_swapchain(context->device, context->window);

    hr = ID3D11Device_CreateTexture2D(context->device, &texture_desc, NULL, &context->rt);
    ok(hr == S_OK, "Failed to create texture, hr %#lx.\n", hr);

    hr = ID3D11Device_CreateRenderTargetView(context->device, (ID3D11Resource *)context->rt, NULL, &context->rtv);
    ok(hr == S_OK, "Failed to create rendertarget view, hr %#lx.\n", hr);

    ID3D11Device_GetImmediateContext(context->device, &context->immediate_context);

    ID3D11DeviceContext_OMSetRenderTargets(context->immediate_context, 1, &context->rtv, NULL);

    vp.TopLeftX = 0.0f;
    vp.TopLeftY = 0.0f;
    vp.Width = rt_width;
    vp.Height = rt_height;
    vp.MinDepth = 0.0f;
    vp.MaxDepth = 1.0f;
    ID3D11DeviceContext_RSSetViewports(context->immediate_context, 1, &vp);

    return TRUE;
}

static void destroy_test_context(struct d3d11_shader_context *context)
{
    ULONG ref;

    if (context->input_layout)
        ID3D11InputLayout_Release(context->input_layout);
    if (context->vs)
        ID3D11VertexShader_Release(context->vs);
    if (context->vb)
        ID3D11Buffer_Release(context->vb);

    ID3D11DeviceContext_Release(context->immediate_context);
    ID3D11RenderTargetView_Release(context->rtv);
    ID3D11Texture2D_Release(context->rt);
    IDXGISwapChain_Release(context->swapchain);
    DestroyWindow(context->window);

    ref = ID3D11Device_Release(context->device);
    ok(!ref, "Device has %lu references left.\n", ref);
}

static ID3D11Buffer *create_buffer(ID3D11Device *device, unsigned int bind_flags, unsigned int size, const void *data)
{
    D3D11_SUBRESOURCE_DATA resource_data;
    D3D11_BUFFER_DESC buffer_desc;
    ID3D11Buffer *buffer;
    HRESULT hr;

    buffer_desc.ByteWidth = size;
    buffer_desc.Usage = D3D11_USAGE_DEFAULT;
    buffer_desc.BindFlags = bind_flags;
    buffer_desc.CPUAccessFlags = 0;
    buffer_desc.MiscFlags = 0;
    buffer_desc.StructureByteStride = 0;

    resource_data.pSysMem = data;
    resource_data.SysMemPitch = 0;
    resource_data.SysMemSlicePitch = 0;

    hr = ID3D11Device_CreateBuffer(device, &buffer_desc, data ? &resource_data : NULL, &buffer);
    ok(hr == S_OK, "Failed to create buffer, hr %#lx.\n", hr);
    return buffer;
}

static struct texture *d3d11_runner_create_texture(struct shader_context *c, const struct texture_params *params)
{
    struct d3d11_shader_context *context = d3d11_shader_context(c);
    ID3D11Device *device = context->device;
    D3D11_SUBRESOURCE_DATA resource_data;
    D3D11_TEXTURE2D_DESC desc = {0};
    struct d3d11_texture *texture;
    HRESULT hr;

    texture = calloc(1, sizeof(*texture));

    texture->t.slot = params->slot;

    desc.Width = params->width;
    desc.Height = params->height;
    desc.MipLevels = 1;
    desc.ArraySize = 1;
    desc.Format = params->format;
    desc.SampleDesc.Count = 1;
    desc.Usage = D3D11_USAGE_DEFAULT;
    desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

    resource_data.pSysMem = params->data;
    resource_data.SysMemPitch = params->width * params->texel_size;
    resource_data.SysMemSlicePitch = params->height * resource_data.SysMemPitch;

    hr = ID3D11Device_CreateTexture2D(device, &desc, &resource_data, &texture->texture);
    ok(hr == S_OK, "Failed to create texture, hr %#lx.\n", hr);
    hr = ID3D11Device_CreateShaderResourceView(device,
            (ID3D11Resource *)texture->texture, NULL, &texture->srv);
    ok(hr == S_OK, "Failed to create shader resource view, hr %#lx.\n", hr);

    return &texture->t;
}

static void d3d11_runner_destroy_texture(struct shader_context *c, struct texture *t)
{
    struct d3d11_texture *texture = d3d11_texture(t);

    ID3D11Texture2D_Release(texture->texture);
    ID3D11ShaderResourceView_Release(texture->srv);
    free(texture);
}

static void d3d11_runner_draw_quad(struct shader_context *c)
{
    static const D3D11_INPUT_ELEMENT_DESC default_layout_desc[] =
    {
        {"POSITION", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 0, D3D11_INPUT_PER_VERTEX_DATA, 0},
    };

    static const char vs_source[] =
        "float4 main(float4 position : POSITION) : sv_position\n"
        "{\n"
        "    return position;\n"
        "}";

    static const struct vec2 quad[] =
    {
        {-1.0f, -1.0f},
        {-1.0f,  1.0f},
        { 1.0f, -1.0f},
        { 1.0f,  1.0f},
    };

    struct d3d11_shader_context *context = d3d11_shader_context(c);
    ID3D11Device *device = context->device;
    unsigned int stride, offset;
    ID3D11Buffer *cb = NULL;
    ID3D11PixelShader *ps;
    ID3D10Blob *ps_code;
    unsigned int i;
    HRESULT hr;

    static const char *const ps_profiles[] =
    {
        [SHADER_MODEL_4_0] = "ps_4_0",
        [SHADER_MODEL_4_1] = "ps_4_1",
        [SHADER_MODEL_5_0] = "ps_5_0",
        [SHADER_MODEL_5_1] = "ps_5_1",
    };

    if (!(ps_code = compile_shader(context->c.ps_source, ps_profiles[context->c.minimum_shader_model])))
        return;

    if (!context->input_layout || !context->vs)
    {
        ID3D10Blob *vs_code = compile_shader(vs_source, "vs_4_0");

        if (!context->input_layout)
        {
            hr = ID3D11Device_CreateInputLayout(device, default_layout_desc, ARRAY_SIZE(default_layout_desc),
                    ID3D10Blob_GetBufferPointer(vs_code), ID3D10Blob_GetBufferSize(vs_code), &context->input_layout);
            ok(hr == S_OK, "Failed to create input layout, hr %#lx.\n", hr);
        }

        if (!context->vs)
        {
            hr = ID3D11Device_CreateVertexShader(device, ID3D10Blob_GetBufferPointer(vs_code),
                    ID3D10Blob_GetBufferSize(vs_code), NULL, &context->vs);
            ok(hr == S_OK, "Failed to create vertex shader, hr %#lx.\n", hr);
        }

        ID3D10Blob_Release(vs_code);
    }

    if (!context->vb)
        context->vb = create_buffer(device, D3D11_BIND_VERTEX_BUFFER, sizeof(quad), quad);

    hr = ID3D11Device_CreatePixelShader(device, ID3D10Blob_GetBufferPointer(ps_code),
            ID3D10Blob_GetBufferSize(ps_code), NULL, &ps);
    ok(hr == S_OK, "Failed to create pixel shader, hr %#lx.\n", hr);

    if (context->c.uniform_count)
    {
        cb = create_buffer(device, D3D11_BIND_CONSTANT_BUFFER,
                context->c.uniform_count * sizeof(*context->c.uniforms), context->c.uniforms);
        ID3D11DeviceContext_PSSetConstantBuffers(context->immediate_context, 0, 1, &cb);
    }

    for (i = 0; i < context->c.texture_count; ++i)
    {
        struct d3d11_texture *texture = d3d11_texture(context->c.textures[i]);

        ID3D11DeviceContext_PSSetShaderResources(context->immediate_context, texture->t.slot, 1, &texture->srv);
    }

    for (i = 0; i < context->c.sampler_count; ++i)
    {
        struct sampler *sampler = &context->c.samplers[i];
        ID3D11SamplerState *d3d11_sampler;
        D3D11_SAMPLER_DESC desc = {0};

        /* Members of D3D11_FILTER are compatible with D3D12_FILTER. */
        desc.Filter = (D3D11_FILTER)sampler->filter;
        /* Members of D3D11_TEXTURE_ADDRESS_MODE are compatible with D3D12_TEXTURE_ADDRESS_MODE. */
        desc.AddressU = (D3D11_TEXTURE_ADDRESS_MODE)sampler->u_address;
        desc.AddressV = (D3D11_TEXTURE_ADDRESS_MODE)sampler->v_address;
        desc.AddressW = (D3D11_TEXTURE_ADDRESS_MODE)sampler->w_address;
        desc.ComparisonFunc = D3D11_COMPARISON_NEVER;
        desc.MaxLOD = D3D11_FLOAT32_MAX;

        hr = ID3D11Device_CreateSamplerState(device, &desc, &d3d11_sampler);
        ok(hr == S_OK, "Failed to create sampler state, hr %#lx.\n", hr);
        ID3D11DeviceContext_PSSetSamplers(context->immediate_context, sampler->slot, 1, &d3d11_sampler);
        ID3D11SamplerState_Release(d3d11_sampler);
    }

    ID3D11DeviceContext_IASetInputLayout(context->immediate_context, context->input_layout);
    ID3D11DeviceContext_IASetPrimitiveTopology(context->immediate_context, D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
    stride = sizeof(*quad);
    offset = 0;
    ID3D11DeviceContext_IASetVertexBuffers(context->immediate_context, 0, 1, &context->vb, &stride, &offset);
    ID3D11DeviceContext_VSSetShader(context->immediate_context, context->vs, NULL, 0);
    ID3D11DeviceContext_PSSetShader(context->immediate_context, ps, NULL, 0);

    ID3D11DeviceContext_Draw(context->immediate_context, 4, 0);

    ID3D11PixelShader_Release(ps);
    if (cb)
        ID3D11Buffer_Release(cb);
}

struct resource_readback
{
    uint64_t width;
    unsigned int height;
    ID3D11Resource *resource;
    D3D11_MAPPED_SUBRESOURCE map_desc;
};

static void init_readback(struct d3d11_shader_context *context, struct resource_readback *rb)
{
    D3D11_TEXTURE2D_DESC texture_desc;
    HRESULT hr;

    ID3D11Texture2D_GetDesc(context->rt, &texture_desc);
    texture_desc.Usage = D3D11_USAGE_STAGING;
    texture_desc.BindFlags = 0;
    texture_desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
    texture_desc.MiscFlags = 0;
    hr = ID3D11Device_CreateTexture2D(context->device, &texture_desc, NULL, (ID3D11Texture2D **)&rb->resource);
    ok(hr == S_OK, "Failed to create texture, hr %#lx.\n", hr);

    ID3D11DeviceContext_CopyResource(context->immediate_context, rb->resource, (ID3D11Resource *)context->rt);
    hr = ID3D11DeviceContext_Map(context->immediate_context, rb->resource, 0, D3D11_MAP_READ, 0, &rb->map_desc);
    ok(hr == S_OK, "Failed to map texture, hr %#lx.\n", hr);

    rb->width = texture_desc.Width;
    rb->height = texture_desc.Height;
}

static void release_readback(struct d3d11_shader_context *context, struct resource_readback *rb)
{
    ID3D11DeviceContext_Unmap(context->immediate_context, rb->resource, 0);
    ID3D11Resource_Release(rb->resource);
}

static const struct vec4 *get_readback_vec4(struct resource_readback *rb, unsigned int x, unsigned int y)
{
    return (struct vec4 *)((BYTE *)rb->map_desc.pData + y * rb->map_desc.RowPitch) + x;
}

static void check_readback_data_vec4(struct resource_readback *rb,
        const RECT *rect, const struct vec4 *expected, unsigned int max_diff)
{
    RECT r = {0, 0, rb->width, rb->height};
    unsigned int x = 0, y = 0;
    struct vec4 got = {0};
    bool all_match = true;

    if (rect)
        r = *rect;

    for (y = r.top; y < r.bottom; ++y)
    {
        for (x = r.left; x < r.right; ++x)
        {
            got = *get_readback_vec4(rb, x, y);
            if (!compare_vec4(&got, expected, max_diff))
            {
                all_match = false;
                break;
            }
        }
        if (!all_match)
            break;
    }
    ok(all_match, "Got {%.8e, %.8e, %.8e, %.8e}, expected {%.8e, %.8e, %.8e, %.8e} at (%u, %u).\n",
            got.x, got.y, got.z, got.w, expected->x, expected->y, expected->z, expected->w, x, y);
}

static void d3d11_runner_probe_vec4(struct shader_context *c, const RECT *rect, const struct vec4 *v, unsigned int ulps)
{
    struct d3d11_shader_context *context = d3d11_shader_context(c);
    struct resource_readback rb;

    init_readback(context, &rb);
    check_readback_data_vec4(&rb, rect, v, ulps);
    release_readback(context, &rb);
}

static const struct shader_runner_ops d3d11_runner_ops =
{
    .create_texture = d3d11_runner_create_texture,
    .destroy_texture = d3d11_runner_destroy_texture,
    .draw_quad = d3d11_runner_draw_quad,
    .probe_vec4 = d3d11_runner_probe_vec4,
};

void run_shader_tests_d3d11(int argc, char **argv)
{
    HMODULE dxgi_module, d3d11_module;
    struct d3d11_shader_context context;

    d3d11_module = LoadLibraryA("d3d11.dll");
    dxgi_module = LoadLibraryA("dxgi.dll");
    if (d3d11_module && dxgi_module)
    {
        pCreateDXGIFactory1 = (void *)GetProcAddress(dxgi_module, "CreateDXGIFactory1");
        pD3D11CreateDevice = (void *)GetProcAddress(d3d11_module, "D3D11CreateDevice");

        parse_args(argc, argv);
        enable_d3d11_debug_layer(argc, argv);
        init_adapter_info();
        init_test_context(&context);
        run_shader_tests(&context.c, argc, argv, &d3d11_runner_ops);
        destroy_test_context(&context);
    }
    FreeLibrary(d3d11_module);
    FreeLibrary(dxgi_module);
}
