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

#ifdef _WIN32

#define COBJMACROS
#define CONST_VTABLE
#define INITGUID
#define VKD3D_TEST_NO_DEFS
#include <d3d11_4.h>
#define __vkd3d_d3dcommon_h__
#define __vkd3d_dxgibase_h__
#define __vkd3d_dxgiformat_h__
#include "vkd3d_d3dcompiler.h"
#include "shader_runner.h"
#include "vkd3d_test.h"

static HRESULT (WINAPI *pCreateDXGIFactory1)(REFIID iid, void **factory);

static HRESULT (WINAPI *pD3D11CreateDevice)(IDXGIAdapter *adapter, D3D_DRIVER_TYPE driver_type,
        HMODULE swrast, UINT flags, const D3D_FEATURE_LEVEL *feature_levels, UINT levels,
        UINT sdk_version, ID3D11Device **device_out, D3D_FEATURE_LEVEL *obtained_feature_level,
        ID3D11DeviceContext **immediate_context);

struct d3d11_resource
{
    struct resource r;

    ID3D11Resource *resource;
    ID3D11Buffer *buffer;
    ID3D11Texture2D *texture;
    ID3D11RenderTargetView *rtv;
    ID3D11ShaderResourceView *srv;
    ID3D11UnorderedAccessView *uav;
};

static struct d3d11_resource *d3d11_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct d3d11_resource, r);
}

struct d3d11_shader_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;

    ID3D11Device *device;
    HWND window;
    IDXGISwapChain *swapchain;
    ID3D11DeviceContext *immediate_context;
    ID3D11RasterizerState *rasterizer_state;
};

static struct d3d11_shader_runner *d3d11_shader_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct d3d11_shader_runner, r);
}

static ID3D10Blob *compile_shader(const struct d3d11_shader_runner *runner, const char *source, const char *type)
{
    ID3D10Blob *blob = NULL, *errors = NULL;
    char profile[7];
    HRESULT hr;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "4_0",
        [SHADER_MODEL_3_0] = "4_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
    };

    sprintf(profile, "%s_%s", type, shader_models[runner->r.minimum_shader_model]);
    hr = D3DCompile(source, strlen(source), NULL, NULL, NULL, "main", profile, runner->r.compile_options, 0, &blob, &errors);
    ok(hr == S_OK, "Failed to compile shader, hr %#lx.\n", hr);
    if (errors)
    {
        if (vkd3d_test_state.debug_level)
            trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }
    return blob;
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
    if (test_options.use_warp_device)
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
        hr = IDXGIFactory_EnumAdapters(factory, test_options.adapter_idx, &adapter);
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
        test_options.use_warp_device = true;
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

    if (test_options.enable_debug_layer)
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

    dxgi_desc.BufferDesc.Width = RENDER_TARGET_WIDTH;
    dxgi_desc.BufferDesc.Height = RENDER_TARGET_HEIGHT;
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

static BOOL init_test_context(struct d3d11_shader_runner *runner)
{
    D3D11_FEATURE_DATA_D3D11_OPTIONS2 options2 = {0};
    D3D11_FEATURE_DATA_DOUBLES doubles = {0};
    unsigned int rt_width, rt_height;
    D3D11_RASTERIZER_DESC rs_desc;
    D3D11_VIEWPORT vp;
    HRESULT hr;
    RECT rect;

    memset(runner, 0, sizeof(*runner));

    if (!(runner->device = create_device()))
    {
        skip("Failed to create device.\n");
        return FALSE;
    }

    runner->caps.runner = "d3d11.dll";
    runner->caps.minimum_shader_model = SHADER_MODEL_4_0;
    runner->caps.maximum_shader_model = SHADER_MODEL_5_1;

    hr = ID3D11Device_CheckFeatureSupport(runner->device, D3D11_FEATURE_DOUBLES,
            &doubles, sizeof(doubles));
    ok(hr == S_OK, "Failed to check double precision feature support, hr %#lx.\n", hr);
    runner->caps.float64 = doubles.DoublePrecisionFloatShaderOps;

    hr = ID3D11Device_CheckFeatureSupport(runner->device,
            D3D11_FEATURE_D3D11_OPTIONS2, &options2, sizeof(options2));
    ok(hr == S_OK, "Got hr %#lx.\n", hr);
    runner->caps.rov = options2.ROVsSupported;

    rt_width = RENDER_TARGET_WIDTH;
    rt_height = RENDER_TARGET_HEIGHT;
    SetRect(&rect, 0, 0, rt_width, rt_height);
    AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, FALSE);
    runner->window = CreateWindowA("static", "d3dcompiler_test", WS_OVERLAPPEDWINDOW,
            0, 0, rect.right - rect.left, rect.bottom - rect.top, NULL, NULL, NULL, NULL);
    runner->swapchain = create_swapchain(runner->device, runner->window);

    ID3D11Device_GetImmediateContext(runner->device, &runner->immediate_context);

    vp.TopLeftX = 0.0f;
    vp.TopLeftY = 0.0f;
    vp.Width = rt_width;
    vp.Height = rt_height;
    vp.MinDepth = 0.0f;
    vp.MaxDepth = 1.0f;
    ID3D11DeviceContext_RSSetViewports(runner->immediate_context, 1, &vp);

    rs_desc.FillMode = D3D11_FILL_SOLID;
    rs_desc.CullMode = D3D11_CULL_NONE;
    rs_desc.FrontCounterClockwise = FALSE;
    rs_desc.DepthBias = 0;
    rs_desc.DepthBiasClamp = 0.0f;
    rs_desc.SlopeScaledDepthBias = 0.0f;
    rs_desc.DepthClipEnable = TRUE;
    rs_desc.ScissorEnable = FALSE;
    rs_desc.MultisampleEnable = FALSE;
    rs_desc.AntialiasedLineEnable = FALSE;
    hr = ID3D11Device_CreateRasterizerState(runner->device, &rs_desc, &runner->rasterizer_state);
    ok(hr == S_OK, "Failed to create rasterizer state.\n");

    return TRUE;
}

static void destroy_test_context(struct d3d11_shader_runner *runner)
{
    ULONG ref;

    ID3D11RasterizerState_Release(runner->rasterizer_state);
    ID3D11DeviceContext_Release(runner->immediate_context);
    IDXGISwapChain_Release(runner->swapchain);
    DestroyWindow(runner->window);

    ref = ID3D11Device_Release(runner->device);
    ok(!ref, "Device has %lu references left.\n", ref);
}

static ID3D11Buffer *create_buffer(ID3D11Device *device, unsigned int bind_flags, unsigned int size,
        unsigned int stride, const void *data)
{
    D3D11_SUBRESOURCE_DATA resource_data;
    D3D11_BUFFER_DESC buffer_desc;
    ID3D11Buffer *buffer;
    HRESULT hr;

    buffer_desc.ByteWidth = size;
    buffer_desc.Usage = D3D11_USAGE_DEFAULT;
    buffer_desc.BindFlags = bind_flags;
    buffer_desc.CPUAccessFlags = 0;
    buffer_desc.MiscFlags = stride ? D3D11_RESOURCE_MISC_BUFFER_STRUCTURED : 0;
    buffer_desc.StructureByteStride = stride;

    resource_data.pSysMem = data;
    resource_data.SysMemPitch = 0;
    resource_data.SysMemSlicePitch = 0;

    hr = ID3D11Device_CreateBuffer(device, &buffer_desc, data ? &resource_data : NULL, &buffer);
    ok(hr == S_OK, "Failed to create buffer, hr %#lx.\n", hr);
    return buffer;
}

static void init_resource_2d(struct d3d11_shader_runner *runner, struct d3d11_resource *resource,
        const struct resource_params *params)
{
    D3D11_SUBRESOURCE_DATA resource_data[2];
    ID3D11Device *device = runner->device;
    D3D11_TEXTURE2D_DESC desc = {0};
    HRESULT hr;

    if (params->level_count > ARRAY_SIZE(resource_data))
        fatal_error("Level count %u is too high.\n", params->level_count);

    desc.Width = params->width;
    desc.Height = params->height;
    desc.MipLevels = params->level_count;
    desc.ArraySize = 1;
    desc.Format = params->format;
    desc.SampleDesc.Count = 1;
    desc.Usage = D3D11_USAGE_DEFAULT;
    if (params->type == RESOURCE_TYPE_UAV)
        desc.BindFlags = D3D11_BIND_UNORDERED_ACCESS;
    else if (params->type == RESOURCE_TYPE_RENDER_TARGET)
        desc.BindFlags = D3D11_BIND_RENDER_TARGET;
    else
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

    if (params->data)
    {
        unsigned int buffer_offset = 0;

        for (unsigned int level = 0; level < params->level_count; ++level)
        {
            unsigned int level_width = get_level_dimension(params->width, level);
            unsigned int level_height = get_level_dimension(params->height, level);

            resource_data[level].pSysMem = &params->data[buffer_offset];
            resource_data[level].SysMemPitch = level_width * params->texel_size;
            resource_data[level].SysMemSlicePitch = level_height * resource_data[level].SysMemPitch;
            buffer_offset += resource_data[level].SysMemSlicePitch;
        }
        hr = ID3D11Device_CreateTexture2D(device, &desc, resource_data, &resource->texture);
    }
    else
    {
        hr = ID3D11Device_CreateTexture2D(device, &desc, NULL, &resource->texture);
    }
    ok(hr == S_OK, "Failed to create texture, hr %#lx.\n", hr);

    resource->resource = (ID3D11Resource *)resource->texture;
    if (params->type == RESOURCE_TYPE_UAV)
        hr = ID3D11Device_CreateUnorderedAccessView(device, resource->resource, NULL, &resource->uav);
    else if (params->type == RESOURCE_TYPE_RENDER_TARGET)
        hr = ID3D11Device_CreateRenderTargetView(device, resource->resource, NULL, &resource->rtv);
    else
        hr = ID3D11Device_CreateShaderResourceView(device, resource->resource, NULL, &resource->srv);
    ok(hr == S_OK, "Failed to create view, hr %#lx.\n", hr);
}

static void init_resource_srv_buffer(struct d3d11_shader_runner *runner, struct d3d11_resource *resource,
        const struct resource_params *params)
{
    D3D11_SHADER_RESOURCE_VIEW_DESC srv_desc;
    ID3D11Device *device = runner->device;
    HRESULT hr;

    resource->buffer = create_buffer(device, D3D11_BIND_SHADER_RESOURCE, params->data_size, params->stride, params->data);
    resource->resource = (ID3D11Resource *)resource->buffer;

    srv_desc.Format = params->format;
    srv_desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFER;
    srv_desc.Buffer.FirstElement = 0;
    srv_desc.Buffer.NumElements = params->data_size / params->texel_size;
    hr = ID3D11Device_CreateShaderResourceView(device, resource->resource, &srv_desc, &resource->srv);
    ok(hr == S_OK, "Failed to create view, hr %#lx.\n", hr);
}

static void init_resource_uav_buffer(struct d3d11_shader_runner *runner, struct d3d11_resource *resource,
        const struct resource_params *params)
{
    D3D11_UNORDERED_ACCESS_VIEW_DESC uav_desc;
    ID3D11Device *device = runner->device;
    HRESULT hr;

    resource->buffer = create_buffer(device, D3D11_BIND_UNORDERED_ACCESS, params->data_size, params->stride, params->data);
    resource->resource = (ID3D11Resource *)resource->buffer;

    uav_desc.Format = params->format;
    uav_desc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
    uav_desc.Buffer.FirstElement = 0;
    uav_desc.Buffer.NumElements = params->data_size / params->texel_size;
    uav_desc.Buffer.Flags = 0;
    hr = ID3D11Device_CreateUnorderedAccessView(device, resource->resource, &uav_desc, &resource->uav);
    ok(hr == S_OK, "Failed to create view, hr %#lx.\n", hr);
}

static struct resource *d3d11_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct d3d11_shader_runner *runner = d3d11_shader_runner(r);
    ID3D11Device *device = runner->device;
    struct d3d11_resource *resource;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
        case RESOURCE_TYPE_TEXTURE:
            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
                init_resource_srv_buffer(runner, resource, params);
            else
                init_resource_2d(runner, resource, params);
            break;

        case RESOURCE_TYPE_UAV:
            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
                init_resource_uav_buffer(runner, resource, params);
            else
                init_resource_2d(runner, resource, params);
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            resource->buffer = create_buffer(device, D3D11_BIND_VERTEX_BUFFER, params->data_size, params->stride, params->data);
            resource->resource = (ID3D11Resource *)resource->buffer;
            break;
    }

    return &resource->r;
}

static void d3d11_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct d3d11_resource *resource = d3d11_resource(res);

    ID3D11Resource_Release(resource->resource);
    if (resource->rtv)
        ID3D11RenderTargetView_Release(resource->rtv);
    if (resource->srv)
        ID3D11ShaderResourceView_Release(resource->srv);
    if (resource->uav)
        ID3D11UnorderedAccessView_Release(resource->uav);
    free(resource);
}

static ID3D11SamplerState *create_sampler(ID3D11Device *device, const struct sampler *sampler)
{
    ID3D11SamplerState *d3d11_sampler;
    D3D11_SAMPLER_DESC desc = {0};
    HRESULT hr;

    /* Members of D3D11_FILTER are compatible with D3D12_FILTER. */
    desc.Filter = (D3D11_FILTER)sampler->filter;
    /* Members of D3D11_TEXTURE_ADDRESS_MODE are compatible with D3D12_TEXTURE_ADDRESS_MODE. */
    desc.AddressU = (D3D11_TEXTURE_ADDRESS_MODE)sampler->u_address;
    desc.AddressV = (D3D11_TEXTURE_ADDRESS_MODE)sampler->v_address;
    desc.AddressW = (D3D11_TEXTURE_ADDRESS_MODE)sampler->w_address;
    desc.ComparisonFunc = sampler->func;
    desc.MaxLOD = D3D11_FLOAT32_MAX;

    hr = ID3D11Device_CreateSamplerState(device, &desc, &d3d11_sampler);
    ok(hr == S_OK, "Failed to create sampler state, hr %#lx.\n", hr);
    return d3d11_sampler;
}

static bool d3d11_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    struct d3d11_shader_runner *runner = d3d11_shader_runner(r);
    ID3D11DeviceContext *context = runner->immediate_context;
    ID3D11Device *device = runner->device;
    ID3D11ComputeShader *cs;
    ID3D10Blob *cs_code;
    HRESULT hr;
    size_t i;

    if (!(cs_code = compile_shader(runner, runner->r.cs_source, "cs")))
        return false;

    hr = ID3D11Device_CreateComputeShader(device, ID3D10Blob_GetBufferPointer(cs_code),
            ID3D10Blob_GetBufferSize(cs_code), NULL, &cs);
    ok(hr == S_OK, "Failed to create compute shader, hr %#lx.\n", hr);

    if (runner->r.uniform_count)
    {
        ID3D11Buffer *cb;

        cb = create_buffer(device, D3D11_BIND_CONSTANT_BUFFER,
                runner->r.uniform_count * sizeof(*runner->r.uniforms), 0, runner->r.uniforms);
        ID3D11DeviceContext_CSSetConstantBuffers(context, 0, 1, &cb);
        ID3D11Buffer_Release(cb);
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d11_resource *resource = d3d11_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_TEXTURE:
                ID3D11DeviceContext_CSSetShaderResources(context, resource->r.slot, 1, &resource->srv);
                break;

            case RESOURCE_TYPE_UAV:
                ID3D11DeviceContext_CSSetUnorderedAccessViews(context, resource->r.slot, 1, &resource->uav, NULL);
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        struct sampler *sampler = &runner->r.samplers[i];
        ID3D11SamplerState *d3d11_sampler;

        d3d11_sampler = create_sampler(device, sampler);
        ID3D11DeviceContext_CSSetSamplers(context, sampler->slot, 1, &d3d11_sampler);
        ID3D11SamplerState_Release(d3d11_sampler);
    }

    ID3D11DeviceContext_CSSetShader(context, cs, NULL, 0);
    ID3D11DeviceContext_Dispatch(context, x, y, z);

    ID3D11ComputeShader_Release(cs);

    return true;
}

static bool d3d11_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count)
{
    ID3D11UnorderedAccessView *uavs[D3D11_PS_CS_UAV_REGISTER_COUNT] = {0};
    ID3D11RenderTargetView *rtvs[D3D11_PS_CS_UAV_REGISTER_COUNT] = {0};
    struct d3d11_shader_runner *runner = d3d11_shader_runner(r);
    ID3D11DeviceContext *context = runner->immediate_context;
    unsigned int min_uav_slot = ARRAY_SIZE(uavs);
    ID3D11Device *device = runner->device;
    ID3D10Blob *vs_code, *ps_code;
    unsigned int rtv_count = 0;
    ID3D11Buffer *cb = NULL;
    ID3D11VertexShader *vs;
    ID3D11PixelShader *ps;
    unsigned int i;
    HRESULT hr;

    if (!(vs_code = compile_shader(runner, runner->r.vs_source, "vs")))
        return false;

    if (!(ps_code = compile_shader(runner, runner->r.ps_source, "ps")))
    {
        ID3D10Blob_Release(vs_code);
        return false;
    }

    hr = ID3D11Device_CreateVertexShader(device, ID3D10Blob_GetBufferPointer(vs_code),
            ID3D10Blob_GetBufferSize(vs_code), NULL, &vs);
    ok(hr == S_OK, "Failed to create vertex shader, hr %#lx.\n", hr);

    hr = ID3D11Device_CreatePixelShader(device, ID3D10Blob_GetBufferPointer(ps_code),
            ID3D10Blob_GetBufferSize(ps_code), NULL, &ps);
    ok(hr == S_OK, "Failed to create pixel shader, hr %#lx.\n", hr);

    if (runner->r.uniform_count)
    {
        cb = create_buffer(device, D3D11_BIND_CONSTANT_BUFFER,
                runner->r.uniform_count * sizeof(*runner->r.uniforms), 0, runner->r.uniforms);
        ID3D11DeviceContext_PSSetConstantBuffers(context, 0, 1, &cb);
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct d3d11_resource *resource = d3d11_resource(runner->r.resources[i]);
        unsigned int stride = get_vb_stride(&runner->r, resource->r.slot);
        unsigned int offset = 0;

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                rtvs[resource->r.slot] = resource->rtv;
                rtv_count = max(rtv_count, resource->r.slot + 1);
                break;

            case RESOURCE_TYPE_TEXTURE:
                ID3D11DeviceContext_PSSetShaderResources(context, resource->r.slot, 1, &resource->srv);
                break;

            case RESOURCE_TYPE_UAV:
                uavs[resource->r.slot] = resource->uav;
                min_uav_slot = min(min_uav_slot, resource->r.slot);
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
                ID3D11DeviceContext_IASetVertexBuffers(context, resource->r.slot, 1,
                        (ID3D11Buffer **)&resource->resource, &stride, &offset);
                break;
        }
    }

    ID3D11DeviceContext_OMSetRenderTargetsAndUnorderedAccessViews(context, rtv_count, rtvs, NULL,
            min_uav_slot, ARRAY_SIZE(uavs) - min_uav_slot, &uavs[min_uav_slot], NULL);

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        struct sampler *sampler = &runner->r.samplers[i];
        ID3D11SamplerState *d3d11_sampler;

        d3d11_sampler = create_sampler(device, sampler);
        ID3D11DeviceContext_PSSetSamplers(context, sampler->slot, 1, &d3d11_sampler);
        ID3D11SamplerState_Release(d3d11_sampler);
    }

    if (runner->r.input_element_count)
    {
        D3D11_INPUT_ELEMENT_DESC *descs;
        ID3D11InputLayout *input_layout;

        descs = calloc(runner->r.input_element_count, sizeof(*descs));
        for (i = 0; i < runner->r.input_element_count; ++i)
        {
            const struct input_element *element = &runner->r.input_elements[i];
            D3D11_INPUT_ELEMENT_DESC *desc = &descs[i];

            desc->SemanticName = element->name;
            desc->SemanticIndex = element->index;
            desc->Format = element->format;
            desc->InputSlot = element->slot;
            desc->AlignedByteOffset = D3D11_APPEND_ALIGNED_ELEMENT;
            desc->InputSlotClass = D3D11_INPUT_PER_VERTEX_DATA;
        }

        hr = ID3D11Device_CreateInputLayout(device, descs, runner->r.input_element_count,
                ID3D10Blob_GetBufferPointer(vs_code), ID3D10Blob_GetBufferSize(vs_code), &input_layout);
        ok(hr == S_OK, "Failed to create input layout, hr %#lx.\n", hr);
        ID3D11DeviceContext_IASetInputLayout(context, input_layout);
        ID3D11InputLayout_Release(input_layout);
    }

    ID3D11DeviceContext_IASetPrimitiveTopology(context, primitive_topology);
    ID3D11DeviceContext_VSSetShader(context, vs, NULL, 0);
    ID3D11DeviceContext_PSSetShader(context, ps, NULL, 0);
    ID3D11DeviceContext_RSSetState(context, runner->rasterizer_state);

    ID3D11DeviceContext_Draw(context, vertex_count, 0);

    ID3D11PixelShader_Release(ps);
    ID3D11VertexShader_Release(vs);
    if (cb)
        ID3D11Buffer_Release(cb);

    return true;
}

struct d3d11_resource_readback
{
    struct resource_readback rb;
    ID3D11Resource *resource;
};

static struct resource_readback *d3d11_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct d3d11_shader_runner *runner = d3d11_shader_runner(r);
    struct d3d11_resource_readback *rb = malloc(sizeof(*rb));
    struct d3d11_resource *resource = d3d11_resource(res);
    D3D11_TEXTURE2D_DESC texture_desc;
    D3D11_MAPPED_SUBRESOURCE map_desc;
    D3D11_BUFFER_DESC buffer_desc;
    HRESULT hr;

    switch (resource->r.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
        case RESOURCE_TYPE_UAV:
            if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
            {
                ID3D11Buffer_GetDesc(resource->buffer, &buffer_desc);
                buffer_desc.Usage = D3D11_USAGE_STAGING;
                buffer_desc.BindFlags = 0;
                buffer_desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
                buffer_desc.MiscFlags = 0;
                hr = ID3D11Device_CreateBuffer(runner->device, &buffer_desc, NULL, (ID3D11Buffer **)&rb->resource);
                ok(hr == S_OK, "Failed to create buffer, hr %#lx.\n", hr);
            }
            else
            {
                ID3D11Texture2D_GetDesc(resource->texture, &texture_desc);
                texture_desc.Usage = D3D11_USAGE_STAGING;
                texture_desc.BindFlags = 0;
                texture_desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
                texture_desc.MiscFlags = 0;
                hr = ID3D11Device_CreateTexture2D(runner->device, &texture_desc, NULL, (ID3D11Texture2D **)&rb->resource);
                ok(hr == S_OK, "Failed to create texture, hr %#lx.\n", hr);
            }
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
        case RESOURCE_TYPE_TEXTURE:
            assert(0);
    }

    ID3D11DeviceContext_CopyResource(runner->immediate_context, rb->resource, resource->resource);
    hr = ID3D11DeviceContext_Map(runner->immediate_context, rb->resource, 0, D3D11_MAP_READ, 0, &map_desc);
    ok(hr == S_OK, "Failed to map texture, hr %#lx.\n", hr);

    rb->rb.data = map_desc.pData;
    rb->rb.row_pitch = map_desc.RowPitch;
    rb->rb.width = resource->r.width;
    rb->rb.height = resource->r.height;
    rb->rb.depth = 1;
    return &rb->rb;
}

static void d3d11_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct d3d11_resource_readback *d3d11_rb = CONTAINING_RECORD(rb, struct d3d11_resource_readback, rb);
    struct d3d11_shader_runner *runner = d3d11_shader_runner(r);

    ID3D11DeviceContext_Unmap(runner->immediate_context, d3d11_rb->resource, 0);
    ID3D11Resource_Release(d3d11_rb->resource);
    free(d3d11_rb);
}

static const struct shader_runner_ops d3d11_runner_ops =
{
    .create_resource = d3d11_runner_create_resource,
    .destroy_resource = d3d11_runner_destroy_resource,
    .dispatch = d3d11_runner_dispatch,
    .draw = d3d11_runner_draw,
    .get_resource_readback = d3d11_runner_get_resource_readback,
    .release_readback = d3d11_runner_release_readback,
};

void run_shader_tests_d3d11(void)
{
    struct d3d11_shader_runner runner;
    HMODULE dxgi_module, d3d11_module;

    d3d11_module = LoadLibraryA("d3d11.dll");
    dxgi_module = LoadLibraryA("dxgi.dll");
    if (d3d11_module && dxgi_module)
    {
        pCreateDXGIFactory1 = (void *)GetProcAddress(dxgi_module, "CreateDXGIFactory1");
        pD3D11CreateDevice = (void *)GetProcAddress(d3d11_module, "D3D11CreateDevice");

        init_adapter_info();
        if (init_test_context(&runner))
        {
            run_shader_tests(&runner.r, &runner.caps, &d3d11_runner_ops, NULL);
            destroy_test_context(&runner);
        }
    }
    FreeLibrary(d3d11_module);
    FreeLibrary(dxgi_module);
}

#endif
