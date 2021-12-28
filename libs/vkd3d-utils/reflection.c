/*
 * Copyright 2009 Henri Verbeet for CodeWeavers
 * Copyright 2010 Rico Schüller
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

#include "vkd3d_utils_private.h"
#include <vkd3d_d3dcommon.h>
#include <vkd3d_d3d12shader.h>

struct d3d12_reflection
{
    ID3D12ShaderReflection ID3D12ShaderReflection_iface;
    unsigned int refcount;
};

static struct d3d12_reflection *impl_from_ID3D12ShaderReflection(ID3D12ShaderReflection *iface)
{
    return CONTAINING_RECORD(iface, struct d3d12_reflection, ID3D12ShaderReflection_iface);
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_QueryInterface(
        ID3D12ShaderReflection *iface, REFIID riid, void **object)
{
    TRACE("iface %p, riid %s, object %p\n", iface, debugstr_guid(riid), object);

    if (IsEqualGUID(riid, &IID_ID3D12ShaderReflection)
            || IsEqualGUID(riid, &IID_IUnknown))
    {
        IUnknown_AddRef(iface);
        *object = iface;
        return S_OK;
    }

    WARN("%s not implemented, returning E_NOINTERFACE.\n", debugstr_guid(riid));

    *object = NULL;
    return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE d3d12_reflection_AddRef(ID3D12ShaderReflection *iface)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);
    unsigned int refcount = vkd3d_atomic_increment_u32(&reflection->refcount);

    TRACE("%p increasing refcount to %u.\n", reflection, refcount);

    return refcount;
}

static ULONG STDMETHODCALLTYPE d3d12_reflection_Release(ID3D12ShaderReflection *iface)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);
    unsigned int refcount = vkd3d_atomic_decrement_u32(&reflection->refcount);

    TRACE("%p decreasing refcount to %u.\n", reflection, refcount);

    if (!refcount)
    {
        free(reflection);
    }

    return refcount;
}

/* ID3D12ShaderReflection methods */

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetDesc(ID3D12ShaderReflection *iface, D3D12_SHADER_DESC *desc)
{
    FIXME("iface %p, desc %p partial stub!\n", iface, desc);

    return E_NOTIMPL;
}

static struct ID3D12ShaderReflectionConstantBuffer * STDMETHODCALLTYPE d3d12_reflection_GetConstantBufferByIndex(
        ID3D12ShaderReflection *iface, UINT index)
{
    FIXME("iface %p, index %u stub!\n", iface, index);

    return NULL;
}

static struct ID3D12ShaderReflectionConstantBuffer * STDMETHODCALLTYPE d3d12_reflection_GetConstantBufferByName(
        ID3D12ShaderReflection *iface, const char *name)
{
    FIXME("iface %p, name %s stub!\n", iface, debugstr_a(name));

    return NULL;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetResourceBindingDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SHADER_INPUT_BIND_DESC *desc)
{
    FIXME("iface %p, index %u, desc %p stub!\n", iface, index, desc);

    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetInputParameterDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SIGNATURE_PARAMETER_DESC *desc)
{
    FIXME("iface %p, index %u, desc %p stub!\n", iface, index, desc);

    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetOutputParameterDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SIGNATURE_PARAMETER_DESC *desc)
{
    FIXME("iface %p, index %u, desc %p stub!\n", iface, index, desc);

    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetPatchConstantParameterDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SIGNATURE_PARAMETER_DESC *desc)
{
    FIXME("iface %p, index %u, desc %p stub!\n", iface, index, desc);

    return E_NOTIMPL;
}

static struct ID3D12ShaderReflectionVariable * STDMETHODCALLTYPE d3d12_reflection_GetVariableByName(
        ID3D12ShaderReflection *iface, const char *name)
{
    FIXME("iface %p, name %s stub!\n", iface, debugstr_a(name));

    return NULL;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetResourceBindingDescByName(
        ID3D12ShaderReflection *iface, const char *name, D3D12_SHADER_INPUT_BIND_DESC *desc)
{
    FIXME("iface %p, name %s, desc %p stub!\n", iface, debugstr_a(name), desc);

    return E_NOTIMPL;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetMovInstructionCount(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetMovcInstructionCount(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetConversionInstructionCount(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetBitwiseInstructionCount(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static D3D_PRIMITIVE STDMETHODCALLTYPE d3d12_reflection_GetGSInputPrimitive(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static BOOL STDMETHODCALLTYPE d3d12_reflection_IsSampleFrequencyShader(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return FALSE;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetNumInterfaceSlots(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetMinFeatureLevel(
        ID3D12ShaderReflection *iface, D3D_FEATURE_LEVEL *level)
{
    FIXME("iface %p, level %p stub!\n", iface, level);

    return E_NOTIMPL;
}

static UINT STDMETHODCALLTYPE d3d12_reflection_GetThreadGroupSize(
        ID3D12ShaderReflection *iface, UINT *sizex, UINT *sizey, UINT *sizez)
{
    FIXME("iface %p, sizex %p, sizey %p, sizez %p stub!\n", iface, sizex, sizey, sizez);

    return 0;
}

static UINT64 STDMETHODCALLTYPE d3d12_reflection_GetRequiresFlags(ID3D12ShaderReflection *iface)
{
    FIXME("iface %p stub!\n", iface);

    return 0;
}

static const struct ID3D12ShaderReflectionVtbl d3d12_reflection_vtbl =
{
    /* IUnknown methods */
    d3d12_reflection_QueryInterface,
    d3d12_reflection_AddRef,
    d3d12_reflection_Release,
    /* ID3D12ShaderReflection methods */
    d3d12_reflection_GetDesc,
    d3d12_reflection_GetConstantBufferByIndex,
    d3d12_reflection_GetConstantBufferByName,
    d3d12_reflection_GetResourceBindingDesc,
    d3d12_reflection_GetInputParameterDesc,
    d3d12_reflection_GetOutputParameterDesc,
    d3d12_reflection_GetPatchConstantParameterDesc,
    d3d12_reflection_GetVariableByName,
    d3d12_reflection_GetResourceBindingDescByName,
    d3d12_reflection_GetMovInstructionCount,
    d3d12_reflection_GetMovcInstructionCount,
    d3d12_reflection_GetConversionInstructionCount,
    d3d12_reflection_GetBitwiseInstructionCount,
    d3d12_reflection_GetGSInputPrimitive,
    d3d12_reflection_IsSampleFrequencyShader,
    d3d12_reflection_GetNumInterfaceSlots,
    d3d12_reflection_GetMinFeatureLevel,
    d3d12_reflection_GetThreadGroupSize,
    d3d12_reflection_GetRequiresFlags,
};

static HRESULT d3d12_reflection_init(struct d3d12_reflection *reflection, const void *data, size_t data_size)
{
    reflection->ID3D12ShaderReflection_iface.lpVtbl = &d3d12_reflection_vtbl;
    reflection->refcount = 1;

    return S_OK;
}

HRESULT WINAPI D3DReflect(const void *data, SIZE_T data_size, REFIID iid, void **reflection)
{
    struct d3d12_reflection *object;
    HRESULT hr;

    TRACE("data %p, data_size %lu, iid %s, reflection %p.\n", data, data_size, debugstr_guid(iid), reflection);

    if (!IsEqualGUID(iid, &IID_ID3D12ShaderReflection))
    {
        WARN("Invalid iid %s.\n", debugstr_guid(iid));
        return E_INVALIDARG;
    }

    if (!(object = vkd3d_calloc(1, sizeof(*object))))
        return E_OUTOFMEMORY;

    if (FAILED(hr = d3d12_reflection_init(object, data, data_size)))
    {
        free(object);
        return hr;
    }

    *reflection = &object->ID3D12ShaderReflection_iface;
    TRACE("Created reflection %p.\n", object);
    return S_OK;
}
