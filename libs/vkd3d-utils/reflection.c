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

struct d3d12_buffer
{
    ID3D12ShaderReflectionConstantBuffer ID3D12ShaderReflectionConstantBuffer_iface;
    D3D12_SHADER_BUFFER_DESC desc;
};

struct d3d12_reflection
{
    ID3D12ShaderReflection ID3D12ShaderReflection_iface;
    unsigned int refcount;

    struct vkd3d_shader_scan_signature_info signature_info;

    D3D12_SHADER_DESC desc;

    struct d3d12_buffer *buffers;
};

static struct d3d12_buffer null_buffer;

static struct d3d12_buffer *impl_from_ID3D12ShaderReflectionConstantBuffer(ID3D12ShaderReflectionConstantBuffer *iface)
{
    return CONTAINING_RECORD(iface, struct d3d12_buffer, ID3D12ShaderReflectionConstantBuffer_iface);
}

static HRESULT STDMETHODCALLTYPE d3d12_buffer_GetDesc(
        ID3D12ShaderReflectionConstantBuffer *iface, D3D12_SHADER_BUFFER_DESC *desc)
{
    struct d3d12_buffer *buffer = impl_from_ID3D12ShaderReflectionConstantBuffer(iface);

    TRACE("iface %p, desc %p.\n", iface, desc);

    if (buffer == &null_buffer)
    {
        WARN("Null constant buffer, returning E_FAIL.\n");
        return E_FAIL;
    }

    if (!desc)
    {
        WARN("NULL pointer, returning E_FAIL.\n");
        return E_FAIL;
    }

    *desc = buffer->desc;
    return S_OK;
}

static ID3D12ShaderReflectionVariable * STDMETHODCALLTYPE d3d12_buffer_GetVariableByIndex(
        ID3D12ShaderReflectionConstantBuffer *iface, UINT index)
{
    FIXME("iface %p, index %u, stub!\n", iface, index);

    return NULL;
}

static ID3D12ShaderReflectionVariable * STDMETHODCALLTYPE d3d12_buffer_GetVariableByName(
        ID3D12ShaderReflectionConstantBuffer *iface, const char *name)
{
    FIXME("iface %p, name %s, stub!\n", iface, debugstr_a(name));

    return NULL;
}

static const struct ID3D12ShaderReflectionConstantBufferVtbl d3d12_buffer_vtbl =
{
    d3d12_buffer_GetDesc,
    d3d12_buffer_GetVariableByIndex,
    d3d12_buffer_GetVariableByName,
};

static struct d3d12_buffer null_buffer = {{&d3d12_buffer_vtbl}};

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
        for (UINT i = 0; i < reflection->desc.ConstantBuffers; ++i)
            vkd3d_free((void *)reflection->buffers[i].desc.Name);
        vkd3d_free(reflection->buffers);

        vkd3d_shader_free_scan_signature_info(&reflection->signature_info);
        free(reflection);
    }

    return refcount;
}

/* ID3D12ShaderReflection methods */

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetDesc(ID3D12ShaderReflection *iface, D3D12_SHADER_DESC *desc)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);

    FIXME("iface %p, desc %p partial stub!\n", iface, desc);

    *desc = reflection->desc;

    return S_OK;
}

static struct ID3D12ShaderReflectionConstantBuffer * STDMETHODCALLTYPE d3d12_reflection_GetConstantBufferByIndex(
        ID3D12ShaderReflection *iface, UINT index)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);

    TRACE("iface %p, index %u.\n", iface, index);

    if (index > reflection->desc.ConstantBuffers)
    {
        WARN("Invalid index %u.\n", index);
        return &null_buffer.ID3D12ShaderReflectionConstantBuffer_iface;
    }

    return &reflection->buffers[index].ID3D12ShaderReflectionConstantBuffer_iface;
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

static HRESULT get_signature_parameter(const struct vkd3d_shader_signature *signature,
        unsigned int index, D3D12_SIGNATURE_PARAMETER_DESC *desc, bool output)
{
    const struct vkd3d_shader_signature_element *e;

    if (!desc || index >= signature->element_count)
    {
        WARN("Invalid argument specified.\n");
        return E_INVALIDARG;
    }
    e = &signature->elements[index];

    desc->SemanticName = e->semantic_name;
    desc->SemanticIndex = e->semantic_index;
    desc->Register = e->register_index;
    desc->SystemValueType = (D3D_NAME)e->sysval_semantic;
    desc->ComponentType = (D3D_REGISTER_COMPONENT_TYPE)e->component_type;
    desc->Mask = e->mask;
    desc->ReadWriteMask = output ? (e->mask & ~e->used_mask) : e->used_mask;
    desc->Stream = e->stream_index;
    desc->MinPrecision = (D3D_MIN_PRECISION)e->min_precision;

    return S_OK;
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetInputParameterDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SIGNATURE_PARAMETER_DESC *desc)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);

    TRACE("iface %p, index %u, desc %p.\n", iface, index, desc);

    return get_signature_parameter(&reflection->signature_info.input, index, desc, false);
}

static HRESULT STDMETHODCALLTYPE d3d12_reflection_GetOutputParameterDesc(
        ID3D12ShaderReflection *iface, UINT index, D3D12_SIGNATURE_PARAMETER_DESC *desc)
{
    struct d3d12_reflection *reflection = impl_from_ID3D12ShaderReflection(iface);

    TRACE("iface %p, index %u, desc %p.\n", iface, index, desc);

    return get_signature_parameter(&reflection->signature_info.output, index, desc, true);
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

static bool require_space(size_t offset, size_t count, size_t size, size_t data_size)
{
    return !count || (data_size - offset) / count >= size;
}

/* Return a pointer to data in a code blob, with bounds checking. */
static const void *get_data_ptr(const struct vkd3d_shader_code *code,
        uint32_t offset, uint32_t count, uint32_t size)
{
    if (!require_space(offset, count, size, code->size))
    {
        WARN("Offset %#x and size %#x exceeds section size %#zx.\n", offset, size, code->size);
        return NULL;
    }

    return (const uint8_t *)code->code + offset;
}

static HRESULT get_string(const struct vkd3d_shader_code *code, uint32_t offset, char **ret)
{
    const char *str;
    char *end;

    if (offset >= code->size)
    {
        WARN("Offset %#x exceeds size %#zx.\n", offset, code->size);
        return E_INVALIDARG;
    }

    str = (const char *)code->code + offset;
    if (!(end = memchr(str, 0, code->size - offset)))
    {
        WARN("String at %#x is not properly zero-terminated.\n", offset);
        return E_INVALIDARG;
    }

    if (!(*ret = vkd3d_memdup(str, end + 1 - str)))
        return E_OUTOFMEMORY;
    return S_OK;
}

struct rdef_header
{
    uint32_t buffer_count;
    uint32_t buffers_offset;
    uint32_t binding_count;
    uint32_t bindings_offset;
    uint8_t minor_version;
    uint8_t major_version;
    uint16_t type;
    uint32_t compile_flags;
    uint32_t creator_offset;
};

struct rdef_buffer
{
    uint32_t name_offset;
    uint32_t var_count;
    uint32_t vars_offset;
    uint32_t size;
    uint32_t flags;
    uint32_t type;
};

static HRESULT d3d12_buffer_init(struct d3d12_buffer *buffer, const struct rdef_buffer *rdef_buffer,
        const struct vkd3d_shader_code *section)
{
    HRESULT hr;
    char *name;

    if ((FAILED(hr = get_string(section, rdef_buffer->name_offset, &name))))
        return hr;

    buffer->ID3D12ShaderReflectionConstantBuffer_iface.lpVtbl = &d3d12_buffer_vtbl;

    buffer->desc.Type = rdef_buffer->type;
    buffer->desc.Variables = rdef_buffer->var_count;
    buffer->desc.Size = rdef_buffer->size;
    buffer->desc.uFlags = rdef_buffer->flags;
    buffer->desc.Name = name;

    return S_OK;
}

static HRESULT parse_rdef(struct d3d12_reflection *reflection, const struct vkd3d_shader_code *section)
{
    const struct rdef_header *header;
    HRESULT hr;

    if (!(header = get_data_ptr(section, 0, 1, sizeof(*header))))
        return E_INVALIDARG;

    reflection->desc.ConstantBuffers = header->buffer_count;

    if (header->buffer_count)
    {
        const struct rdef_buffer *rdef_buffers;

        if (!(rdef_buffers = get_data_ptr(section, header->buffers_offset,
                header->buffer_count, sizeof(*rdef_buffers))))
            return E_INVALIDARG;

        if (!(reflection->buffers = vkd3d_calloc(header->buffer_count, sizeof(*reflection->buffers))))
            return E_OUTOFMEMORY;

        for (uint32_t i = 0; i < header->buffer_count; ++i)
        {
            if ((hr = d3d12_buffer_init(&reflection->buffers[i], &rdef_buffers[i], section)))
                return hr;
        }
    }

    return S_OK;
}

static HRESULT d3d12_reflection_init(struct d3d12_reflection *reflection, const void *data, size_t data_size)
{
    struct vkd3d_shader_compile_info compile_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_dxbc_desc dxbc_desc;
    bool found_rdef = false;
    enum vkd3d_result ret;
    HRESULT hr;

    reflection->ID3D12ShaderReflection_iface.lpVtbl = &d3d12_reflection_vtbl;
    reflection->refcount = 1;

    compile_info.source.code = data;
    compile_info.source.size = data_size;
    compile_info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;

    compile_info.next = &reflection->signature_info;
    reflection->signature_info.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_SIGNATURE_INFO;

    if (FAILED(hr = hresult_from_vkd3d_result(vkd3d_shader_scan(&compile_info, NULL))))
        return hr;

    if ((ret = vkd3d_shader_parse_dxbc(&compile_info.source, 0, &dxbc_desc, NULL)))
    {
        vkd3d_shader_free_scan_signature_info(&reflection->signature_info);
        return hresult_from_vkd3d_result(ret);
    }

    for (unsigned int i = 0; i < dxbc_desc.section_count; ++i)
    {
        const struct vkd3d_shader_dxbc_section_desc *section = &dxbc_desc.sections[i];

        if (section->tag == TAG_RDEF)
        {
            if (found_rdef)
            {
                FIXME("Multiple RDEF chunks.\n");
                continue;
            }

            if (FAILED(hr = parse_rdef(reflection, &section->data)))
            {
                vkd3d_shader_free_scan_signature_info(&reflection->signature_info);
                vkd3d_shader_free_dxbc(&dxbc_desc);
                return hr;
            }
            found_rdef = true;
        }
    }

    reflection->desc.InputParameters = reflection->signature_info.input.element_count;
    reflection->desc.OutputParameters = reflection->signature_info.output.element_count;
    reflection->desc.PatchConstantParameters = reflection->signature_info.patch_constant.element_count;

    vkd3d_shader_free_dxbc(&dxbc_desc);

    return S_OK;
}

HRESULT WINAPI D3DReflect(const void *data, SIZE_T data_size, REFIID iid, void **reflection)
{
    struct d3d12_reflection *object;
    HRESULT hr;

    TRACE("data %p, data_size %"PRIuPTR", iid %s, reflection %p.\n",
            data, (uintptr_t)data_size, debugstr_guid(iid), reflection);

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
