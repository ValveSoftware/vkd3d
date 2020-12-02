/*
 * Copyright 2016 Józef Kucia for CodeWeavers
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
#undef D3D12CreateDevice

VKD3D_DEBUG_ENV_NAME("VKD3D_DEBUG");

HRESULT WINAPI D3D12GetDebugInterface(REFIID iid, void **debug)
{
    FIXME("iid %s, debug %p stub!\n", debugstr_guid(iid), debug);

    return E_NOTIMPL;
}

HRESULT WINAPI D3D12CreateDeviceVKD3D(IUnknown *adapter, D3D_FEATURE_LEVEL minimum_feature_level,
        REFIID iid, void **device, enum vkd3d_api_version api_version)
{
    struct vkd3d_optional_instance_extensions_info optional_extensions_info;
    struct vkd3d_instance_create_info instance_create_info;
    struct vkd3d_device_create_info device_create_info;

    static const char * const instance_extensions[] =
    {
        VK_KHR_SURFACE_EXTENSION_NAME,
    };
    static const char * const optional_instance_extensions[] =
    {
        "VK_KHR_xcb_surface",
        "VK_MVK_macos_surface",
    };
    static const char * const device_extensions[] =
    {
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    };
    struct vkd3d_application_info application_info =
    {
        .type = VKD3D_STRUCTURE_TYPE_APPLICATION_INFO,
        .api_version = api_version,
    };

    TRACE("adapter %p, minimum_feature_level %#x, iid %s, device %p, api_version %#x.\n",
            adapter, minimum_feature_level, debugstr_guid(iid), device, api_version);

    if (adapter)
        FIXME("Ignoring adapter %p.\n", adapter);

    memset(&optional_extensions_info, 0, sizeof(optional_extensions_info));
    optional_extensions_info.type = VKD3D_STRUCTURE_TYPE_OPTIONAL_INSTANCE_EXTENSIONS_INFO;
    optional_extensions_info.next = &application_info;
    optional_extensions_info.extensions = optional_instance_extensions;
    optional_extensions_info.extension_count = ARRAY_SIZE(optional_instance_extensions);

    memset(&instance_create_info, 0, sizeof(instance_create_info));
    instance_create_info.type = VKD3D_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    instance_create_info.next = &optional_extensions_info;
    instance_create_info.pfn_signal_event = vkd3d_signal_event;
    instance_create_info.wchar_size = sizeof(WCHAR);
    instance_create_info.instance_extensions = instance_extensions;
    instance_create_info.instance_extension_count = ARRAY_SIZE(instance_extensions);

    memset(&device_create_info, 0, sizeof(device_create_info));
    device_create_info.type = VKD3D_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    device_create_info.next = NULL;
    device_create_info.minimum_feature_level = minimum_feature_level;
    device_create_info.instance_create_info = &instance_create_info;
    device_create_info.device_extensions = device_extensions;
    device_create_info.device_extension_count = ARRAY_SIZE(device_extensions);

    return vkd3d_create_device(&device_create_info, iid, device);
}

HRESULT WINAPI D3D12CreateDevice(IUnknown *adapter,
        D3D_FEATURE_LEVEL minimum_feature_level, REFIID iid, void **device)
{
    return D3D12CreateDeviceVKD3D(adapter, minimum_feature_level, iid, device, VKD3D_API_VERSION_1_0);
}

HRESULT WINAPI D3D12CreateRootSignatureDeserializer(const void *data, SIZE_T data_size,
        REFIID iid, void **deserializer)
{
    TRACE("data %p, data_size %lu, iid %s, deserializer %p.\n",
            data, data_size, debugstr_guid(iid), deserializer);

    return vkd3d_create_root_signature_deserializer(data, data_size, iid, deserializer);
}

HRESULT WINAPI D3D12CreateVersionedRootSignatureDeserializer(const void *data, SIZE_T data_size,
        REFIID iid,void **deserializer)
{
    TRACE("data %p, data_size %lu, iid %s, deserializer %p.\n",
            data, data_size, debugstr_guid(iid), deserializer);

    return vkd3d_create_versioned_root_signature_deserializer(data, data_size, iid, deserializer);
}

HRESULT WINAPI D3D12SerializeRootSignature(const D3D12_ROOT_SIGNATURE_DESC *desc,
        D3D_ROOT_SIGNATURE_VERSION version, ID3DBlob **blob, ID3DBlob **error_blob)
{
    TRACE("desc %p, version %#x, blob %p, error_blob %p.\n", desc, version, blob, error_blob);

    return vkd3d_serialize_root_signature(desc, version, blob, error_blob);
}

HRESULT WINAPI D3D12SerializeVersionedRootSignature(const D3D12_VERSIONED_ROOT_SIGNATURE_DESC *desc,
        ID3DBlob **blob, ID3DBlob **error_blob)
{
    TRACE("desc %p, blob %p, error_blob %p.\n", desc, blob, error_blob);

    return vkd3d_serialize_versioned_root_signature(desc, blob, error_blob);
}

static int open_include(const char *filename, bool local, const char *parent_data, void *context,
        struct vkd3d_shader_code *code)
{
    ID3DInclude *iface = context;
    unsigned int size = 0;

    if (!iface)
        return VKD3D_ERROR;

    memset(code, 0, sizeof(*code));
    if (FAILED(ID3DInclude_Open(iface, local ? D3D_INCLUDE_LOCAL : D3D_INCLUDE_SYSTEM,
            filename, parent_data, &code->code, &size)))
        return VKD3D_ERROR;

    code->size = size;
    return VKD3D_OK;
}

static void close_include(const struct vkd3d_shader_code *code, void *context)
{
    ID3DInclude *iface = context;

    ID3DInclude_Close(iface, code->code);
}

HRESULT WINAPI D3DCompile2(const void *data, SIZE_T data_size, const char *filename,
        const D3D_SHADER_MACRO *macros, ID3DInclude *include, const char *entry_point,
        const char *profile, UINT flags, UINT effect_flags, UINT secondary_flags,
        const void *secondary_data, SIZE_T secondary_data_size, ID3DBlob **shader_blob,
        ID3DBlob **messages_blob)
{
    struct vkd3d_shader_preprocess_info preprocess_info;
    struct vkd3d_shader_hlsl_source_info hlsl_info;
    struct vkd3d_shader_compile_option options[1];
    struct vkd3d_shader_compile_info compile_info;
    struct vkd3d_shader_code byte_code;
    const D3D_SHADER_MACRO *macro;
    char *messages;
    HRESULT hr;
    int ret;

    TRACE("data %p, data_size %lu, filename %s, macros %p, include %p, entry_point %s, "
            "profile %s, flags %#x, effect_flags %#x, secondary_flags %#x, secondary_data %p, "
            "secondary_data_size %lu, shader_blob %p, messages_blob %p.\n",
            data, data_size, debugstr_a(filename), macros, include, debugstr_a(entry_point),
            debugstr_a(profile), flags, effect_flags, secondary_flags, secondary_data,
            secondary_data_size, shader_blob, messages_blob);

    if (flags & ~D3DCOMPILE_DEBUG)
        FIXME("Ignoring flags %#x.\n", flags);
    if (effect_flags)
        FIXME("Ignoring effect flags %#x.\n", effect_flags);
    if (secondary_flags)
        FIXME("Ignoring secondary flags %#x.\n", secondary_flags);

    if (messages_blob)
        *messages_blob = NULL;

    compile_info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    compile_info.next = &preprocess_info;
    compile_info.source.code = data;
    compile_info.source.size = data_size;
    compile_info.source_type = VKD3D_SHADER_SOURCE_HLSL;
    compile_info.target_type = VKD3D_SHADER_TARGET_DXBC_TPF;
    compile_info.options = options;
    compile_info.option_count = 0;
    compile_info.log_level = VKD3D_SHADER_LOG_INFO;
    compile_info.source_name = filename;

    preprocess_info.type = VKD3D_SHADER_STRUCTURE_TYPE_PREPROCESS_INFO;
    preprocess_info.next = &hlsl_info;
    preprocess_info.macros = (const struct vkd3d_shader_macro *)macros;
    preprocess_info.macro_count = 0;
    if (macros)
    {
        for (macro = macros; macro->Name; ++macro)
            ++preprocess_info.macro_count;
    }
    preprocess_info.pfn_open_include = open_include;
    preprocess_info.pfn_close_include = close_include;
    preprocess_info.include_context = include;

    hlsl_info.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO;
    hlsl_info.next = NULL;
    hlsl_info.profile = profile;
    hlsl_info.entry_point = entry_point;
    hlsl_info.secondary_code.code = secondary_data;
    hlsl_info.secondary_code.size = secondary_data_size;

    if (!(flags & D3DCOMPILE_DEBUG))
        options[compile_info.option_count++].name = VKD3D_SHADER_COMPILE_OPTION_STRIP_DEBUG;

    ret = vkd3d_shader_compile(&compile_info, &byte_code, &messages);
    if (messages)
    {
        if (messages_blob)
        {
            if (FAILED(hr = vkd3d_blob_create(messages, strlen(messages), messages_blob)))
            {
                vkd3d_shader_free_shader_code(&byte_code);
                return hr;
            }
        }
        else
            vkd3d_shader_free_messages(messages);
    }

    if (!ret)
    {
        if (FAILED(hr = vkd3d_blob_create((void *)byte_code.code, byte_code.size, shader_blob)))
        {
            vkd3d_shader_free_shader_code(&byte_code);
            return hr;
        }
    }

    return hresult_from_vkd3d_result(ret);
}

HRESULT WINAPI D3DCompile(const void *data, SIZE_T data_size, const char *filename,
        const D3D_SHADER_MACRO *macros, ID3DInclude *include, const char *entrypoint,
        const char *profile, UINT flags, UINT effect_flags, ID3DBlob **shader, ID3DBlob **error_messages)
{
    TRACE("data %p, data_size %lu, filename %s, macros %p, include %p, entrypoint %s, "
            "profile %s, flags %#x, effect_flags %#x, shader %p, error_messages %p.\n",
            data, data_size, debugstr_a(filename), macros, include, debugstr_a(entrypoint),
            debugstr_a(profile), flags, effect_flags, shader, error_messages);

    return D3DCompile2(data, data_size, filename, macros, include, entrypoint, profile, flags,
            effect_flags, 0, NULL, 0, shader, error_messages);
}

HRESULT WINAPI D3DPreprocess(const void *data, SIZE_T size, const char *filename,
        const D3D_SHADER_MACRO *macros, ID3DInclude *include,
        ID3DBlob **preprocessed_blob, ID3DBlob **messages_blob)
{
    struct vkd3d_shader_preprocess_info preprocess_info;
    struct vkd3d_shader_compile_info compile_info;
    struct vkd3d_shader_code preprocessed_code;
    const D3D_SHADER_MACRO *macro;
    char *messages;
    HRESULT hr;
    int ret;

    TRACE("data %p, size %lu, filename %s, macros %p, include %p, preprocessed_blob %p, messages_blob %p.\n",
            data, size, debugstr_a(filename), macros, include, preprocessed_blob, messages_blob);

    if (messages_blob)
        *messages_blob = NULL;

    compile_info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    compile_info.next = &preprocess_info;
    compile_info.source.code = data;
    compile_info.source.size = size;
    compile_info.source_type = VKD3D_SHADER_SOURCE_HLSL;
    compile_info.target_type = VKD3D_SHADER_TARGET_NONE;
    compile_info.options = NULL;
    compile_info.option_count = 0;
    compile_info.log_level = VKD3D_SHADER_LOG_INFO;
    compile_info.source_name = filename;

    preprocess_info.type = VKD3D_SHADER_STRUCTURE_TYPE_PREPROCESS_INFO;
    preprocess_info.next = NULL;
    preprocess_info.macros = (const struct vkd3d_shader_macro *)macros;
    preprocess_info.macro_count = 0;
    if (macros)
    {
        for (macro = macros; macro->Name; ++macro)
            ++preprocess_info.macro_count;
    }
    preprocess_info.pfn_open_include = open_include;
    preprocess_info.pfn_close_include = close_include;
    preprocess_info.include_context = include;

    ret = vkd3d_shader_preprocess(&compile_info, &preprocessed_code, &messages);
    if (messages)
    {
        if (messages_blob)
        {
            if (FAILED(hr = vkd3d_blob_create(messages, strlen(messages), messages_blob)))
            {
                vkd3d_shader_free_shader_code(&preprocessed_code);
                return hr;
            }
        }
        else
            vkd3d_shader_free_messages(messages);
    }

    if (!ret)
    {
        if (FAILED(hr = vkd3d_blob_create((void *)preprocessed_code.code, preprocessed_code.size, preprocessed_blob)))
        {
            vkd3d_shader_free_shader_code(&preprocessed_code);
            return hr;
        }
    }

    return hresult_from_vkd3d_result(ret);
}

/* Events */
HANDLE vkd3d_create_event(void)
{
    struct vkd3d_event *event;
    int rc;

    TRACE(".\n");

    if (!(event = vkd3d_malloc(sizeof(*event))))
        return NULL;

    if ((rc = pthread_mutex_init(&event->mutex, NULL)))
    {
        ERR("Failed to initialize mutex, error %d.\n", rc);
        vkd3d_free(event);
        return NULL;
    }
    if ((rc = pthread_cond_init(&event->cond, NULL)))
    {
        ERR("Failed to initialize condition variable, error %d.\n", rc);
        pthread_mutex_destroy(&event->mutex);
        vkd3d_free(event);
        return NULL;
    }

    event->is_signaled = false;

    TRACE("Created event %p.\n", event);

    return event;
}

unsigned int vkd3d_wait_event(HANDLE event, unsigned int milliseconds)
{
    struct vkd3d_event *impl = event;
    int rc;

    TRACE("event %p, milliseconds %u.\n", event, milliseconds);

    if ((rc = pthread_mutex_lock(&impl->mutex)))
    {
        ERR("Failed to lock mutex, error %d.\n", rc);
        return VKD3D_WAIT_FAILED;
    }

    if (impl->is_signaled || !milliseconds)
    {
        bool is_signaled = impl->is_signaled;
        impl->is_signaled = false;
        pthread_mutex_unlock(&impl->mutex);
        return is_signaled ? VKD3D_WAIT_OBJECT_0 : VKD3D_WAIT_TIMEOUT;
    }

    if (milliseconds == VKD3D_INFINITE)
    {
        do
        {
            if ((rc = pthread_cond_wait(&impl->cond, &impl->mutex)))
            {
                ERR("Failed to wait on condition variable, error %d.\n", rc);
                pthread_mutex_unlock(&impl->mutex);
                return VKD3D_WAIT_FAILED;
            }
        } while (!impl->is_signaled);

        impl->is_signaled = false;
        pthread_mutex_unlock(&impl->mutex);
        return VKD3D_WAIT_OBJECT_0;
    }

    pthread_mutex_unlock(&impl->mutex);
    FIXME("Timed wait not implemented yet.\n");
    return VKD3D_WAIT_FAILED;
}

HRESULT vkd3d_signal_event(HANDLE event)
{
    struct vkd3d_event *impl = event;
    int rc;

    TRACE("event %p.\n", event);

    if ((rc = pthread_mutex_lock(&impl->mutex)))
    {
        ERR("Failed to lock mutex, error %d.\n", rc);
        return E_FAIL;
    }
    impl->is_signaled = true;
    pthread_cond_signal(&impl->cond);
    pthread_mutex_unlock(&impl->mutex);

    return S_OK;
}

void vkd3d_destroy_event(HANDLE event)
{
    struct vkd3d_event *impl = event;
    int rc;

    TRACE("event %p.\n", event);

    if ((rc = pthread_mutex_destroy(&impl->mutex)))
        ERR("Failed to destroy mutex, error %d.\n", rc);
    if ((rc = pthread_cond_destroy(&impl->cond)))
        ERR("Failed to destroy condition variable, error %d.\n", rc);
    vkd3d_free(impl);
}

HRESULT WINAPI D3DCreateBlob(SIZE_T data_size, ID3D10Blob **blob)
{
    HRESULT hr;
    void *data;

    TRACE("data_size %lu, blob %p.\n", data_size, blob);

    if (!(data = vkd3d_calloc(data_size, 1)))
        return E_OUTOFMEMORY;

    if (FAILED(hr = vkd3d_blob_create(data, data_size, blob)))
    {
        WARN("Failed to create blob object, hr %#x.\n", hr);
        vkd3d_free(data);
    }
    return hr;
}
