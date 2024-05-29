/*
 * Shader runner which uses libvkd3d-shader to compile HLSL -> D3D bytecode -> SPIR-V
 *
 * Copyright 2020-2022 Zebediah Figura for CodeWeavers
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

#ifndef _WIN32

#define VK_NO_PROTOTYPES
#define VKD3D_TEST_NO_DEFS
#include "config.h"
#include "vulkan/vulkan.h"
#include "vkd3d.h"
#include "vkd3d_d3dcompiler.h"
#include "shader_runner.h"
#include "vkd3d_test.h"

struct vulkan_resource
{
    struct resource r;

    VkBuffer buffer;
    VkBufferView buffer_view;
    VkImage image;
    VkImageView image_view;
    VkDeviceMemory memory;

    uint32_t binding;
};

static struct vulkan_resource *vulkan_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct vulkan_resource, r);
}

#define DECLARE_VK_PFN(name) PFN_##name name;

DECLARE_VK_PFN(vkGetInstanceProcAddr)

struct vulkan_shader_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;
    bool demote_to_helper_invocation;

    VkInstance instance;
    VkPhysicalDevice phys_device;
    VkDevice device;
    VkQueue queue;
    VkCommandPool command_pool;
    VkCommandBuffer cmd_buffer;
    VkDescriptorPool descriptor_pool;

    struct vkd3d_shader_scan_signature_info vs_signatures;

    struct vulkan_sampler
    {
        VkSampler vk_sampler;
        uint32_t binding;
    } samplers[MAX_SAMPLERS];

    DECLARE_VK_PFN(vkCreateInstance);
    DECLARE_VK_PFN(vkEnumerateInstanceExtensionProperties);
#define VK_INSTANCE_PFN   DECLARE_VK_PFN
#define VK_DEVICE_PFN     DECLARE_VK_PFN
#include "vulkan_procs.h"
};

struct extension_list
{
    const char **names;
    size_t count;
};

struct physical_device_info
{
    VkPhysicalDeviceFeatures2 features2;
    VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT interlock_features;
    VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT demote_to_helper_invocation_features;
};

static struct vulkan_shader_runner *vulkan_shader_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct vulkan_shader_runner, r);
}

#define VK_CALL(f) (runner->f)

static void begin_command_buffer(struct vulkan_shader_runner *runner)
{
    VkCommandBufferBeginInfo buffer_begin_desc = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO};

    VK_CALL(vkBeginCommandBuffer(runner->cmd_buffer, &buffer_begin_desc));
}

static void end_command_buffer(struct vulkan_shader_runner *runner)
{
    VkSubmitInfo submit_desc = {.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO};

    VK_CALL(vkEndCommandBuffer(runner->cmd_buffer));

    submit_desc.commandBufferCount = 1;
    submit_desc.pCommandBuffers = &runner->cmd_buffer;
    VK_CALL(vkQueueSubmit(runner->queue, 1, &submit_desc, VK_NULL_HANDLE));
    VK_CALL(vkQueueWaitIdle(runner->queue));
}

static void transition_image_layout(struct vulkan_shader_runner *runner,
        VkImage image, VkImageAspectFlags aspect_mask, VkImageLayout src_layout, VkImageLayout dst_layout)
{
    VkImageMemoryBarrier barrier = {.sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER};

    barrier.srcAccessMask = VK_ACCESS_MEMORY_READ_BIT | VK_ACCESS_MEMORY_WRITE_BIT;
    barrier.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT | VK_ACCESS_MEMORY_WRITE_BIT;
    barrier.oldLayout = src_layout;
    barrier.newLayout = dst_layout;
    barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.image = image;
    barrier.subresourceRange.aspectMask = aspect_mask;
    barrier.subresourceRange.baseMipLevel = 0;
    barrier.subresourceRange.levelCount = VK_REMAINING_MIP_LEVELS;
    barrier.subresourceRange.baseArrayLayer = 0;
    barrier.subresourceRange.layerCount = 1;

    VK_CALL(vkCmdPipelineBarrier(runner->cmd_buffer, VK_PIPELINE_STAGE_ALL_COMMANDS_BIT,
            VK_PIPELINE_STAGE_ALL_COMMANDS_BIT, 0, 0, NULL, 0, NULL, 1, &barrier));
}

static unsigned int select_vulkan_memory_type(const struct vulkan_shader_runner *runner,
        uint32_t memory_type_mask, VkMemoryPropertyFlags required_flags)
{
    VkPhysicalDeviceMemoryProperties memory_info;
    unsigned int i;

    VK_CALL(vkGetPhysicalDeviceMemoryProperties(runner->phys_device, &memory_info));

    for (i = 0; i < memory_info.memoryTypeCount; ++i)
    {
        if (!(memory_type_mask & (1u << i)))
            continue;
        if ((memory_info.memoryTypes[i].propertyFlags & required_flags) == required_flags)
            return i;
    }

    fatal_error("No valid memory types found matching mask %#x, property flags %#x.\n",
            memory_type_mask, required_flags);
}

static VkDeviceMemory allocate_memory(const struct vulkan_shader_runner *runner,
        const VkMemoryRequirements *memory_reqs, VkMemoryPropertyFlags flags)
{
    VkMemoryAllocateInfo alloc_info = {.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO};
    VkDeviceMemory vk_memory;
    VkResult vr;

    alloc_info.allocationSize = memory_reqs->size;
    alloc_info.memoryTypeIndex = select_vulkan_memory_type(runner,
            memory_reqs->memoryTypeBits, flags);
    vr = VK_CALL(vkAllocateMemory(runner->device, &alloc_info, NULL, &vk_memory));
    ok(vr == VK_SUCCESS, "Got unexpected VkResult %d.\n", vr);

    return vk_memory;
}

static VkBuffer create_buffer(const struct vulkan_shader_runner *runner, VkDeviceSize size,
        VkBufferUsageFlags usage, VkMemoryPropertyFlags memory_flags, VkDeviceMemory *memory)
{
    VkBufferCreateInfo buffer_info = {.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO};
    VkMemoryRequirements memory_reqs;
    VkBuffer buffer;

    buffer_info.size = size;
    buffer_info.usage = usage;
    buffer_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

    VK_CALL(vkCreateBuffer(runner->device, &buffer_info, NULL, &buffer));
    VK_CALL(vkGetBufferMemoryRequirements(runner->device, buffer, &memory_reqs));
    *memory = allocate_memory(runner, &memory_reqs, memory_flags);
    VK_CALL(vkBindBufferMemory(runner->device, buffer, *memory, 0));

    return buffer;
}

static VkBufferView create_buffer_view(const struct vulkan_shader_runner *runner, VkBuffer buffer,
        VkFormat format)
{
    VkBufferViewCreateInfo view_info = {.sType = VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO};
    VkBufferView view;

    view_info.buffer = buffer;
    view_info.format = format;
    view_info.range = VK_WHOLE_SIZE;

    VK_CALL(vkCreateBufferView(runner->device, &view_info, NULL, &view));

    return view;
}

static VkImage create_2d_image(const struct vulkan_shader_runner *runner, uint32_t width, uint32_t height,
        uint32_t level_count, VkImageUsageFlags usage, VkFormat format, VkDeviceMemory *memory)
{
    VkImageCreateInfo image_info = {.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO};
    VkMemoryRequirements memory_reqs;
    VkImage image;

    image_info.imageType = VK_IMAGE_TYPE_2D;
    image_info.format = format;
    image_info.extent.width = width;
    image_info.extent.height = height;
    image_info.extent.depth = 1;
    image_info.mipLevels = level_count;
    image_info.arrayLayers = 1;
    image_info.samples = VK_SAMPLE_COUNT_1_BIT;
    image_info.tiling = VK_IMAGE_TILING_OPTIMAL;
    image_info.usage = usage;
    image_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    image_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    VK_CALL(vkCreateImage(runner->device, &image_info, NULL, &image));

    VK_CALL(vkGetImageMemoryRequirements(runner->device, image, &memory_reqs));
    *memory = allocate_memory(runner, &memory_reqs, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
    VK_CALL(vkBindImageMemory(runner->device, image, *memory, 0));

    return image;
}

static VkImageView create_2d_image_view(const struct vulkan_shader_runner *runner, VkImage image, VkFormat format,
        VkImageAspectFlags aspect_mask)
{
    VkImageViewCreateInfo view_info = {.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO};
    VkImageView view;

    view_info.image = image;
    view_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
    view_info.format = format;
    view_info.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
    view_info.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
    view_info.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
    view_info.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
    view_info.subresourceRange.aspectMask = aspect_mask;
    view_info.subresourceRange.baseMipLevel = 0;
    view_info.subresourceRange.levelCount = VK_REMAINING_MIP_LEVELS;
    view_info.subresourceRange.baseArrayLayer = 0;
    view_info.subresourceRange.layerCount = 1;

    VK_CALL(vkCreateImageView(runner->device, &view_info, NULL, &view));
    return view;
}

static void resource_init_2d(struct vulkan_shader_runner *runner, struct vulkan_resource *resource,
        const struct resource_params *params)
{
    VkImageUsageFlagBits usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT;
    VkImageLayout layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    VkFormat format = vkd3d_get_vk_format(params->format);
    VkDevice device = runner->device;
    unsigned int buffer_offset = 0;
    VkDeviceMemory staging_memory;
    VkBuffer staging_buffer;
    void *data;

    if (params->type == RESOURCE_TYPE_UAV)
    {
        layout = VK_IMAGE_LAYOUT_GENERAL;
        usage |= VK_IMAGE_USAGE_STORAGE_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
    }

    resource->image = create_2d_image(runner, params->width, params->height, params->level_count,
            usage, format, &resource->memory);
    resource->image_view = create_2d_image_view(runner, resource->image, format, VK_IMAGE_ASPECT_COLOR_BIT);

    if (!params->data)
    {
        begin_command_buffer(runner);
        transition_image_layout(runner, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
                VK_IMAGE_LAYOUT_UNDEFINED, layout);
        end_command_buffer(runner);
        return;
    }

    staging_buffer = create_buffer(runner, params->data_size,
            VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &staging_memory);
    VK_CALL(vkMapMemory(device, staging_memory, 0, VK_WHOLE_SIZE, 0, &data));
    memcpy(data, params->data, params->data_size);
    VK_CALL(vkUnmapMemory(device, staging_memory));

    begin_command_buffer(runner);

    transition_image_layout(runner, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
            VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);

    for (unsigned int level = 0; level < params->level_count; ++level)
    {
        unsigned int level_width = get_level_dimension(params->width, level);
        unsigned int level_height = get_level_dimension(params->height, level);
        VkBufferImageCopy region = {0};

        region.bufferOffset = buffer_offset;
        region.imageSubresource.mipLevel = level;
        region.imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        region.imageSubresource.layerCount = 1;
        region.imageExtent.width = level_width;
        region.imageExtent.height = level_height;
        region.imageExtent.depth = 1;
        VK_CALL(vkCmdCopyBufferToImage(runner->cmd_buffer, staging_buffer, resource->image,
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region));

        buffer_offset += level_width * level_height * params->texel_size;
    }

    transition_image_layout(runner, resource->image, VK_IMAGE_ASPECT_COLOR_BIT, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, layout);

    end_command_buffer(runner);

    VK_CALL(vkFreeMemory(device, staging_memory, NULL));
    VK_CALL(vkDestroyBuffer(device, staging_buffer, NULL));
}

static void resource_init_buffer(struct vulkan_shader_runner *runner, struct vulkan_resource *resource,
        const struct resource_params *params)
{
    VkFormat format = vkd3d_get_vk_format(params->format);
    VkDevice device = runner->device;
    VkBufferUsageFlagBits usage;
    void *data;

    if (params->type == RESOURCE_TYPE_UAV)
        usage = VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT;
    else
        usage = VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT;

    /* d3d12 requires DXGI_FORMAT_UNKNOWN for structured buffers, but Vulkan requires a defined format. */
    if (format == VK_FORMAT_UNDEFINED && params->stride)
        format = VK_FORMAT_R32_UINT;

    resource->buffer = create_buffer(runner, params->data_size, usage,
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &resource->memory);
    resource->buffer_view = create_buffer_view(runner, resource->buffer, format);

    VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
    memcpy(data, params->data, params->data_size);
    VK_CALL(vkUnmapMemory(device, resource->memory));
}

static struct resource *vulkan_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    struct vulkan_resource *resource;
    VkDevice device = runner->device;
    VkFormat format;
    void *data;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            format = vkd3d_get_vk_format(params->format);

            resource->image = create_2d_image(runner, params->width, params->height, params->level_count,
                    VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, format, &resource->memory);
            resource->image_view = create_2d_image_view(runner, resource->image, format, VK_IMAGE_ASPECT_COLOR_BIT);

            begin_command_buffer(runner);
            transition_image_layout(runner, resource->image, VK_IMAGE_ASPECT_COLOR_BIT,
                    VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL);
            end_command_buffer(runner);
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            format = vkd3d_get_vk_format(params->format);

            resource->image = create_2d_image(runner, params->width, params->height, params->level_count,
                    VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, format,
                    &resource->memory);
            resource->image_view = create_2d_image_view(runner, resource->image, format, VK_IMAGE_ASPECT_DEPTH_BIT);

            begin_command_buffer(runner);
            transition_image_layout(runner, resource->image, VK_IMAGE_ASPECT_DEPTH_BIT,
                    VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL);
            end_command_buffer(runner);
            break;

        case RESOURCE_TYPE_TEXTURE:
        case RESOURCE_TYPE_UAV:
            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
                resource_init_buffer(runner, resource, params);
            else
                resource_init_2d(runner, resource, params);
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            resource->buffer = create_buffer(runner, params->data_size, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                    VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &resource->memory);

            VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
            memcpy(data, params->data, params->data_size);
            VK_CALL(vkUnmapMemory(device, resource->memory));
            break;
    }

    return &resource->r;
}

static void vulkan_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    struct vulkan_resource *resource = vulkan_resource(res);
    VkDevice device = runner->device;

    if (resource->memory)
        VK_CALL(vkFreeMemory(device, resource->memory, NULL));
    if (resource->image)
        VK_CALL(vkDestroyImage(device, resource->image, NULL));
    if (resource->image_view)
        VK_CALL(vkDestroyImageView(device, resource->image_view, NULL));
    if (resource->buffer)
        VK_CALL(vkDestroyBuffer(device, resource->buffer, NULL));
    if (resource->buffer_view)
        VK_CALL(vkDestroyBufferView(device, resource->buffer_view, NULL));

    free(resource);
}

static bool compile_shader(struct vulkan_shader_runner *runner, const char *source, const char *type,
        struct vkd3d_shader_code *dxbc, struct vkd3d_shader_code *spirv)
{
    struct vkd3d_shader_spirv_target_info spirv_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_SPIRV_TARGET_INFO};
    struct vkd3d_shader_interface_info interface_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_INTERFACE_INFO};
    struct vkd3d_shader_hlsl_source_info hlsl_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO};
    struct vkd3d_shader_compile_info info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_resource_binding bindings[MAX_RESOURCES + MAX_SAMPLERS];
    struct vkd3d_shader_push_constant_buffer push_constants;
    enum vkd3d_shader_spirv_extension spirv_extensions[2];
    struct vkd3d_shader_resource_binding *binding;
    struct vkd3d_shader_compile_option options[3];
    struct vkd3d_shader_compile_option *option;
    unsigned int i, compile_options;
    char profile[7];
    char *messages;
    int ret;

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_2_0] = "2_0",
        [SHADER_MODEL_3_0] = "3_0",
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
    };

    info.next = &hlsl_info;
    info.source.code = source;
    info.source.size = strlen(source);
    info.source_type = VKD3D_SHADER_SOURCE_HLSL;
    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
        info.target_type = VKD3D_SHADER_TARGET_D3D_BYTECODE;
    else
        info.target_type = VKD3D_SHADER_TARGET_DXBC_TPF;

    info.log_level = VKD3D_SHADER_LOG_WARNING;

    info.options = options;
    info.option_count = 0;

    option = &options[info.option_count++];
    option->name = VKD3D_SHADER_COMPILE_OPTION_API_VERSION;
    option->value = VKD3D_SHADER_API_VERSION_1_12;

    compile_options = runner->r.compile_options;
    if (compile_options)
    {

        if (compile_options & (D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR))
        {
            option = &options[info.option_count++];
            option->name = VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_ORDER;
            option->value = 0;
            if (compile_options & D3DCOMPILE_PACK_MATRIX_ROW_MAJOR)
                option->value |= VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_ROW_MAJOR;
            if (compile_options & D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR)
                option->value |= VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_COLUMN_MAJOR;

            compile_options &= ~(D3DCOMPILE_PACK_MATRIX_ROW_MAJOR | D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR);
        }

        /* FIXME: ignore compatibility flag for now */
        if (compile_options & D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY)
            compile_options &= ~D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY;

        if (compile_options)
            fatal_error("Unsupported compiler options %#x.\n", compile_options);
    }

    hlsl_info.entry_point = "main";
    sprintf(profile, "%s_%s", type, shader_models[runner->r.minimum_shader_model]);
    hlsl_info.profile = profile;

    ret = vkd3d_shader_compile(&info, dxbc, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    if (ret)
        return false;

    info.next = &spirv_info;
    info.source = *dxbc;
    if (runner->r.minimum_shader_model < SHADER_MODEL_4_0)
        info.source_type = VKD3D_SHADER_SOURCE_D3D_BYTECODE;
    else
        info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    info.target_type = VKD3D_SHADER_TARGET_SPIRV_BINARY;

    option = &options[info.option_count++];
    option->name = VKD3D_SHADER_COMPILE_OPTION_FEATURE;
    option->value = shader_runner_caps_get_feature_flags(&runner->caps);

    spirv_info.next = &interface_info;
    spirv_info.environment = VKD3D_SHADER_SPIRV_ENVIRONMENT_VULKAN_1_0;
    spirv_info.extensions = spirv_extensions;
    spirv_info.extension_count = 0;

    if (runner->caps.rov)
        spirv_extensions[spirv_info.extension_count++] = VKD3D_SHADER_SPIRV_EXTENSION_EXT_FRAGMENT_SHADER_INTERLOCK;
    if (runner->demote_to_helper_invocation)
        spirv_extensions[spirv_info.extension_count++] = VKD3D_SHADER_SPIRV_EXTENSION_EXT_DEMOTE_TO_HELPER_INVOCATION;

    push_constants.register_space = 0;
    push_constants.register_index = 0;
    push_constants.shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
    push_constants.offset = 0;
    push_constants.size = runner->r.uniform_count * sizeof(*runner->r.uniforms);

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;

            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                binding = &bindings[interface_info.binding_count++];
                if (resource->r.type == RESOURCE_TYPE_UAV)
                    binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_UAV;
                else
                    binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_SRV;
                binding->register_space = 0;
                binding->register_index = resource->r.slot;
                binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
                if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_BUFFER;
                else
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE;
                binding->binding.set = 0;
                binding->binding.binding = resource->binding;
                binding->binding.count = 1;
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        binding = &bindings[interface_info.binding_count++];
        binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_SAMPLER;
        binding->register_space = 0;
        binding->register_index = runner->r.samplers[i].slot;
        binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
        binding->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE;
        binding->binding.set = 0;
        binding->binding.binding = runner->samplers[i].binding;
        binding->binding.count = 1;
    }

    interface_info.bindings = bindings;

    interface_info.push_constant_buffer_count = 1;
    interface_info.push_constant_buffers = &push_constants;

    if (!strcmp(type, "vs"))
    {
        interface_info.next = &runner->vs_signatures;

        runner->vs_signatures.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_SIGNATURE_INFO;
        runner->vs_signatures.next = NULL;
    }

    ret = vkd3d_shader_compile(&info, spirv, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    if (ret)
        return false;

    return true;
}

static bool create_shader_stage(struct vulkan_shader_runner *runner,
        VkPipelineShaderStageCreateInfo *stage_info, const char *type, enum VkShaderStageFlagBits stage,
        const char *source, struct vkd3d_shader_code *dxbc_ptr)
{
    VkShaderModuleCreateInfo module_info = {.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO};
    struct vkd3d_shader_code spirv, dxbc;

    if (!dxbc_ptr)
        dxbc_ptr = &dxbc;

    if (!compile_shader(runner, source, type, dxbc_ptr, &spirv))
        return false;

    if (dxbc_ptr == &dxbc)
        vkd3d_shader_free_shader_code(&dxbc);

    memset(stage_info, 0, sizeof(*stage_info));
    stage_info->sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stage_info->stage = stage;
    stage_info->pName = "main";

    module_info.codeSize = spirv.size;
    module_info.pCode = spirv.code;

    VK_CALL(vkCreateShaderModule(runner->device, &module_info, NULL, &stage_info->module));
    vkd3d_shader_free_shader_code(&spirv);
    return true;
}

static VkPrimitiveTopology vulkan_primitive_topology_from_d3d(D3D_PRIMITIVE_TOPOLOGY topology)
{
    switch (topology)
    {
        default:
            fatal_error("Unhandled primitive topology %#x.\n", topology);
            /* fall through */
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST:
            return VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP:
            return VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP;
    }
}

static VkPipelineLayout create_pipeline_layout(const struct vulkan_shader_runner *runner,
        VkDescriptorSetLayout set_layout)
{
    VkPipelineLayoutCreateInfo layout_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO};
    VkPushConstantRange push_constant_range;
    VkPipelineLayout pipeline_layout;

    layout_desc.setLayoutCount = 1;
    layout_desc.pSetLayouts = &set_layout;

    if (runner->r.uniform_count)
    {
        layout_desc.pushConstantRangeCount = 1;
        layout_desc.pPushConstantRanges = &push_constant_range;

        push_constant_range.stageFlags = VK_SHADER_STAGE_ALL;
        push_constant_range.offset = 0;
        push_constant_range.size = runner->r.uniform_count * sizeof(*runner->r.uniforms);
    }

    VK_CALL(vkCreatePipelineLayout(runner->device, &layout_desc, NULL, &pipeline_layout));

    return pipeline_layout;
}

static enum VkCompareOp vk_compare_op_from_d3d12(D3D12_COMPARISON_FUNC op)
{
    switch (op)
    {
        case D3D12_COMPARISON_FUNC_NEVER:
            return VK_COMPARE_OP_NEVER;
        case D3D12_COMPARISON_FUNC_LESS:
            return VK_COMPARE_OP_LESS;
        case D3D12_COMPARISON_FUNC_EQUAL:
            return VK_COMPARE_OP_EQUAL;
        case D3D12_COMPARISON_FUNC_LESS_EQUAL:
            return VK_COMPARE_OP_LESS_OR_EQUAL;
        case D3D12_COMPARISON_FUNC_GREATER:
            return VK_COMPARE_OP_GREATER;
        case D3D12_COMPARISON_FUNC_NOT_EQUAL:
            return VK_COMPARE_OP_NOT_EQUAL;
        case D3D12_COMPARISON_FUNC_GREATER_EQUAL:
            return VK_COMPARE_OP_GREATER_OR_EQUAL;
        case D3D12_COMPARISON_FUNC_ALWAYS:
            return VK_COMPARE_OP_ALWAYS;
        default:
            fatal_error("Unhandled compare op %#x.\n", op);
    }
}

static VkPipeline create_graphics_pipeline(struct vulkan_shader_runner *runner, VkRenderPass render_pass,
        VkPipelineLayout pipeline_layout, D3D_PRIMITIVE_TOPOLOGY primitive_topology)
{
    VkPipelineInputAssemblyStateCreateInfo ia_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO};
    VkPipelineRasterizationStateCreateInfo rs_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO};
    VkPipelineVertexInputStateCreateInfo input_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO};
    VkPipelineColorBlendStateCreateInfo blend_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO};
    VkPipelineMultisampleStateCreateInfo ms_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO};
    static const VkViewport viewport = {.y = RENDER_TARGET_HEIGHT,
            .width = RENDER_TARGET_WIDTH, .height = -RENDER_TARGET_HEIGHT, .maxDepth = 1};
    VkPipelineViewportStateCreateInfo vp_desc = {.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO};
    static const VkRect2D rt_rect = {.extent.width = RENDER_TARGET_WIDTH, .extent.height = RENDER_TARGET_HEIGHT};
    VkGraphicsPipelineCreateInfo pipeline_desc = {.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO};
    VkPipelineColorBlendAttachmentState attachment_desc[MAX_RESOURCES] = {0};
    VkPipelineTessellationStateCreateInfo tessellation_info;
    VkVertexInputAttributeDescription input_attributes[32];
    VkPipelineDepthStencilStateCreateInfo ds_desc = {0};
    VkVertexInputBindingDescription input_bindings[32];
    VkPipelineShaderStageCreateInfo stage_desc[5];
    struct vkd3d_shader_code vs_dxbc;
    VkDevice device = runner->device;
    unsigned int stage_count = 0;
    VkPipeline pipeline;
    unsigned int i, j;
    VkResult vr;
    int ret;

    memset(stage_desc, 0, sizeof(stage_desc));
    ret = create_shader_stage(runner, &stage_desc[stage_count++], "vs",
            VK_SHADER_STAGE_VERTEX_BIT, runner->r.vs_source, &vs_dxbc);
    ret &= create_shader_stage(runner, &stage_desc[stage_count++], "ps",
            VK_SHADER_STAGE_FRAGMENT_BIT, runner->r.ps_source, NULL);

    if (runner->r.hs_source)
    {
        ret &= create_shader_stage(runner, &stage_desc[stage_count++], "hs",
                VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, runner->r.hs_source, NULL);
        ret &= create_shader_stage(runner, &stage_desc[stage_count++], "ds",
                VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, runner->r.ds_source, NULL);
    }

    if (runner->r.gs_source)
        ret &= create_shader_stage(runner, &stage_desc[stage_count++], "gs",
                VK_SHADER_STAGE_GEOMETRY_BIT, runner->r.gs_source, NULL);

    todo_if (runner->r.is_todo) ok(ret, "Failed to compile shaders.\n");
    if (!ret)
    {
        for (i = 0; i < ARRAY_SIZE(stage_desc); ++i)
            VK_CALL(vkDestroyShaderModule(device, stage_desc[i].module, NULL));
        return VK_NULL_HANDLE;
    }

    if (runner->r.input_element_count > ARRAY_SIZE(input_attributes))
        fatal_error("Input element count %zu is too high.\n", runner->r.input_element_count);

    for (i = 0; i < runner->r.input_element_count; ++i)
    {
        VkVertexInputAttributeDescription *attribute = &input_attributes[i];
        const struct input_element *element = &runner->r.input_elements[i];
        const struct vkd3d_shader_signature_element *signature_element;

        signature_element = vkd3d_shader_find_signature_element(&runner->vs_signatures.input, element->name, element->index, 0);
        ok(signature_element, "Cannot find signature element %s%u.\n", element->name, element->index);

        attribute->location = signature_element->register_index;
        attribute->binding = element->slot;
        attribute->format = vkd3d_get_vk_format(element->format);
        /* The offset will be filled below. */
    }

    input_desc.vertexAttributeDescriptionCount = runner->r.input_element_count;
    input_desc.pVertexAttributeDescriptions = input_attributes;
    input_desc.pVertexBindingDescriptions = input_bindings;

    blend_desc.attachmentCount = 0;
    blend_desc.pAttachments = attachment_desc;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
                attachment_desc[blend_desc.attachmentCount++].colorWriteMask =
                        VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT
                        | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
                break;

            case RESOURCE_TYPE_DEPTH_STENCIL:
                ds_desc.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
                ds_desc.pNext = NULL;
                ds_desc.flags = 0;
                ds_desc.depthTestEnable = VK_TRUE;
                ds_desc.depthWriteEnable = VK_TRUE;
                ds_desc.depthCompareOp = vk_compare_op_from_d3d12(runner->r.depth_func);
                ds_desc.depthBoundsTestEnable = VK_FALSE;
                ds_desc.stencilTestEnable = VK_FALSE;
                ds_desc.minDepthBounds = 0.0f;
                ds_desc.maxDepthBounds = 1.0f;
                pipeline_desc.pDepthStencilState = &ds_desc;
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
            {
                VkVertexInputBindingDescription *binding = &input_bindings[input_desc.vertexBindingDescriptionCount++];

                binding->binding = resource->r.slot;
                binding->stride = 0;
                binding->inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

                for (j = 0; j < runner->r.input_element_count; ++j)
                {
                    if (runner->r.input_elements[j].slot == resource->r.slot)
                    {
                        input_attributes[j].offset = binding->stride;
                        binding->stride += runner->r.input_elements[j].texel_size;
                    }
                }
                break;
            }
        }
    }

    ia_desc.topology = vulkan_primitive_topology_from_d3d(primitive_topology);

    vp_desc.viewportCount = 1;
    vp_desc.pViewports = &viewport;
    vp_desc.scissorCount = 1;
    vp_desc.pScissors = &rt_rect;

    rs_desc.cullMode = VK_CULL_MODE_NONE;
    rs_desc.frontFace = VK_FRONT_FACE_CLOCKWISE;
    rs_desc.lineWidth = 1.0f;

    /* TODO: sample count and mask. */
    ms_desc.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    pipeline_desc.stageCount = stage_count;
    pipeline_desc.pStages = stage_desc;
    pipeline_desc.pVertexInputState = &input_desc;
    pipeline_desc.pInputAssemblyState = &ia_desc;
    pipeline_desc.pViewportState = &vp_desc;
    pipeline_desc.pRasterizationState = &rs_desc;
    pipeline_desc.pMultisampleState = &ms_desc;
    pipeline_desc.pColorBlendState = &blend_desc;
    pipeline_desc.layout = pipeline_layout;
    pipeline_desc.renderPass = render_pass;
    pipeline_desc.subpass = 0;

    if (runner->r.hs_source)
    {
        tessellation_info.sType = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO;
        tessellation_info.pNext = NULL;
        tessellation_info.flags = 0;
        tessellation_info.patchControlPoints
                = max(primitive_topology - D3D_PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST + 1, 1);
        pipeline_desc.pTessellationState = &tessellation_info;
    }

    vr = VK_CALL(vkCreateGraphicsPipelines(runner->device, VK_NULL_HANDLE, 1, &pipeline_desc, NULL, &pipeline));
    ok(vr == VK_SUCCESS, "Failed to create graphics pipeline, vr %d.\n", vr);

    for (i = 0; i < ARRAY_SIZE(stage_desc); ++i)
        VK_CALL(vkDestroyShaderModule(device, stage_desc[i].module, NULL));
    vkd3d_shader_free_scan_signature_info(&runner->vs_signatures);
    vkd3d_shader_free_shader_code(&vs_dxbc);

    return pipeline;
}

static VkPipeline create_compute_pipeline(struct vulkan_shader_runner *runner, VkPipelineLayout pipeline_layout)
{
    VkComputePipelineCreateInfo pipeline_desc = {.sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO};
    VkPipeline pipeline;
    bool ret;

    ret = create_shader_stage(runner, &pipeline_desc.stage, "cs",
            VK_SHADER_STAGE_COMPUTE_BIT, runner->r.cs_source, NULL);
    todo_if (runner->r.is_todo) ok(ret, "Failed to compile shader.\n");
    if (!ret)
        return VK_NULL_HANDLE;

    pipeline_desc.layout = pipeline_layout;

    VK_CALL(vkCreateComputePipelines(runner->device, VK_NULL_HANDLE, 1, &pipeline_desc, NULL, &pipeline));

    VK_CALL(vkDestroyShaderModule(runner->device, pipeline_desc.stage.module, NULL));

    return pipeline;
}

static VkSamplerAddressMode vk_address_mode_from_d3d12(D3D12_TEXTURE_ADDRESS_MODE mode)
{
    switch (mode)
    {
        case D3D12_TEXTURE_ADDRESS_MODE_WRAP:
            return VK_SAMPLER_ADDRESS_MODE_REPEAT;
        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR:
            return VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
        case D3D12_TEXTURE_ADDRESS_MODE_CLAMP:
            return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
        case D3D12_TEXTURE_ADDRESS_MODE_BORDER:
            return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
        default:
            fatal_error("Unhandled sampler address mode %#x.\n", mode);
            return VK_SAMPLER_ADDRESS_MODE_REPEAT;
    }
}

static VkDescriptorSetLayout create_descriptor_set_layout(struct vulkan_shader_runner *runner)
{
    VkDescriptorSetLayoutCreateInfo set_desc = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO};
    VkDescriptorSetLayoutBinding bindings[MAX_RESOURCES + MAX_SAMPLERS];
    VkDescriptorSetLayoutBinding *binding;
    VkDescriptorSetLayout set_layout;
    uint32_t binding_index = 0;
    size_t i;

    if (runner->r.resource_count > ARRAY_SIZE(bindings))
        fatal_error("Resource count %zu is too high.\n", runner->r.resource_count);

    set_desc.pBindings = bindings;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
                break;

            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                binding = &bindings[set_desc.bindingCount++];

                resource->binding = binding_index++;

                binding->binding = resource->binding;
                if (resource->r.type == RESOURCE_TYPE_UAV)
                {
                    if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;
                    else
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
                }
                else
                {
                    if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
                    else
                        binding->descriptorType = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
                }
                binding->descriptorCount = 1;
                binding->stageFlags = VK_SHADER_STAGE_ALL;
                binding->pImmutableSamplers = NULL;
                break;
        }
    }

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        VkSamplerCreateInfo sampler_desc = {.sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO};
        struct vulkan_sampler *vulkan_sampler = &runner->samplers[i];
        const struct sampler *sampler = &runner->r.samplers[i];

        sampler_desc.magFilter = (sampler->filter & 0x4) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST;
        sampler_desc.minFilter = (sampler->filter & 0x1) ? VK_FILTER_LINEAR : VK_FILTER_NEAREST;
        sampler_desc.mipmapMode = (sampler->filter & 0x10) ? VK_SAMPLER_MIPMAP_MODE_LINEAR : VK_SAMPLER_MIPMAP_MODE_NEAREST;
        sampler_desc.addressModeU = vk_address_mode_from_d3d12(sampler->u_address);
        sampler_desc.addressModeV = vk_address_mode_from_d3d12(sampler->v_address);
        sampler_desc.addressModeW = vk_address_mode_from_d3d12(sampler->w_address);
        sampler_desc.compareEnable = !!sampler->func;
        sampler_desc.compareOp = sampler->func ? vk_compare_op_from_d3d12(sampler->func) : 0;
        sampler_desc.maxLod = FLT_MAX;

        VK_CALL(vkCreateSampler(runner->device, &sampler_desc, NULL, &vulkan_sampler->vk_sampler));
        vulkan_sampler->binding = binding_index++;

        binding = &bindings[set_desc.bindingCount++];

        binding->binding = vulkan_sampler->binding;
        binding->descriptorType = VK_DESCRIPTOR_TYPE_SAMPLER;
        binding->descriptorCount = 1;
        binding->stageFlags = VK_SHADER_STAGE_ALL;
        binding->pImmutableSamplers = &vulkan_sampler->vk_sampler;
    }

    VK_CALL(vkCreateDescriptorSetLayout(runner->device, &set_desc, NULL, &set_layout));

    return set_layout;
}

static void bind_resources(struct vulkan_shader_runner *runner, VkPipelineBindPoint bind_point,
        VkDescriptorSetLayout set_layout, VkPipelineLayout pipeline_layout)
{
    VkDescriptorSetAllocateInfo set_desc = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
    VkCommandBuffer cmd_buffer = runner->cmd_buffer;
    VkDescriptorSet descriptor_set;
    unsigned int i;

    set_desc.descriptorPool = runner->descriptor_pool;
    set_desc.descriptorSetCount = 1;
    set_desc.pSetLayouts = &set_layout;

    VK_CALL(vkAllocateDescriptorSets(runner->device, &set_desc, &descriptor_set));

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);
        VkWriteDescriptorSet write = {.sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET};
        static const VkDeviceSize zero_offset;
        VkDescriptorImageInfo image_info;

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_TEXTURE:
            case RESOURCE_TYPE_UAV:
                if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                {
                    write.dstSet = descriptor_set;
                    write.dstBinding = resource->binding;
                    write.dstArrayElement = 0;
                    write.descriptorCount = 1;
                    write.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
                    write.pTexelBufferView = &resource->buffer_view;

                    if (resource->r.type == RESOURCE_TYPE_UAV)
                        write.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;

                    VK_CALL(vkUpdateDescriptorSets(runner->device, 1, &write, 0, NULL));
                }
                else
                {
                    image_info.imageView = resource->image_view;
                    image_info.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

                    write.dstSet = descriptor_set;
                    write.dstBinding = resource->binding;
                    write.dstArrayElement = 0;
                    write.descriptorCount = 1;
                    write.descriptorType = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
                    write.pImageInfo = &image_info;

                    if (resource->r.type == RESOURCE_TYPE_UAV)
                    {
                        image_info.imageLayout = VK_IMAGE_LAYOUT_GENERAL;
                        write.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
                    }

                    VK_CALL(vkUpdateDescriptorSets(runner->device, 1, &write, 0, NULL));
                }
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
                if (bind_point == VK_PIPELINE_BIND_POINT_GRAPHICS)
                    VK_CALL(vkCmdBindVertexBuffers(cmd_buffer, resource->r.slot, 1, &resource->buffer, &zero_offset));
                break;

            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
                break;
        }
    }

    VK_CALL(vkCmdBindDescriptorSets(cmd_buffer, bind_point, pipeline_layout, 0, 1, &descriptor_set, 0, NULL));

    if (runner->r.uniform_count)
        VK_CALL(vkCmdPushConstants(cmd_buffer, pipeline_layout, VK_SHADER_STAGE_ALL, 0,
                runner->r.uniform_count * sizeof(*runner->r.uniforms), runner->r.uniforms));

    /* The descriptor set will be freed by resetting the descriptor pool. */
}

static void create_render_pass_and_framebuffer(struct vulkan_shader_runner *runner,
        VkRenderPass *render_pass, VkFramebuffer *fb)
{
    VkRenderPassCreateInfo render_pass_desc = {.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO};
    VkFramebufferCreateInfo fb_desc = {.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO};
    VkAttachmentReference ds_ref = {0}, color_refs[MAX_RESOURCES] = {0};
    VkAttachmentDescription attachment_descs[MAX_RESOURCES] = {0};
    unsigned int i, color_ref_count = 0, view_count = 0;
    VkSubpassDescription subpass_desc = {0};
    VkImageView views[MAX_RESOURCES];
    VkImageLayout layout;
    bool is_ds;

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct vulkan_resource *resource = vulkan_resource(runner->r.resources[i]);
        VkAttachmentDescription *attachment_desc = &attachment_descs[view_count];
        VkAttachmentReference *color_ref = &color_refs[color_ref_count];

        if (resource->r.type != RESOURCE_TYPE_RENDER_TARGET && resource->r.type != RESOURCE_TYPE_DEPTH_STENCIL)
            continue;

        is_ds = resource->r.type == RESOURCE_TYPE_DEPTH_STENCIL;
        layout = is_ds ? VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL : VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

        attachment_desc->format = vkd3d_get_vk_format(resource->r.format);
        attachment_desc->samples = VK_SAMPLE_COUNT_1_BIT;
        attachment_desc->loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
        attachment_desc->storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachment_desc->stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        attachment_desc->stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        attachment_desc->initialLayout = layout;
        attachment_desc->finalLayout = layout;

        if (is_ds)
        {
            ds_ref.attachment = view_count;
            ds_ref.layout = layout;
            subpass_desc.pDepthStencilAttachment = &ds_ref;
        }
        else
        {
            color_ref->attachment = view_count;
            color_ref->layout = layout;
            ++color_ref_count;
        }

        views[view_count++] = resource->image_view;
    }

    subpass_desc.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass_desc.colorAttachmentCount = color_ref_count;
    subpass_desc.pColorAttachments = color_refs;

    render_pass_desc.attachmentCount = view_count;
    render_pass_desc.pAttachments = attachment_descs;
    render_pass_desc.subpassCount = 1;
    render_pass_desc.pSubpasses = &subpass_desc;

    VK_CALL(vkCreateRenderPass(runner->device, &render_pass_desc, NULL, render_pass));

    fb_desc.renderPass = *render_pass;
    fb_desc.attachmentCount = view_count;
    fb_desc.pAttachments = views;
    fb_desc.width = RENDER_TARGET_WIDTH;
    fb_desc.height = RENDER_TARGET_HEIGHT;
    fb_desc.layers = 1;

    VK_CALL(vkCreateFramebuffer(runner->device, &fb_desc, NULL, fb));
}

static bool vulkan_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);

    VkCommandBuffer cmd_buffer = runner->cmd_buffer;
    VkDescriptorSetLayout set_layout;
    VkPipelineLayout pipeline_layout;
    VkDevice device = runner->device;
    VkPipeline pipeline;
    bool ret = false;
    unsigned int i;

    /* Create this before compiling shaders, it will assign resource bindings. */
    set_layout = create_descriptor_set_layout(runner);

    pipeline_layout = create_pipeline_layout(runner, set_layout);
    if (!(pipeline = create_compute_pipeline(runner, pipeline_layout)))
        goto out;

    begin_command_buffer(runner);

    VK_CALL(vkCmdBindPipeline(cmd_buffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline));

    bind_resources(runner, VK_PIPELINE_BIND_POINT_COMPUTE, set_layout, pipeline_layout);

    VK_CALL(vkCmdDispatch(cmd_buffer, x, y, z));

    end_command_buffer(runner);

    VK_CALL(vkDestroyPipeline(device, pipeline, NULL));
    VK_CALL(vkResetDescriptorPool(device, runner->descriptor_pool, 0));

    ret = true;
out:
    for (i = 0; i < runner->r.sampler_count; ++i)
        VK_CALL(vkDestroySampler(device, runner->samplers[i].vk_sampler, NULL));

    VK_CALL(vkDestroyPipelineLayout(device, pipeline_layout, NULL));
    VK_CALL(vkDestroyDescriptorSetLayout(device, set_layout, NULL));

    return ret;
}

static void vulkan_runner_clear(struct shader_runner *r, struct resource *res, const struct vec4 *clear_value)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    struct vulkan_resource *resource = vulkan_resource(res);

    size_t width = resource->r.width, height = resource->r.height;
    VkSubpassDescription sub_pass_desc = {0};
    VkAttachmentDescription attachment_desc;
    VkRenderPassCreateInfo pass_desc = {0};
    VkAttachmentReference attachment_ref;
    VkDevice device = runner->device;
    VkRenderPassBeginInfo begin_desc;
    VkFramebufferCreateInfo fb_desc;
    VkClearValue vk_clear_value;
    VkRenderPass render_pass;
    VkFramebuffer fb;

    attachment_desc.flags = 0;
    attachment_desc.format = vkd3d_get_vk_format(resource->r.format);
    attachment_desc.samples = VK_SAMPLE_COUNT_1_BIT;
    attachment_desc.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachment_desc.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
    /* TODO: formats with a stencil component would a clear op here. */
    attachment_desc.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachment_desc.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;

    sub_pass_desc.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;

    switch (resource->r.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            attachment_desc.initialLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
            sub_pass_desc.colorAttachmentCount = 1;
            sub_pass_desc.pColorAttachments = &attachment_ref;
            memcpy(vk_clear_value.color.float32, clear_value, sizeof(vk_clear_value.color.float32));
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            attachment_desc.initialLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
            sub_pass_desc.pDepthStencilAttachment = &attachment_ref;
            vk_clear_value.depthStencil.depth = clear_value->x;
            vk_clear_value.depthStencil.stencil = 0;
            break;

        default:
            fatal_error("Clears are not implemented for resource type %u.\n", resource->r.type);
    }

    attachment_desc.finalLayout = attachment_desc.initialLayout;

    attachment_ref.attachment = 0;
    attachment_ref.layout = attachment_desc.initialLayout;

    begin_command_buffer(runner);

    pass_desc.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    pass_desc.attachmentCount = 1;
    pass_desc.pAttachments = &attachment_desc;
    pass_desc.subpassCount = 1;
    pass_desc.pSubpasses = &sub_pass_desc;
    VK_CALL(vkCreateRenderPass(device, &pass_desc, NULL, &render_pass));

    fb_desc.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    fb_desc.pNext = NULL;
    fb_desc.flags = 0;
    fb_desc.renderPass = render_pass;
    fb_desc.attachmentCount = 1;
    fb_desc.pAttachments = &resource->image_view;
    fb_desc.width = width;
    fb_desc.height = height;
    fb_desc.layers = 1;
    VK_CALL(vkCreateFramebuffer(device, &fb_desc, NULL, &fb));

    begin_desc.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    begin_desc.pNext = NULL;
    begin_desc.renderPass = render_pass;
    begin_desc.framebuffer = fb;
    begin_desc.clearValueCount = 1;
    begin_desc.pClearValues = &vk_clear_value;
    begin_desc.renderArea.offset.x = 0;
    begin_desc.renderArea.offset.y = 0;
    begin_desc.renderArea.extent.width = width;
    begin_desc.renderArea.extent.height = height;
    VK_CALL(vkCmdBeginRenderPass(runner->cmd_buffer, &begin_desc, VK_SUBPASS_CONTENTS_INLINE));
    VK_CALL(vkCmdEndRenderPass(runner->cmd_buffer));

    end_command_buffer(runner);

    VK_CALL(vkDestroyRenderPass(device, render_pass, NULL));
    VK_CALL(vkDestroyFramebuffer(device, fb, NULL));
}

static bool vulkan_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count, unsigned int instance_count)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);

    static const VkRect2D rt_rect = {.extent.width = RENDER_TARGET_WIDTH, .extent.height = RENDER_TARGET_HEIGHT};
    VkRenderPassBeginInfo pass_begin_desc = {.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO};
    VkCommandBuffer cmd_buffer = runner->cmd_buffer;
    VkDescriptorSetLayout set_layout;
    VkPipelineLayout pipeline_layout;
    VkDevice device = runner->device;
    VkRenderPass render_pass;
    VkPipeline pipeline;
    VkFramebuffer fb;
    bool ret = false;
    unsigned int i;

    create_render_pass_and_framebuffer(runner, &render_pass, &fb);

    /* Create this before compiling shaders, it will assign resource bindings. */
    set_layout = create_descriptor_set_layout(runner);

    pipeline_layout = create_pipeline_layout(runner, set_layout);
    if (!(pipeline = create_graphics_pipeline(runner, render_pass, pipeline_layout, primitive_topology)))
        goto out;

    begin_command_buffer(runner);

    pass_begin_desc.renderPass = render_pass;
    pass_begin_desc.framebuffer = fb;
    pass_begin_desc.renderArea = rt_rect;

    VK_CALL(vkCmdBeginRenderPass(cmd_buffer, &pass_begin_desc, VK_SUBPASS_CONTENTS_INLINE));

    VK_CALL(vkCmdBindPipeline(cmd_buffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline));

    bind_resources(runner, VK_PIPELINE_BIND_POINT_GRAPHICS, set_layout, pipeline_layout);

    VK_CALL(vkCmdDraw(cmd_buffer, vertex_count, instance_count, 0, 0));

    VK_CALL(vkCmdEndRenderPass(cmd_buffer));
    end_command_buffer(runner);

    VK_CALL(vkDestroyPipeline(device, pipeline, NULL));
    VK_CALL(vkResetDescriptorPool(device, runner->descriptor_pool, 0));

    ret = true;
out:
    for (i = 0; i < runner->r.sampler_count; ++i)
        VK_CALL(vkDestroySampler(device, runner->samplers[i].vk_sampler, NULL));

    VK_CALL(vkDestroyPipelineLayout(device, pipeline_layout, NULL));
    VK_CALL(vkDestroyDescriptorSetLayout(device, set_layout, NULL));
    VK_CALL(vkDestroyRenderPass(device, render_pass, NULL));
    VK_CALL(vkDestroyFramebuffer(device, fb, NULL));

    return ret;
}

struct vulkan_resource_readback
{
    struct resource_readback rb;
    VkDeviceMemory memory;
    VkBuffer buffer;
};

static struct resource_readback *vulkan_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    struct vulkan_resource_readback *rb = malloc(sizeof(*rb));
    struct vulkan_resource *resource = vulkan_resource(res);
    VkDevice device = runner->device;
    VkImageAspectFlags aspect_mask;
    VkBufferImageCopy region = {0};
    VkImageLayout layout;

    rb->rb.width = resource->r.width;
    rb->rb.height = resource->r.height;
    rb->rb.depth = 1;

    rb->rb.row_pitch = rb->rb.width * resource->r.texel_size;

    rb->buffer = create_buffer(runner, rb->rb.row_pitch * rb->rb.height,
            VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, &rb->memory);

    if (resource->r.type == RESOURCE_TYPE_UAV && resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
    {
        void *data;

        VK_CALL(vkMapMemory(device, resource->memory, 0, VK_WHOLE_SIZE, 0, &data));
        VK_CALL(vkMapMemory(device, rb->memory, 0, VK_WHOLE_SIZE, 0, &rb->rb.data));
        memcpy(rb->rb.data, data, rb->rb.row_pitch * rb->rb.height);
        VK_CALL(vkUnmapMemory(device, resource->memory));
    }
    else
    {
        aspect_mask = (resource->r.type == RESOURCE_TYPE_DEPTH_STENCIL)
                ? VK_IMAGE_ASPECT_DEPTH_BIT : VK_IMAGE_ASPECT_COLOR_BIT;

        if (resource->r.type == RESOURCE_TYPE_RENDER_TARGET)
            layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        else if (resource->r.type == RESOURCE_TYPE_DEPTH_STENCIL)
            layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        else
            layout = VK_IMAGE_LAYOUT_GENERAL;

        begin_command_buffer(runner);

        transition_image_layout(runner, resource->image, aspect_mask, layout, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);

        region.imageSubresource.aspectMask = aspect_mask;
        region.imageSubresource.layerCount = 1;
        region.imageExtent.width = resource->r.width;
        region.imageExtent.height = resource->r.height;
        region.imageExtent.depth = 1;

        VK_CALL(vkCmdCopyImageToBuffer(runner->cmd_buffer, resource->image,
                VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, rb->buffer, 1, &region));

        transition_image_layout(runner, resource->image, aspect_mask, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, layout);

        end_command_buffer(runner);

        VK_CALL(vkMapMemory(device, rb->memory, 0, VK_WHOLE_SIZE, 0, &rb->rb.data));
    }

    return &rb->rb;
}

static void vulkan_runner_release_readback(struct shader_runner *r, struct resource_readback *rb)
{
    struct vulkan_resource_readback *vulkan_rb = CONTAINING_RECORD(rb, struct vulkan_resource_readback, rb);
    struct vulkan_shader_runner *runner = vulkan_shader_runner(r);
    VkDevice device = runner->device;

    VK_CALL(vkUnmapMemory(device, vulkan_rb->memory));

    VK_CALL(vkFreeMemory(device, vulkan_rb->memory, NULL));
    VK_CALL(vkDestroyBuffer(device, vulkan_rb->buffer, NULL));
    free(vulkan_rb);
}

static const struct shader_runner_ops vulkan_runner_ops =
{
    .create_resource = vulkan_runner_create_resource,
    .destroy_resource = vulkan_runner_destroy_resource,
    .dispatch = vulkan_runner_dispatch,
    .clear = vulkan_runner_clear,
    .draw = vulkan_runner_draw,
    .get_resource_readback = vulkan_runner_get_resource_readback,
    .release_readback = vulkan_runner_release_readback,
};

static bool get_graphics_queue_index(const struct vulkan_shader_runner *runner, uint32_t *index)
{
    VkQueueFamilyProperties *queue_properties;
    uint32_t count, i;

    count = 0;
    VK_CALL(vkGetPhysicalDeviceQueueFamilyProperties(runner->phys_device, &count, NULL));
    queue_properties = malloc(count * sizeof(*queue_properties));
    VK_CALL(vkGetPhysicalDeviceQueueFamilyProperties(runner->phys_device, &count, queue_properties));

    for (i = 0; i < count; ++i)
    {
        if (queue_properties[i].queueFlags & VK_QUEUE_GRAPHICS_BIT)
        {
            free(queue_properties);
            *index = i;
            return true;
        }
    }

    free(queue_properties);
    return false;
}

static bool has_extension(const VkExtensionProperties *extensions, uint32_t count, const char *extension_name)
{
    uint32_t i;

    for (i = 0; i < count; ++i)
    {
        if (!strcmp(extensions[i].extensionName, extension_name))
            return true;
    }
    return false;
}

static void check_instance_extensions(struct vulkan_shader_runner *runner, struct extension_list *enabled_extensions)
{
    VkExtensionProperties *extensions;
    uint32_t count, i;

    static const char *instance_extensions[] =
    {
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
    };

    enabled_extensions->names = calloc(ARRAY_SIZE(instance_extensions), sizeof(*enabled_extensions->names));
    enabled_extensions->count = 0;

    VK_CALL(vkEnumerateInstanceExtensionProperties(NULL, &count, NULL));
    extensions = calloc(count, sizeof(*extensions));
    VK_CALL(vkEnumerateInstanceExtensionProperties(NULL, &count, extensions));

    for (i = 0; i < ARRAY_SIZE(instance_extensions); ++i)
    {
        const char *name = instance_extensions[i];

        if (has_extension(extensions, count, name))
            enabled_extensions->names[enabled_extensions->count++] = name;
    }

    free(extensions);
}

static bool check_device_extensions(struct vulkan_shader_runner *runner, struct extension_list *enabled_extensions)
{
    VkPhysicalDevice phys_device = runner->phys_device;
    VkExtensionProperties *extensions;
    uint32_t i, count;

    static const struct
    {
        const char *name;
        bool required;
    }
    device_extensions[] =
    {
        {VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME},
        {VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME},
        {VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME, true},
        {VK_KHR_MAINTENANCE1_EXTENSION_NAME, true},
    };

    enabled_extensions->names = calloc(ARRAY_SIZE(device_extensions), sizeof(*enabled_extensions->names));
    enabled_extensions->count = 0;

    VK_CALL(vkEnumerateDeviceExtensionProperties(phys_device, NULL, &count, NULL));
    extensions = calloc(count, sizeof(*extensions));
    VK_CALL(vkEnumerateDeviceExtensionProperties(phys_device, NULL, &count, extensions));

    for (i = 0; i < ARRAY_SIZE(device_extensions); ++i)
    {
        const char *name = device_extensions[i].name;

        if (has_extension(extensions, count, name))
        {
            enabled_extensions->names[enabled_extensions->count++] = name;
            if (!strcmp(name, VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME))
                runner->caps.rov = true;
            if (!strcmp(name, VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME))
                runner->demote_to_helper_invocation = true;
            continue;
        }

        if (device_extensions[i].required)
        {
            skip("The selected Vulkan device does not support %s.\n", name);
            free(enabled_extensions->names);
            free(extensions);
            return false;
        }
    }

    free(extensions);
    return true;
}

static void get_physical_device_info(struct vulkan_shader_runner *runner, struct physical_device_info *info)
{
    memset(info, 0, sizeof(*info));

    info->features2.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2;
    if (runner->caps.rov)
    {
        info->features2.pNext = &info->interlock_features;
        info->interlock_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT;
    }

    if (runner->demote_to_helper_invocation)
    {
        void *list = info->features2.pNext;

        info->features2.pNext = &info->demote_to_helper_invocation_features;
        info->demote_to_helper_invocation_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT;
        info->demote_to_helper_invocation_features.pNext = list;
    }

    if (runner->vkGetPhysicalDeviceFeatures2KHR)
        VK_CALL(vkGetPhysicalDeviceFeatures2KHR(runner->phys_device, &info->features2));
    else
        VK_CALL(vkGetPhysicalDeviceFeatures(runner->phys_device, &info->features2.features));
}

static bool init_vulkan_runner(struct vulkan_shader_runner *runner)
{
    VkDescriptorPoolCreateInfo descriptor_pool_desc = {.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO};
    VkCommandBufferAllocateInfo cmd_buffer_desc = {.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO};
    VkCommandPoolCreateInfo command_pool_desc = {.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO};
    VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT demote_to_helper_invocation_features;
    VkDeviceQueueCreateInfo queue_desc = {.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO};
    VkInstanceCreateInfo instance_desc = {.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
    VkDeviceCreateInfo device_desc = {.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO};
    VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT interlock_features;
    VkDescriptorPoolSize descriptor_pool_sizes[5];
    const VkPhysicalDeviceFeatures *ret_features;
    struct extension_list enabled_extensions;
    static const float queue_priority = 1.0f;
    struct physical_device_info device_info;
    VkPhysicalDeviceFeatures features;
    VkFormatProperties format_props;
    uint32_t count, graphics_index;
    VkDevice device;
    void *libvulkan;
    VkResult vr;

    if (!(libvulkan = vkd3d_dlopen(SONAME_LIBVULKAN)))
    {
        skip("Failed to load %s: %s.\n", SONAME_LIBVULKAN, vkd3d_dlerror());
        return false;
    }
    vkGetInstanceProcAddr = vkd3d_dlsym(libvulkan, "vkGetInstanceProcAddr");

    runner->vkCreateInstance = (void *)vkGetInstanceProcAddr(NULL, "vkCreateInstance");
    runner->vkEnumerateInstanceExtensionProperties = (void *)vkGetInstanceProcAddr(NULL,
            "vkEnumerateInstanceExtensionProperties");

    check_instance_extensions(runner, &enabled_extensions);
    instance_desc.ppEnabledExtensionNames = enabled_extensions.names;
    instance_desc.enabledExtensionCount = enabled_extensions.count;
    vr = VK_CALL(vkCreateInstance(&instance_desc, NULL, &runner->instance));
    free(enabled_extensions.names);
    if (vr < 0)
    {
        skip("Failed to create a Vulkan instance, vr %d.\n", vr);
        return false;
    }

#define VK_INSTANCE_PFN(name) runner->name = (void *)vkGetInstanceProcAddr(runner->instance, #name);
#include "vulkan_procs.h"

    count = 1;
    if ((vr = VK_CALL(vkEnumeratePhysicalDevices(runner->instance, &count, &runner->phys_device))) < 0)
    {
        skip("Failed to enumerate physical devices, vr %d.\n", vr);
        goto out_destroy_instance;
    }

    if (!count)
    {
        skip("No Vulkan devices are available.\n");
        goto out_destroy_instance;
    }

    if (!get_graphics_queue_index(runner, &graphics_index))
    {
        skip("The selected Vulkan device does not support graphics operations.\n");
        goto out_destroy_instance;
    }

    device_desc.pQueueCreateInfos = &queue_desc;
    device_desc.queueCreateInfoCount = 1;

    queue_desc.queueFamilyIndex = graphics_index;
    queue_desc.queueCount = 1;
    queue_desc.pQueuePriorities = &queue_priority;

    if (!check_device_extensions(runner, &enabled_extensions))
        goto out_destroy_instance;
    device_desc.ppEnabledExtensionNames = enabled_extensions.names;
    device_desc.enabledExtensionCount = enabled_extensions.count;

    VK_CALL(vkGetPhysicalDeviceFormatProperties(runner->phys_device, VK_FORMAT_R32G32B32A32_SFLOAT, &format_props));
    if (!(format_props.optimalTilingFeatures & VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT))
    {
        skip("The selected Vulkan device does not support R32G32B32A32_SFLOAT render targets.\n");
        goto out_destroy_instance;
    }

    runner->caps.runner = "Vulkan";
    get_physical_device_info(runner, &device_info);
    ret_features = &device_info.features2.features;

    device_desc.pEnabledFeatures = &features;
    memset(&features, 0, sizeof(features));

    /* FIXME: Probably make these optional. */

#define ENABLE_FEATURE(x) \
    if (!ret_features->x) \
    { \
        skip("The selected Vulkan device does not support " #x ".\n"); \
        goto out_destroy_instance; \
    } \
    features.x = VK_TRUE

    ENABLE_FEATURE(fragmentStoresAndAtomics);
    /* For SV_PrimitiveID/SpvBuiltInPrimitiveId in fragment shaders. */
    ENABLE_FEATURE(geometryShader);
    ENABLE_FEATURE(shaderImageGatherExtended);
    ENABLE_FEATURE(shaderStorageImageWriteWithoutFormat);

    if (ret_features->shaderFloat64)
    {
        features.shaderFloat64 = VK_TRUE;
        runner->caps.float64 = true;
    }

    if (ret_features->shaderInt64)
    {
        features.shaderInt64 = VK_TRUE;
        runner->caps.int64 = true;
    }

    if (device_info.interlock_features.fragmentShaderSampleInterlock
            && device_info.interlock_features.fragmentShaderPixelInterlock)
    {
        memset(&interlock_features, 0, sizeof(interlock_features));
        interlock_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT;
        interlock_features.pNext = (void *)device_desc.pNext;
        interlock_features.fragmentShaderSampleInterlock = VK_TRUE;
        interlock_features.fragmentShaderPixelInterlock = VK_TRUE;
        device_desc.pNext = &interlock_features;
    }
    else
    {
        runner->caps.rov = false;
    }

    if (device_info.demote_to_helper_invocation_features.shaderDemoteToHelperInvocation)
    {
        memset(&demote_to_helper_invocation_features, 0, sizeof(demote_to_helper_invocation_features));
        demote_to_helper_invocation_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT;
        demote_to_helper_invocation_features.pNext = (void *)device_desc.pNext;
        demote_to_helper_invocation_features.shaderDemoteToHelperInvocation = VK_TRUE;
        device_desc.pNext = &demote_to_helper_invocation_features;
    }
    else
    {
        runner->demote_to_helper_invocation = false;
    }

    vr = VK_CALL(vkCreateDevice(runner->phys_device, &device_desc, NULL, &device));
    free(enabled_extensions.names);
    if (vr)
    {
        skip("Failed to create device, vr %d.\n", vr);
        goto out_destroy_instance;
    }
    runner->device = device;

#define VK_DEVICE_PFN(name) runner->name = (void *)VK_CALL(vkGetDeviceProcAddr(device, #name));
#include "vulkan_procs.h"

    VK_CALL(vkGetDeviceQueue(device, graphics_index, 0, &runner->queue));

    command_pool_desc.flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    command_pool_desc.queueFamilyIndex = graphics_index;

    VK_CALL(vkCreateCommandPool(device, &command_pool_desc, NULL, &runner->command_pool));

    cmd_buffer_desc.commandPool = runner->command_pool;
    cmd_buffer_desc.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    cmd_buffer_desc.commandBufferCount = 1;

    VK_CALL(vkAllocateCommandBuffers(device, &cmd_buffer_desc, &runner->cmd_buffer));

    descriptor_pool_sizes[0].type = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
    descriptor_pool_sizes[0].descriptorCount = MAX_RESOURCES;
    descriptor_pool_sizes[1].type = VK_DESCRIPTOR_TYPE_SAMPLER;
    descriptor_pool_sizes[1].descriptorCount = MAX_SAMPLERS;
    descriptor_pool_sizes[2].type = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
    descriptor_pool_sizes[2].descriptorCount = MAX_RESOURCES;
    descriptor_pool_sizes[3].type = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;
    descriptor_pool_sizes[3].descriptorCount = MAX_RESOURCES;
    descriptor_pool_sizes[4].type = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
    descriptor_pool_sizes[4].descriptorCount = MAX_RESOURCES;

    descriptor_pool_desc.maxSets = 1;
    descriptor_pool_desc.poolSizeCount = ARRAY_SIZE(descriptor_pool_sizes);
    descriptor_pool_desc.pPoolSizes = descriptor_pool_sizes;

    VK_CALL(vkCreateDescriptorPool(device, &descriptor_pool_desc, NULL, &runner->descriptor_pool));

    return true;

out_destroy_instance:
    VK_CALL(vkDestroyInstance(runner->instance, NULL));
    return false;
};

static void cleanup_vulkan_runner(struct vulkan_shader_runner *runner)
{
    VkDevice device = runner->device;

    VK_CALL(vkDestroyDescriptorPool(device, runner->descriptor_pool, NULL));
    VK_CALL(vkFreeCommandBuffers(device, runner->command_pool, 1, &runner->cmd_buffer));
    VK_CALL(vkDestroyCommandPool(device, runner->command_pool, NULL));
    VK_CALL(vkDestroyDevice(device, NULL));
    VK_CALL(vkDestroyInstance(runner->instance, NULL));
}

void run_shader_tests_vulkan(void)
{
    struct vulkan_shader_runner runner = {0};

    if (!init_vulkan_runner(&runner))
        return;

    runner.caps.minimum_shader_model = SHADER_MODEL_2_0;
    runner.caps.maximum_shader_model = SHADER_MODEL_3_0;
    run_shader_tests(&runner.r, &runner.caps, &vulkan_runner_ops, NULL);

    runner.caps.minimum_shader_model = SHADER_MODEL_4_0;
    runner.caps.maximum_shader_model = SHADER_MODEL_5_1;
    run_shader_tests(&runner.r, &runner.caps, &vulkan_runner_ops, NULL);

    cleanup_vulkan_runner(&runner);
}

#endif
