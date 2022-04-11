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

#ifndef VK_INSTANCE_PFN
# define VK_INSTANCE_PFN(x)
#endif

#ifndef VK_INSTANCE_EXT_PFN
# define VK_INSTANCE_EXT_PFN(x)
#endif

#ifndef VK_DEVICE_PFN
# define VK_DEVICE_PFN(x)
#endif

#ifndef VK_DEVICE_EXT_PFN
# define VK_DEVICE_EXT_PFN(x)
#endif

/* Instance functions (obtained by vkGetInstanceProcAddr). */
VK_INSTANCE_PFN(vkDestroyInstance) /* Load vkDestroyInstance() first. */
VK_INSTANCE_PFN(vkCreateDevice)
VK_INSTANCE_PFN(vkEnumerateDeviceExtensionProperties)
VK_INSTANCE_PFN(vkEnumeratePhysicalDevices)
VK_INSTANCE_PFN(vkGetDeviceProcAddr)
VK_INSTANCE_PFN(vkGetPhysicalDeviceFeatures)
VK_INSTANCE_PFN(vkGetPhysicalDeviceFormatProperties)
VK_INSTANCE_PFN(vkGetPhysicalDeviceMemoryProperties)
VK_INSTANCE_PFN(vkGetPhysicalDeviceQueueFamilyProperties)

/* Device functions (obtained by vkGetDeviceProcAddr). */
VK_DEVICE_PFN(vkDestroyDevice) /* Load vkDestroyDevice() first. */
VK_DEVICE_PFN(vkAllocateCommandBuffers)
VK_DEVICE_PFN(vkAllocateDescriptorSets)
VK_DEVICE_PFN(vkAllocateMemory)
VK_DEVICE_PFN(vkBeginCommandBuffer)
VK_DEVICE_PFN(vkBindBufferMemory)
VK_DEVICE_PFN(vkBindImageMemory)
VK_DEVICE_PFN(vkCmdBeginRenderPass)
VK_DEVICE_PFN(vkCmdBindDescriptorSets)
VK_DEVICE_PFN(vkCmdBindPipeline)
VK_DEVICE_PFN(vkCmdBindVertexBuffers)
VK_DEVICE_PFN(vkCmdClearAttachments)
VK_DEVICE_PFN(vkCmdCopyBufferToImage)
VK_DEVICE_PFN(vkCmdCopyImageToBuffer)
VK_DEVICE_PFN(vkCmdDraw)
VK_DEVICE_PFN(vkCmdEndRenderPass)
VK_DEVICE_PFN(vkCmdFillBuffer)
VK_DEVICE_PFN(vkCmdPipelineBarrier)
VK_DEVICE_PFN(vkCmdPushConstants)
VK_DEVICE_PFN(vkCreateBuffer)
VK_DEVICE_PFN(vkCreateCommandPool)
VK_DEVICE_PFN(vkCreateDescriptorPool)
VK_DEVICE_PFN(vkCreateDescriptorSetLayout)
VK_DEVICE_PFN(vkCreateFramebuffer)
VK_DEVICE_PFN(vkCreateGraphicsPipelines)
VK_DEVICE_PFN(vkCreateImage)
VK_DEVICE_PFN(vkCreateImageView)
VK_DEVICE_PFN(vkCreatePipelineLayout)
VK_DEVICE_PFN(vkCreateRenderPass)
VK_DEVICE_PFN(vkCreateSampler)
VK_DEVICE_PFN(vkCreateShaderModule)
VK_DEVICE_PFN(vkDestroyBuffer)
VK_DEVICE_PFN(vkDestroyCommandPool)
VK_DEVICE_PFN(vkDestroyDescriptorPool)
VK_DEVICE_PFN(vkDestroyDescriptorSetLayout)
VK_DEVICE_PFN(vkDestroyFramebuffer)
VK_DEVICE_PFN(vkDestroyImage)
VK_DEVICE_PFN(vkDestroyImageView)
VK_DEVICE_PFN(vkDestroyPipeline)
VK_DEVICE_PFN(vkDestroyPipelineLayout)
VK_DEVICE_PFN(vkDestroyRenderPass)
VK_DEVICE_PFN(vkDestroySampler)
VK_DEVICE_PFN(vkDestroyShaderModule)
VK_DEVICE_PFN(vkEndCommandBuffer)
VK_DEVICE_PFN(vkFreeCommandBuffers)
VK_DEVICE_PFN(vkFreeMemory)
VK_DEVICE_PFN(vkGetBufferMemoryRequirements)
VK_DEVICE_PFN(vkGetDeviceQueue)
VK_DEVICE_PFN(vkGetImageMemoryRequirements)
VK_DEVICE_PFN(vkMapMemory)
VK_DEVICE_PFN(vkQueueSubmit)
VK_DEVICE_PFN(vkQueueWaitIdle)
VK_DEVICE_PFN(vkResetDescriptorPool)
VK_DEVICE_PFN(vkUnmapMemory)
VK_DEVICE_PFN(vkUpdateDescriptorSets)

#undef VK_INSTANCE_PFN
#undef VK_INSTANCE_EXT_PFN
#undef VK_DEVICE_PFN
#undef VK_DEVICE_EXT_PFN
