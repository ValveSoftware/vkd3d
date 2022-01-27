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

#include "vkd3d_windows.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3d12.h"
#include "vkd3d_dxgiformat.h"
#include "utils.h"

enum shader_model
{
    SHADER_MODEL_4_0 = 0,
    SHADER_MODEL_4_1,
    SHADER_MODEL_5_0,
    SHADER_MODEL_5_1,
};

enum texture_data_type
{
    TEXTURE_DATA_FLOAT,
    TEXTURE_DATA_SINT,
    TEXTURE_DATA_UINT,
};

struct sampler
{
    unsigned int slot;

    D3D12_FILTER filter;
    D3D12_TEXTURE_ADDRESS_MODE u_address, v_address, w_address;
};

struct texture
{
    unsigned int slot;

    DXGI_FORMAT format;
    enum texture_data_type data_type;
    unsigned int texel_size;
    unsigned int width, height;
    uint8_t *data;
    size_t data_size, data_capacity;

    D3D12_DESCRIPTOR_RANGE descriptor_range;
    ID3D12DescriptorHeap *heap;
    ID3D12Resource *resource;
    unsigned int root_index;
};

struct shader_context
{
    const struct shader_runner_ops *ops;

    ID3D10Blob *ps_code;
    enum shader_model minimum_shader_model;

    uint32_t *uniforms;
    size_t uniform_count;

    struct texture *textures;
    size_t texture_count;

    struct sampler *samplers;
    size_t sampler_count;
};

struct shader_runner_ops
{
    ID3D10Blob *(*compile_shader)(struct shader_context *context, const char *source, enum shader_model minimum_shader_model);
    void (*draw_quad)(struct shader_context *context);
    void (*probe_vec4)(struct shader_context *context, const RECT *rect, const struct vec4 *v, unsigned int ulps);
};

void run_shader_tests(struct shader_context *context, int argc, char **argv, const struct shader_runner_ops *ops);

void run_shader_tests_d3d12(int argc, char **argv);
