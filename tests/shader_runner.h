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

#include <stdint.h>
#include "vkd3d_windows.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3d12.h"
#include "vkd3d_dxgiformat.h"
#include "vkd3d_common.h"
#include "shader_runner_library.h"
#include "utils.h"

#define RENDER_TARGET_WIDTH 640
#define RENDER_TARGET_HEIGHT 480

enum shader_model
{
    SHADER_MODEL_2_0,
    SHADER_MODEL_4_0,
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

enum resource_type
{
    RESOURCE_TYPE_RENDER_TARGET,
    RESOURCE_TYPE_TEXTURE,
    RESOURCE_TYPE_UAV,
    RESOURCE_TYPE_VERTEX_BUFFER,
};

struct resource_params
{
    unsigned int slot;
    enum resource_type type;

    DXGI_FORMAT format;
    enum texture_data_type data_type;
    unsigned int texel_size;
    unsigned int width, height;
    uint8_t *data;
    size_t data_size, data_capacity;
};

struct resource
{
    unsigned int slot;
    enum resource_type type;

    DXGI_FORMAT format;
    unsigned int size;
    unsigned int texel_size;
    unsigned int width, height;
};

struct input_element
{
    char *name;
    unsigned int slot;
    DXGI_FORMAT format;
    unsigned int texel_size;
    unsigned int index;
};

#define MAX_RESOURCES 32
#define MAX_SAMPLERS 32

struct shader_runner
{
    const struct shader_runner_ops *ops;

    bool is_todo;

    char *vs_source;
    char *ps_source;
    char *cs_source;
    enum shader_model minimum_shader_model;

    bool last_render_failed;

    uint32_t *uniforms;
    size_t uniform_count, uniform_capacity;

    struct resource *resources[MAX_RESOURCES];
    size_t resource_count;

    struct sampler samplers[MAX_SAMPLERS];
    size_t sampler_count;

    struct input_element *input_elements;
    size_t input_element_count, input_element_capacity;
};

struct shader_runner_ops
{
    /* Returns false if unable to run the given tests. If NULL, all tests are
     * run. */
    bool (*check_requirements)(struct shader_runner *runner);
    struct resource *(*create_resource)(struct shader_runner *runner, const struct resource_params *params);
    void (*destroy_resource)(struct shader_runner *runner, struct resource *resource);
    bool (*draw)(struct shader_runner *runner, D3D_PRIMITIVE_TOPOLOGY primitive_topology, unsigned int vertex_count);
    bool (*dispatch)(struct shader_runner *runner, unsigned int x, unsigned int y, unsigned int z);
    struct resource_readback *(*get_resource_readback)(struct shader_runner *runner, struct resource *resource);
    void (*release_readback)(struct shader_runner *runner, struct resource_readback *rb);
};

void fatal_error(const char *format, ...) VKD3D_NORETURN VKD3D_PRINTF_FUNC(1, 2);

unsigned int get_vb_stride(const struct shader_runner *runner, unsigned int slot);
void init_resource(struct resource *resource, const struct resource_params *params);

#ifdef _WIN32
void run_shader_tests_d3d9(shader_runner_frontend_func func, int argc, char **argv);
void run_shader_tests_d3d11(shader_runner_frontend_func func, int argc, char **argv);
#else
void run_shader_tests_vulkan(shader_runner_frontend_func func, int argc, char **argv);
#endif
void run_shader_tests_d3d12(shader_runner_frontend_func func, int argc, char **argv);
