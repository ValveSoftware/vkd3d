/*
 * Copyright 2022 Zebediah Figura for CodeWeavers
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

#ifndef __SHADER_RUNNER_LIBRARY_H
#define __SHADER_RUNNER_LIBRARY_H

#include <vkd3d_types.h>

#ifdef LIBSHADER_RUNNER_SOURCE
# define SHADER_RUNNER_API VKD3D_EXPORT
#else
# define SHADER_RUNNER_API VKD3D_IMPORT
#endif

struct shader_runner;
struct shader_runner_ops;

enum texture_data_type
{
    TEXTURE_DATA_FLOAT,
    TEXTURE_DATA_SINT,
    TEXTURE_DATA_UINT,
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

typedef void (*shader_runner_frontend_func)(struct shader_runner *runner,
        int argc, char **argv, const struct shader_runner_ops *ops);

SHADER_RUNNER_API void shader_runner_compile_cs(struct shader_runner *runner, const char *source, HRESULT expect_hr);
SHADER_RUNNER_API void shader_runner_compile_ps(struct shader_runner *runner, const char *source, HRESULT expect_hr);
SHADER_RUNNER_API void shader_runner_compile_vs(struct shader_runner *runner, const char *source, HRESULT expect_hr);
SHADER_RUNNER_API void shader_runner_create_resource(struct shader_runner *runner, const struct resource_params *params);
SHADER_RUNNER_API void shader_runner_run(shader_runner_frontend_func func, int argc, char **argv);

#endif  /* __SHADER_RUNNER_LIBRARY_H */
