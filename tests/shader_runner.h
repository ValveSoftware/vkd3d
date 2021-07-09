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

#include "vkd3d_d3dcommon.h"
#include "utils.h"

struct shader_runner
{
    const struct shader_runner_ops *ops;
    void *private;
};

struct draw_params
{
    const void *ps_code;
    size_t ps_code_size;
    uint32_t *ps_uniforms;
    size_t ps_uniform_count;
};

struct shader_runner_ops
{
    const char *ps_profile;
    void (*draw_quad)(void *private, const struct draw_params *params);
    void (*probe_vec4)(void *private, const RECT *rect, const struct vec4 *v, unsigned int ulps);
};

void run_shader_tests(int argc, char **argv, const struct shader_runner_ops *ops, void *private);
