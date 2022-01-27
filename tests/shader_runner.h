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

enum shader_model
{
    SHADER_MODEL_4_0 = 0,
    SHADER_MODEL_4_1,
    SHADER_MODEL_5_0,
    SHADER_MODEL_5_1,
};

struct shader_runner_ops
{
    ID3D10Blob *(*compile_shader)(const char *source, enum shader_model minimum_shader_model);
};

void run_shader_tests(int argc, char **argv, const struct shader_runner_ops *ops);

void run_shader_tests_d3d12(int argc, char **argv);
