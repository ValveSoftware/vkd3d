/*
 * Copyright 2016-2018 Józef Kucia for CodeWeavers
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

#ifndef __VKD3D_TEST_UTILS_H
#define __VKD3D_TEST_UTILS_H

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include "vkd3d_test.h"

struct vec2
{
    float x, y;
};

struct vec4
{
    float x, y, z, w;
};

struct dvec2
{
    double x, y;
};

struct ivec4
{
    int x, y, z, w;
};

struct uvec4
{
    unsigned int x, y, z, w;
};

struct i64vec2
{
    int64_t x, y;
};

struct u64vec2
{
    uint64_t x, y;
};

struct resource_readback
{
    uint64_t width;
    unsigned int height;
    unsigned int depth;
    uint64_t row_pitch;
    void *data;
};

static inline bool vkd3d_array_reserve(void **elements, size_t *capacity, size_t element_count, size_t element_size)
{
    size_t new_capacity, max_capacity;
    void *new_elements;

    if (element_count <= *capacity)
        return true;

    max_capacity = ~(size_t)0 / element_size;
    if (max_capacity < element_count)
        return false;

    new_capacity = max(*capacity, 4);
    while (new_capacity < element_count && new_capacity <= max_capacity / 2)
        new_capacity *= 2;

    if (new_capacity < element_count)
        new_capacity = element_count;

    if (!(new_elements = realloc(*elements, new_capacity * element_size)))
        return false;

    *elements = new_elements;
    *capacity = new_capacity;

    return true;
}

static bool compare_uint(unsigned int x, unsigned int y, unsigned int max_diff)
{
    unsigned int diff = x > y ? x - y : y - x;

    return diff <= max_diff;
}

static bool compare_color(DWORD c1, DWORD c2, BYTE max_diff)
{
    return compare_uint(c1 & 0xff, c2 & 0xff, max_diff)
           && compare_uint((c1 >> 8) & 0xff, (c2 >> 8) & 0xff, max_diff)
           && compare_uint((c1 >> 16) & 0xff, (c2 >> 16) & 0xff, max_diff)
           && compare_uint((c1 >> 24) & 0xff, (c2 >> 24) & 0xff, max_diff);
}

static bool compare_float(float f, float g, unsigned int ulps)
{
    int x, y;
    union
    {
        float f;
        int i;
    } u;

    u.f = f;
    x = u.i;
    u.f = g;
    y = u.i;

    if (x < 0)
        x = INT_MIN - x;
    if (y < 0)
        y = INT_MIN - y;

    return compare_uint(x, y, ulps);
}

static inline bool compare_uvec4(const struct uvec4 *v1, const struct uvec4 *v2)
{
    return v1->x == v2->x && v1->y == v2->y && v1->z == v2->z && v1->w == v2->w;
}

static inline bool compare_vec4(const struct vec4 *v1, const struct vec4 *v2, unsigned int ulps)
{
    return compare_float(v1->x, v2->x, ulps)
            && compare_float(v1->y, v2->y, ulps)
            && compare_float(v1->z, v2->z, ulps)
            && compare_float(v1->w, v2->w, ulps);
}

static inline void set_rect(RECT *rect, int left, int top, int right, int bottom)
{
    rect->left = left;
    rect->right = right;
    rect->top = top;
    rect->bottom = bottom;
}

static void *get_readback_data(const struct resource_readback *rb,
        unsigned int x, unsigned int y, unsigned int z, size_t element_size)
{
    unsigned int slice_pitch = rb->row_pitch * rb->height;
    return &((uint8_t *)rb->data)[slice_pitch * z + rb->row_pitch * y + x * element_size];
}

static float get_readback_float(const struct resource_readback *rb, unsigned int x, unsigned int y)
{
    return *(float *)get_readback_data(rb, x, y, 0, sizeof(float));
}

static unsigned int get_readback_uint(const struct resource_readback *rb, unsigned int x, unsigned int y, unsigned int z)
{
    return *(unsigned int*)get_readback_data(rb, x, y, z, sizeof(unsigned int));
}

static const struct vec4 *get_readback_vec4(const struct resource_readback *rb, unsigned int x, unsigned int y)
{
    return get_readback_data(rb, x, y, 0, sizeof(struct vec4));
}

static const struct uvec4 *get_readback_uvec4(const struct resource_readback *rb, unsigned int x, unsigned int y)
{
    return get_readback_data(rb, x, y, 0, sizeof(struct uvec4));
}

#define check_readback_data_float(a, b, c, d) check_readback_data_float_(__LINE__, a, b, c, d)
static inline void check_readback_data_float_(unsigned int line, const struct resource_readback *rb,
        const RECT *rect, float expected, unsigned int max_diff)
{
    RECT r = {0, 0, rb->width, rb->height};
    unsigned int x = 0, y;
    bool all_match = true;
    float got = 0;

    if (rect)
        r = *rect;

    for (y = r.top; y < r.bottom; ++y)
    {
        for (x = r.left; x < r.right; ++x)
        {
            got = get_readback_float(rb, x, y);
            if (!compare_float(got, expected, max_diff))
            {
                all_match = false;
                break;
            }
        }
        if (!all_match)
            break;
    }
    ok_(line)(all_match, "Got %.8e, expected %.8e at (%u, %u).\n", got, expected, x, y);
}

#define check_readback_data_uint(a, b, c, d) check_readback_data_uint_(__LINE__, a, b, c, d)
static inline void check_readback_data_uint_(unsigned int line, struct resource_readback *rb,
        const D3D12_BOX *box, unsigned int expected, unsigned int max_diff)
{
    D3D12_BOX b = {0, 0, 0, rb->width, rb->height, rb->depth};
    unsigned int x = 0, y = 0, z;
    bool all_match = true;
    unsigned int got = 0;

    if (box)
        b = *box;

    for (z = b.front; z < b.back; ++z)
    {
        for (y = b.top; y < b.bottom; ++y)
        {
            for (x = b.left; x < b.right; ++x)
            {
                got = get_readback_uint(rb, x, y, z);
                if (!compare_color(got, expected, max_diff))
                {
                    all_match = false;
                    break;
                }
            }
            if (!all_match)
                break;
        }
        if (!all_match)
            break;
    }
    ok_(line)(all_match, "Got 0x%08x, expected 0x%08x at (%u, %u, %u).\n", got, expected, x, y, z);
}

#define check_readback_data_vec4(a, b, c, d) check_readback_data_vec4_(__LINE__, a, b, c, d)
static inline void check_readback_data_vec4_(unsigned int line, const struct resource_readback *rb,
        const RECT *rect, const struct vec4 *expected, unsigned int max_diff)
{
    RECT r = {0, 0, rb->width, rb->height};
    unsigned int x = 0, y = 0;
    struct vec4 got = {0};
    bool all_match = true;

    if (rect)
        r = *rect;

    for (y = r.top; y < r.bottom; ++y)
    {
        for (x = r.left; x < r.right; ++x)
        {
            got = *get_readback_vec4(rb, x, y);
            if (!compare_vec4(&got, expected, max_diff))
            {
                all_match = false;
                break;
            }
        }
        if (!all_match)
            break;
    }
    ok_(line)(all_match, "Got {%.8e, %.8e, %.8e, %.8e}, expected {%.8e, %.8e, %.8e, %.8e} at (%u, %u).\n",
            got.x, got.y, got.z, got.w, expected->x, expected->y, expected->z, expected->w, x, y);
}

#define check_readback_data_ivec4(a, b, c) check_readback_data_uvec4_(__LINE__, a, b, (const struct uvec4 *)(c))
#define check_readback_data_uvec4(a, b, c) check_readback_data_uvec4_(__LINE__, a, b, c)
static inline void check_readback_data_uvec4_(unsigned int line, const struct resource_readback *rb,
        const RECT *rect, const struct uvec4 *expected)
{
    RECT r = {0, 0, rb->width, rb->height};
    unsigned int x = 0, y = 0;
    struct uvec4 got = {0};
    bool all_match = true;

    if (rect)
        r = *rect;

    for (y = r.top; y < r.bottom; ++y)
    {
        for (x = r.left; x < r.right; ++x)
        {
            got = *get_readback_uvec4(rb, x, y);
            if (!compare_uvec4(&got, expected))
            {
                all_match = false;
                break;
            }
        }
        if (!all_match)
            break;
    }
    ok_(line)(all_match, "Got {0x%08x, 0x%08x, 0x%08x, 0x%08x}, expected {0x%08x, 0x%08x, 0x%08x, 0x%08x} at (%u, %u).\n",
            got.x, got.y, got.z, got.w, expected->x, expected->y, expected->z, expected->w, x, y);
}

struct test_options
{
    bool use_warp_device;
    unsigned int adapter_idx;
    bool enable_debug_layer;
    bool enable_gpu_based_validation;
    const char *filename;
};

extern struct test_options test_options;

static inline void parse_args(int argc, char **argv)
{
    unsigned int i;

    for (i = 1; i < argc; ++i)
    {
        if (!strcmp(argv[i], "--warp"))
            test_options.use_warp_device = true;
        else if (!strcmp(argv[i], "--adapter") && i + 1 < argc)
            test_options.adapter_idx = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--validate"))
            test_options.enable_debug_layer = true;
        else if (!strcmp(argv[i], "--gbv"))
            test_options.enable_gpu_based_validation = true;
        else if (argv[i][0] != '-')
            test_options.filename = argv[i];
    }
}

#endif
