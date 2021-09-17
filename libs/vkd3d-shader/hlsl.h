/*
 * Copyright 2012 Matteo Bruni for CodeWeavers
 * Copyright 2019-2020 Zebediah Figura for CodeWeavers
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

#ifndef __VKD3D_SHADER_HLSL_H
#define __VKD3D_SHADER_HLSL_H

#include "vkd3d_shader_private.h"
#include "rbtree.h"
#include "vkd3d_d3dcommon.h"
#include "vkd3d_d3dx9shader.h"
#include "sm4.h"

/* The general IR structure is inspired by Mesa GLSL hir, even though the code
 * ends up being quite different in practice. Anyway, here comes the relevant
 * licensing information.
 *
 * Copyright © 2010 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#define HLSL_SWIZZLE_X (0u)
#define HLSL_SWIZZLE_Y (1u)
#define HLSL_SWIZZLE_Z (2u)
#define HLSL_SWIZZLE_W (3u)

#define HLSL_SWIZZLE(x, y, z, w) \
        (((HLSL_SWIZZLE_ ## x) << 0) \
        | ((HLSL_SWIZZLE_ ## y) << 2) \
        | ((HLSL_SWIZZLE_ ## z) << 4) \
        | ((HLSL_SWIZZLE_ ## w) << 6))

enum hlsl_type_class
{
    HLSL_CLASS_SCALAR,
    HLSL_CLASS_VECTOR,
    HLSL_CLASS_MATRIX,
    HLSL_CLASS_LAST_NUMERIC = HLSL_CLASS_MATRIX,
    HLSL_CLASS_STRUCT,
    HLSL_CLASS_ARRAY,
    HLSL_CLASS_OBJECT,
};

enum hlsl_base_type
{
    HLSL_TYPE_FLOAT,
    HLSL_TYPE_HALF,
    HLSL_TYPE_DOUBLE,
    HLSL_TYPE_INT,
    HLSL_TYPE_UINT,
    HLSL_TYPE_BOOL,
    HLSL_TYPE_LAST_SCALAR = HLSL_TYPE_BOOL,
    HLSL_TYPE_SAMPLER,
    HLSL_TYPE_TEXTURE,
    HLSL_TYPE_PIXELSHADER,
    HLSL_TYPE_VERTEXSHADER,
    HLSL_TYPE_STRING,
    HLSL_TYPE_VOID,
};

enum hlsl_sampler_dim
{
   HLSL_SAMPLER_DIM_GENERIC,
   HLSL_SAMPLER_DIM_1D,
   HLSL_SAMPLER_DIM_2D,
   HLSL_SAMPLER_DIM_3D,
   HLSL_SAMPLER_DIM_CUBE,
   HLSL_SAMPLER_DIM_MAX = HLSL_SAMPLER_DIM_CUBE
};

enum hlsl_matrix_majority
{
    HLSL_COLUMN_MAJOR,
    HLSL_ROW_MAJOR
};

struct hlsl_type
{
    struct list entry;
    struct rb_entry scope_entry;
    enum hlsl_type_class type;
    enum hlsl_base_type base_type;
    enum hlsl_sampler_dim sampler_dim;
    const char *name;
    unsigned int modifiers;
    unsigned int dimx;
    unsigned int dimy;
    union
    {
        struct list *elements;
        struct
        {
            struct hlsl_type *type;
            unsigned int elements_count;
        } array;
    } e;

    unsigned int reg_size;
    size_t bytecode_offset;
};

struct hlsl_semantic
{
    const char *name;
    uint32_t index;
};

struct hlsl_struct_field
{
    struct list entry;
    struct vkd3d_shader_location loc;
    struct hlsl_type *type;
    const char *name;
    struct hlsl_semantic semantic;
    unsigned int reg_offset;

    size_t name_bytecode_offset;
};

struct hlsl_reg
{
    uint32_t id;
    unsigned int writemask;
    bool allocated;
};

enum hlsl_ir_node_type
{
    HLSL_IR_CONSTANT,
    HLSL_IR_EXPR,
    HLSL_IR_IF,
    HLSL_IR_LOAD,
    HLSL_IR_LOOP,
    HLSL_IR_JUMP,
    HLSL_IR_STORE,
    HLSL_IR_SWIZZLE,
};

struct hlsl_ir_node
{
    struct list entry;
    enum hlsl_ir_node_type type;
    struct hlsl_type *data_type;

    struct list uses;

    struct vkd3d_shader_location loc;

    /* Liveness ranges. "index" is the index of this instruction. Since this is
     * essentially an SSA value, the earliest live point is the index. This is
     * true even for loops, since currently we can't have a reference to a
     * value generated in an earlier iteration of the loop. */
    unsigned int index, last_read;
    struct hlsl_reg reg;
};

struct hlsl_src
{
    struct hlsl_ir_node *node;
    struct list entry;
};

#define HLSL_STORAGE_EXTERN          0x00000001
#define HLSL_STORAGE_NOINTERPOLATION 0x00000002
#define HLSL_MODIFIER_PRECISE        0x00000004
#define HLSL_STORAGE_SHARED          0x00000008
#define HLSL_STORAGE_GROUPSHARED     0x00000010
#define HLSL_STORAGE_STATIC          0x00000020
#define HLSL_STORAGE_UNIFORM         0x00000040
#define HLSL_STORAGE_VOLATILE        0x00000080
#define HLSL_MODIFIER_CONST          0x00000100
#define HLSL_MODIFIER_ROW_MAJOR      0x00000200
#define HLSL_MODIFIER_COLUMN_MAJOR   0x00000400
#define HLSL_STORAGE_IN              0x00000800
#define HLSL_STORAGE_OUT             0x00001000

#define HLSL_TYPE_MODIFIERS_MASK     (HLSL_MODIFIER_PRECISE | HLSL_STORAGE_VOLATILE | \
                                      HLSL_MODIFIER_CONST | HLSL_MODIFIER_ROW_MAJOR | \
                                      HLSL_MODIFIER_COLUMN_MAJOR)

#define HLSL_MODIFIERS_MAJORITY_MASK (HLSL_MODIFIER_ROW_MAJOR | HLSL_MODIFIER_COLUMN_MAJOR)

struct hlsl_reg_reservation
{
    char type;
    unsigned int index;
};

struct hlsl_ir_var
{
    struct hlsl_type *data_type;
    struct vkd3d_shader_location loc;
    const char *name;
    struct hlsl_semantic semantic;
    struct hlsl_buffer *buffer;
    unsigned int modifiers;
    struct hlsl_reg_reservation reg_reservation;
    struct list scope_entry, param_entry, extern_entry;

    unsigned int first_write, last_read;
    unsigned int buffer_offset;
    struct hlsl_reg reg;

    uint32_t is_input_semantic : 1;
    uint32_t is_output_semantic : 1;
    uint32_t is_uniform : 1;
    uint32_t is_param : 1;
};

struct hlsl_ir_function
{
    struct rb_entry entry;
    const char *name;
    struct rb_tree overloads;
    bool intrinsic;
};

struct hlsl_ir_function_decl
{
    struct hlsl_type *return_type;
    struct hlsl_ir_var *return_var;
    struct vkd3d_shader_location loc;
    struct rb_entry entry;
    struct hlsl_ir_function *func;
    struct list *parameters;
    struct list *body;
};

struct hlsl_ir_if
{
    struct hlsl_ir_node node;
    struct hlsl_src condition;
    struct list then_instrs;
    struct list else_instrs;
};

struct hlsl_ir_loop
{
    struct hlsl_ir_node node;
    /* loop condition is stored in the body (as "if (!condition) break;") */
    struct list body;
    unsigned int next_index; /* liveness index of the end of the loop */
};

enum hlsl_ir_expr_op
{
    HLSL_OP1_ABS,
    HLSL_OP1_BIT_NOT,
    HLSL_OP1_CAST,
    HLSL_OP1_COS,
    HLSL_OP1_COS_REDUCED,    /* Reduced range [-pi, pi] */
    HLSL_OP1_DSX,
    HLSL_OP1_DSY,
    HLSL_OP1_EXP2,
    HLSL_OP1_FRACT,
    HLSL_OP1_LOG2,
    HLSL_OP1_LOGIC_NOT,
    HLSL_OP1_NEG,
    HLSL_OP1_NRM,
    HLSL_OP1_RCP,
    HLSL_OP1_RSQ,
    HLSL_OP1_SAT,
    HLSL_OP1_SIGN,
    HLSL_OP1_SIN,
    HLSL_OP1_SIN_REDUCED,    /* Reduced range [-pi, pi] */
    HLSL_OP1_SQRT,

    HLSL_OP2_ADD,
    HLSL_OP2_BIT_AND,
    HLSL_OP2_BIT_OR,
    HLSL_OP2_BIT_XOR,
    HLSL_OP2_CRS,
    HLSL_OP2_DIV,
    HLSL_OP2_DOT,
    HLSL_OP2_EQUAL,
    HLSL_OP2_GEQUAL,
    HLSL_OP2_GREATER,
    HLSL_OP2_LEQUAL,
    HLSL_OP2_LESS,
    HLSL_OP2_LOGIC_AND,
    HLSL_OP2_LOGIC_OR,
    HLSL_OP2_LSHIFT,
    HLSL_OP2_MAX,
    HLSL_OP2_MIN,
    HLSL_OP2_MOD,
    HLSL_OP2_MUL,
    HLSL_OP2_NEQUAL,
    HLSL_OP2_RSHIFT,

    HLSL_OP3_LERP,
};

struct hlsl_ir_expr
{
    struct hlsl_ir_node node;
    enum hlsl_ir_expr_op op;
    struct hlsl_src operands[3];
};

enum hlsl_ir_jump_type
{
    HLSL_IR_JUMP_BREAK,
    HLSL_IR_JUMP_CONTINUE,
    HLSL_IR_JUMP_DISCARD,
    HLSL_IR_JUMP_RETURN,
};

struct hlsl_ir_jump
{
    struct hlsl_ir_node node;
    enum hlsl_ir_jump_type type;
};

struct hlsl_ir_swizzle
{
    struct hlsl_ir_node node;
    struct hlsl_src val;
    DWORD swizzle;
};

struct hlsl_deref
{
    struct hlsl_ir_var *var;
    struct hlsl_src offset;
};

struct hlsl_ir_load
{
    struct hlsl_ir_node node;
    struct hlsl_deref src;
};

struct hlsl_ir_store
{
    struct hlsl_ir_node node;
    struct hlsl_deref lhs;
    struct hlsl_src rhs;
    unsigned char writemask;
};

struct hlsl_ir_constant
{
    struct hlsl_ir_node node;
    union hlsl_constant_value
    {
        uint32_t u;
        int32_t i;
        float f;
        double d;
        bool b;
    } value[4];
    struct hlsl_reg reg;
};

struct hlsl_scope
{
    struct list entry;
    struct list vars;
    struct rb_tree types;
    struct hlsl_scope *upper;
};

struct hlsl_profile_info
{
    const char *name;
    enum vkd3d_shader_type type;
    unsigned int major_version;
    unsigned int minor_version;
    unsigned int major_level;
    unsigned int minor_level;
    bool software;
};

struct hlsl_vec4
{
    float f[4];
};

enum hlsl_buffer_type
{
    HLSL_BUFFER_CONSTANT,
    HLSL_BUFFER_TEXTURE,
};

struct hlsl_buffer
{
    struct vkd3d_shader_location loc;
    enum hlsl_buffer_type type;
    const char *name;
    struct hlsl_reg_reservation reservation;
    struct list entry;

    unsigned size, used_size;
    struct hlsl_reg reg;
};

struct hlsl_ctx
{
    const struct hlsl_profile_info *profile;

    const char **source_files;
    unsigned int source_files_count;
    struct vkd3d_shader_location location;
    struct vkd3d_shader_message_context *message_context;
    struct vkd3d_string_buffer_cache string_buffers;
    int result;

    void *scanner;

    struct hlsl_scope *cur_scope;
    struct hlsl_scope *globals;
    struct list scopes;
    struct list extern_vars;

    struct list buffers;
    struct hlsl_buffer *cur_buffer, *globals_buffer, *params_buffer;
    struct list types;
    struct rb_tree functions;
    const struct hlsl_ir_function_decl *cur_function;

    enum hlsl_matrix_majority matrix_majority;

    struct
    {
        struct hlsl_type *scalar[HLSL_TYPE_LAST_SCALAR + 1];
        struct hlsl_type *vector[HLSL_TYPE_LAST_SCALAR + 1][4];
        struct hlsl_type *sampler[HLSL_SAMPLER_DIM_MAX + 1];
        struct hlsl_type *Void;
    } builtin_types;

    struct list static_initializers;

    struct hlsl_constant_defs
    {
        struct hlsl_vec4 *values;
        size_t count, size;
    } constant_defs;
    uint32_t temp_count;
};

enum hlsl_error_level
{
    HLSL_LEVEL_ERROR = 0,
    HLSL_LEVEL_WARNING,
    HLSL_LEVEL_NOTE,
};

static inline struct hlsl_ir_constant *hlsl_ir_constant(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_CONSTANT);
    return CONTAINING_RECORD(node, struct hlsl_ir_constant, node);
}

static inline struct hlsl_ir_expr *hlsl_ir_expr(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_EXPR);
    return CONTAINING_RECORD(node, struct hlsl_ir_expr, node);
}

static inline struct hlsl_ir_if *hlsl_ir_if(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_IF);
    return CONTAINING_RECORD(node, struct hlsl_ir_if, node);
}

static inline struct hlsl_ir_jump *hlsl_ir_jump(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_JUMP);
    return CONTAINING_RECORD(node, struct hlsl_ir_jump, node);
}

static inline struct hlsl_ir_load *hlsl_ir_load(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_LOAD);
    return CONTAINING_RECORD(node, struct hlsl_ir_load, node);
}

static inline struct hlsl_ir_loop *hlsl_ir_loop(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_LOOP);
    return CONTAINING_RECORD(node, struct hlsl_ir_loop, node);
}

static inline struct hlsl_ir_store *hlsl_ir_store(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_STORE);
    return CONTAINING_RECORD(node, struct hlsl_ir_store, node);
}

static inline struct hlsl_ir_swizzle *hlsl_ir_swizzle(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_SWIZZLE);
    return CONTAINING_RECORD(node, struct hlsl_ir_swizzle, node);
}

static inline void init_node(struct hlsl_ir_node *node, enum hlsl_ir_node_type type,
        struct hlsl_type *data_type, struct vkd3d_shader_location loc)
{
    memset(node, 0, sizeof(*node));
    node->type = type;
    node->data_type = data_type;
    node->loc = loc;
    list_init(&node->uses);
}

static inline void hlsl_src_from_node(struct hlsl_src *src, struct hlsl_ir_node *node)
{
    src->node = node;
    if (node)
        list_add_tail(&node->uses, &src->entry);
}

static inline void hlsl_src_remove(struct hlsl_src *src)
{
    if (src->node)
        list_remove(&src->entry);
    src->node = NULL;
}

static inline void *hlsl_alloc(struct hlsl_ctx *ctx, size_t size)
{
    void *ptr = vkd3d_calloc(1, size);

    if (!ptr)
        ctx->result = VKD3D_ERROR_OUT_OF_MEMORY;
    return ptr;
}

static inline char *hlsl_strdup(struct hlsl_ctx *ctx, const char *string)
{
    char *ptr = vkd3d_strdup(string);

    if (!ptr)
        ctx->result = VKD3D_ERROR_OUT_OF_MEMORY;
    return ptr;
}

static inline bool hlsl_array_reserve(struct hlsl_ctx *ctx, void **elements,
        size_t *capacity, size_t element_count, size_t element_size)
{
    bool ret = vkd3d_array_reserve(elements, capacity, element_count, element_size);

    if (!ret)
        ctx->result = VKD3D_ERROR_OUT_OF_MEMORY;
    return ret;
}

static inline struct vkd3d_string_buffer *hlsl_get_string_buffer(struct hlsl_ctx *ctx)
{
    struct vkd3d_string_buffer *ret = vkd3d_string_buffer_get(&ctx->string_buffers);

    if (!ret)
        ctx->result = VKD3D_ERROR_OUT_OF_MEMORY;
    return ret;
}

static inline void hlsl_release_string_buffer(struct hlsl_ctx *ctx, struct vkd3d_string_buffer *buffer)
{
    vkd3d_string_buffer_release(&ctx->string_buffers, buffer);
}

const char *debug_hlsl_expr_op(enum hlsl_ir_expr_op op);
const char *debug_hlsl_type(struct hlsl_ctx *ctx, const struct hlsl_type *type);
const char *debug_hlsl_writemask(unsigned int writemask);

struct vkd3d_string_buffer *hlsl_type_to_string(struct hlsl_ctx *ctx, const struct hlsl_type *type);
struct vkd3d_string_buffer *hlsl_modifiers_to_string(struct hlsl_ctx *ctx, unsigned int modifiers);
const char *hlsl_node_type_to_string(enum hlsl_ir_node_type type);

void hlsl_add_function(struct hlsl_ctx *ctx, char *name, struct hlsl_ir_function_decl *decl, bool intrinsic);
bool hlsl_add_var(struct hlsl_ctx *ctx, struct hlsl_ir_var *decl, bool local_var);

void hlsl_dump_function(struct hlsl_ctx *ctx, const struct hlsl_ir_function_decl *func);

int hlsl_emit_dxbc(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out);

void hlsl_free_instr(struct hlsl_ir_node *node);
void hlsl_free_instr_list(struct list *list);
void hlsl_free_type(struct hlsl_type *type);
void hlsl_free_var(struct hlsl_ir_var *decl);

bool hlsl_get_function(struct hlsl_ctx *ctx, const char *name);
struct hlsl_ir_function_decl *hlsl_get_func_decl(struct hlsl_ctx *ctx, const char *name);
struct hlsl_type *hlsl_get_type(struct hlsl_scope *scope, const char *name, bool recursive);
struct hlsl_ir_var *hlsl_get_var(struct hlsl_scope *scope, const char *name);

struct hlsl_type *hlsl_new_array_type(struct hlsl_ctx *ctx, struct hlsl_type *basic_type, unsigned int array_size);
struct hlsl_ir_node *hlsl_new_binary_expr(struct hlsl_ctx *ctx, enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg1,
        struct hlsl_ir_node *arg2);
struct hlsl_buffer *hlsl_new_buffer(struct hlsl_ctx *ctx, enum hlsl_buffer_type type, const char *name,
        const struct hlsl_reg_reservation *reservation, struct vkd3d_shader_location loc);
struct hlsl_ir_expr *hlsl_new_cast(struct hlsl_ctx *ctx, struct hlsl_ir_node *node, struct hlsl_type *type,
        struct vkd3d_shader_location *loc);
struct hlsl_ir_expr *hlsl_new_copy(struct hlsl_ctx *ctx, struct hlsl_ir_node *node);
struct hlsl_ir_function_decl *hlsl_new_func_decl(struct hlsl_ctx *ctx, struct hlsl_type *return_type,
        struct list *parameters, const struct hlsl_semantic *semantic, struct vkd3d_shader_location loc);
struct hlsl_ir_if *hlsl_new_if(struct hlsl_ctx *ctx, struct hlsl_ir_node *condition, struct vkd3d_shader_location loc);
struct hlsl_ir_jump *hlsl_new_jump(struct hlsl_ctx *ctx, enum hlsl_ir_jump_type type, struct vkd3d_shader_location loc);
struct hlsl_ir_load *hlsl_new_load(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, struct hlsl_ir_node *offset,
        struct hlsl_type *type, struct vkd3d_shader_location loc);
struct hlsl_ir_loop *hlsl_new_loop(struct hlsl_ctx *ctx, struct vkd3d_shader_location loc);
struct hlsl_ir_store *hlsl_new_simple_store(struct hlsl_ctx *ctx, struct hlsl_ir_var *lhs, struct hlsl_ir_node *rhs);
struct hlsl_ir_store *hlsl_new_store(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, struct hlsl_ir_node *offset,
        struct hlsl_ir_node *rhs, unsigned int writemask, struct vkd3d_shader_location loc);
struct hlsl_type *hlsl_new_struct_type(struct hlsl_ctx *ctx, const char *name, struct list *fields);
struct hlsl_ir_swizzle *hlsl_new_swizzle(struct hlsl_ctx *ctx, DWORD s, unsigned int components,
        struct hlsl_ir_node *val, struct vkd3d_shader_location *loc);
struct hlsl_ir_var *hlsl_new_synthetic_var(struct hlsl_ctx *ctx, const char *name, struct hlsl_type *type,
        const struct vkd3d_shader_location loc);
struct hlsl_type *hlsl_new_type(struct hlsl_ctx *ctx, const char *name, enum hlsl_type_class type_class,
        enum hlsl_base_type base_type, unsigned dimx, unsigned dimy);
struct hlsl_ir_constant *hlsl_new_uint_constant(struct hlsl_ctx *ctx, unsigned int n,
        const struct vkd3d_shader_location loc);
struct hlsl_ir_node *hlsl_new_unary_expr(struct hlsl_ctx *ctx, enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg,
        struct vkd3d_shader_location loc);
struct hlsl_ir_var *hlsl_new_var(struct hlsl_ctx *ctx, const char *name, struct hlsl_type *type,
        const struct vkd3d_shader_location loc, const struct hlsl_semantic *semantic, unsigned int modifiers,
        const struct hlsl_reg_reservation *reg_reservation);
struct hlsl_ir_load *hlsl_new_var_load(struct hlsl_ctx *ctx, struct hlsl_ir_var *var,
        const struct vkd3d_shader_location loc);

void hlsl_error(struct hlsl_ctx *ctx, const struct vkd3d_shader_location loc,
        enum vkd3d_shader_error error, const char *fmt, ...) VKD3D_PRINTF_FUNC(4, 5);
void hlsl_fixme(struct hlsl_ctx *ctx, const struct vkd3d_shader_location loc,
        const char *fmt, ...) VKD3D_PRINTF_FUNC(3, 4);
void hlsl_warning(struct hlsl_ctx *ctx, const struct vkd3d_shader_location loc,
        enum vkd3d_shader_error error, const char *fmt, ...) VKD3D_PRINTF_FUNC(4, 5);
void hlsl_note(struct hlsl_ctx *ctx, const struct vkd3d_shader_location loc,
        enum vkd3d_shader_log_level level, const char *fmt, ...) VKD3D_PRINTF_FUNC(4, 5);

void hlsl_push_scope(struct hlsl_ctx *ctx);
void hlsl_pop_scope(struct hlsl_ctx *ctx);

bool hlsl_scope_add_type(struct hlsl_scope *scope, struct hlsl_type *type);

struct hlsl_type *hlsl_type_clone(struct hlsl_ctx *ctx, struct hlsl_type *old,
        unsigned int default_majority, unsigned int modifiers);
unsigned int hlsl_type_component_count(struct hlsl_type *type);
unsigned int hlsl_type_get_sm4_offset(const struct hlsl_type *type, unsigned int offset);
bool hlsl_type_is_void(const struct hlsl_type *type);
bool hlsl_types_are_equal(const struct hlsl_type *t1, const struct hlsl_type *t2);

unsigned int hlsl_combine_swizzles(unsigned int first, unsigned int second, unsigned int dim);
unsigned int hlsl_combine_writemasks(unsigned int first, unsigned int second);
unsigned int hlsl_map_swizzle(unsigned int swizzle, unsigned int writemask);
unsigned int hlsl_swizzle_from_writemask(unsigned int writemask);

struct hlsl_reg hlsl_reg_from_deref(const struct hlsl_deref *deref, const struct hlsl_type *type);

bool hlsl_sm1_register_from_semantic(struct hlsl_ctx *ctx, const struct hlsl_semantic *semantic,
        bool output, D3DSHADER_PARAM_REGISTER_TYPE *type, unsigned int *reg);
bool hlsl_sm1_usage_from_semantic(const struct hlsl_semantic *semantic, D3DDECLUSAGE *usage, uint32_t *usage_idx);
int hlsl_sm1_write(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out);

bool hlsl_sm4_usage_from_semantic(struct hlsl_ctx *ctx,
        const struct hlsl_semantic *semantic, bool output, D3D_NAME *usage);
bool hlsl_sm4_register_from_semantic(struct hlsl_ctx *ctx, const struct hlsl_semantic *semantic,
        bool output, enum vkd3d_sm4_register_type *type, bool *has_idx);
int hlsl_sm4_write(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out);

int hlsl_lexer_compile(struct hlsl_ctx *ctx, const struct vkd3d_shader_code *hlsl);

#endif
