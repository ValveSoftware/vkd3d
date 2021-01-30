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

enum parse_status
{
    PARSE_SUCCESS = 0,
    PARSE_WARN = 1,
    PARSE_ERR = 2
};

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
    unsigned int reg_size;
    union
    {
        struct list *elements;
        struct
        {
            struct hlsl_type *type;
            unsigned int elements_count;
        } array;
    } e;
};

struct hlsl_struct_field
{
    struct list entry;
    struct hlsl_type *type;
    const char *name;
    const char *semantic;
    DWORD modifiers;
    unsigned int reg_offset;
};

struct source_location
{
    const char *file;
    unsigned int line;
    unsigned int col;
};

enum hlsl_ir_node_type
{
    HLSL_IR_ASSIGNMENT = 0,
    HLSL_IR_CONSTANT,
    HLSL_IR_EXPR,
    HLSL_IR_IF,
    HLSL_IR_LOAD,
    HLSL_IR_LOOP,
    HLSL_IR_JUMP,
    HLSL_IR_SWIZZLE,
};

struct hlsl_ir_node
{
    struct list entry;
    enum hlsl_ir_node_type type;
    struct hlsl_type *data_type;

    struct list uses;

    struct source_location loc;

    /* Liveness ranges. "index" is the index of this instruction. Since this is
     * essentially an SSA value, the earliest live point is the index. This is
     * true even for loops, since currently we can't have a reference to a
     * value generated in an earlier iteration of the loop. */
    unsigned int index, last_read;
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

struct reg_reservation
{
    enum vkd3d_shader_register_type type;
    DWORD regnum;
};

struct hlsl_ir_var
{
    struct hlsl_type *data_type;
    struct source_location loc;
    const char *name;
    const char *semantic;
    unsigned int modifiers;
    const struct reg_reservation *reg_reservation;
    struct list scope_entry, param_entry;

    unsigned int first_write, last_read;
};

struct hlsl_ir_function
{
    struct rb_entry entry;
    const char *name;
    struct rb_tree overloads;
    BOOL intrinsic;
};

struct hlsl_ir_function_decl
{
    struct hlsl_type *return_type;
    struct hlsl_ir_var *return_var;
    struct source_location loc;
    struct rb_entry entry;
    struct hlsl_ir_function *func;
    const char *semantic;
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
    HLSL_IR_UNOP_BIT_NOT = 0,
    HLSL_IR_UNOP_LOGIC_NOT,
    HLSL_IR_UNOP_NEG,
    HLSL_IR_UNOP_ABS,
    HLSL_IR_UNOP_SIGN,
    HLSL_IR_UNOP_RCP,
    HLSL_IR_UNOP_RSQ,
    HLSL_IR_UNOP_SQRT,
    HLSL_IR_UNOP_NRM,
    HLSL_IR_UNOP_EXP2,
    HLSL_IR_UNOP_LOG2,

    HLSL_IR_UNOP_CAST,

    HLSL_IR_UNOP_FRACT,

    HLSL_IR_UNOP_SIN,
    HLSL_IR_UNOP_COS,
    HLSL_IR_UNOP_SIN_REDUCED,    /* Reduced range [-pi, pi] */
    HLSL_IR_UNOP_COS_REDUCED,    /* Reduced range [-pi, pi] */

    HLSL_IR_UNOP_DSX,
    HLSL_IR_UNOP_DSY,

    HLSL_IR_UNOP_SAT,

    HLSL_IR_UNOP_PREINC,
    HLSL_IR_UNOP_PREDEC,
    HLSL_IR_UNOP_POSTINC,
    HLSL_IR_UNOP_POSTDEC,

    HLSL_IR_BINOP_ADD,
    HLSL_IR_BINOP_SUB,
    HLSL_IR_BINOP_MUL,
    HLSL_IR_BINOP_DIV,

    HLSL_IR_BINOP_MOD,

    HLSL_IR_BINOP_LESS,
    HLSL_IR_BINOP_GREATER,
    HLSL_IR_BINOP_LEQUAL,
    HLSL_IR_BINOP_GEQUAL,
    HLSL_IR_BINOP_EQUAL,
    HLSL_IR_BINOP_NEQUAL,

    HLSL_IR_BINOP_LOGIC_AND,
    HLSL_IR_BINOP_LOGIC_OR,

    HLSL_IR_BINOP_LSHIFT,
    HLSL_IR_BINOP_RSHIFT,
    HLSL_IR_BINOP_BIT_AND,
    HLSL_IR_BINOP_BIT_OR,
    HLSL_IR_BINOP_BIT_XOR,

    HLSL_IR_BINOP_DOT,
    HLSL_IR_BINOP_CRS,
    HLSL_IR_BINOP_MIN,
    HLSL_IR_BINOP_MAX,

    HLSL_IR_BINOP_POW,

    HLSL_IR_TEROP_LERP,

    HLSL_IR_SEQUENCE,
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

struct hlsl_ir_assignment
{
    struct hlsl_ir_node node;
    struct hlsl_deref lhs;
    struct hlsl_src rhs;
    unsigned char writemask;
};

struct hlsl_ir_constant
{
    struct hlsl_ir_node node;
    union
    {
        unsigned u[4];
        int i[4];
        float f[4];
        double d[4];
        BOOL b[4];
    } value;
};

struct hlsl_scope
{
    struct list entry;
    struct list vars;
    struct rb_tree types;
    struct hlsl_scope *upper;
};

/* Structures used only during parsing */
struct parse_parameter
{
    struct hlsl_type *type;
    const char *name;
    const char *semantic;
    const struct reg_reservation *reg_reservation;
    unsigned int modifiers;
};

struct parse_colon_attribute
{
    const char *semantic;
    struct reg_reservation *reg_reservation;
};

struct parse_initializer
{
    struct hlsl_ir_node **args;
    unsigned int args_count;
    struct list *instrs;
};

struct parse_variable_def
{
    struct list entry;
    struct source_location loc;

    char *name;
    unsigned int array_size;
    const char *semantic;
    struct reg_reservation *reg_reservation;
    struct parse_initializer initializer;
};

struct parse_function
{
    char *name;
    struct hlsl_ir_function_decl *decl;
};

struct parse_if_body
{
    struct list *then_instrs;
    struct list *else_instrs;
};

enum parse_unary_op
{
    UNARY_OP_PLUS,
    UNARY_OP_MINUS,
    UNARY_OP_LOGICNOT,
    UNARY_OP_BITNOT,
};

enum parse_assign_op
{
    ASSIGN_OP_ASSIGN,
    ASSIGN_OP_ADD,
    ASSIGN_OP_SUB,
    ASSIGN_OP_MUL,
    ASSIGN_OP_DIV,
    ASSIGN_OP_MOD,
    ASSIGN_OP_LSHIFT,
    ASSIGN_OP_RSHIFT,
    ASSIGN_OP_AND,
    ASSIGN_OP_OR,
    ASSIGN_OP_XOR,
};

struct hlsl_parse_ctx
{
    const char **source_files;
    unsigned int source_files_count;
    const char *source_file;
    unsigned int line_no;
    unsigned int column;
    enum parse_status status;
    struct vkd3d_shader_message_context *message_context;

    struct hlsl_scope *cur_scope;
    struct hlsl_scope *globals;
    struct list scopes;

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
};

extern struct hlsl_parse_ctx hlsl_ctx DECLSPEC_HIDDEN;

enum hlsl_error_level
{
    HLSL_LEVEL_ERROR = 0,
    HLSL_LEVEL_WARNING,
    HLSL_LEVEL_NOTE,
};

void hlsl_message(const char *fmt, ...) VKD3D_PRINTF_FUNC(1,2) DECLSPEC_HIDDEN;
void hlsl_report_message(const struct source_location loc,
        enum hlsl_error_level level, const char *fmt, ...) VKD3D_PRINTF_FUNC(3,4) DECLSPEC_HIDDEN;

static inline struct hlsl_ir_assignment *assignment_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_ASSIGNMENT);
    return CONTAINING_RECORD(node, struct hlsl_ir_assignment, node);
}

static inline struct hlsl_ir_constant *constant_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_CONSTANT);
    return CONTAINING_RECORD(node, struct hlsl_ir_constant, node);
}

static inline struct hlsl_ir_expr *expr_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_EXPR);
    return CONTAINING_RECORD(node, struct hlsl_ir_expr, node);
}

static inline struct hlsl_ir_if *if_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_IF);
    return CONTAINING_RECORD(node, struct hlsl_ir_if, node);
}

static inline struct hlsl_ir_jump *jump_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_JUMP);
    return CONTAINING_RECORD(node, struct hlsl_ir_jump, node);
}

static inline struct hlsl_ir_load *load_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_LOAD);
    return CONTAINING_RECORD(node, struct hlsl_ir_load, node);
}

static inline struct hlsl_ir_loop *loop_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_LOOP);
    return CONTAINING_RECORD(node, struct hlsl_ir_loop, node);
}

static inline struct hlsl_ir_swizzle *swizzle_from_node(const struct hlsl_ir_node *node)
{
    assert(node->type == HLSL_IR_SWIZZLE);
    return CONTAINING_RECORD(node, struct hlsl_ir_swizzle, node);
}

static inline void init_node(struct hlsl_ir_node *node, enum hlsl_ir_node_type type,
        struct hlsl_type *data_type, struct source_location loc)
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

static inline void set_parse_status(enum parse_status *current, enum parse_status update)
{
    if (update == PARSE_ERR)
        *current = PARSE_ERR;
    else if (update == PARSE_WARN && *current == PARSE_SUCCESS)
        *current = PARSE_WARN;
}

struct hlsl_ir_expr *add_expr(struct list *instrs, enum hlsl_ir_expr_op op, struct hlsl_ir_node *operands[3],
        struct source_location *loc) DECLSPEC_HIDDEN;

struct hlsl_type *new_array_type(struct hlsl_type *basic_type, unsigned int array_size) DECLSPEC_HIDDEN;
struct hlsl_ir_assignment *new_assignment(struct hlsl_ir_var *var, struct hlsl_ir_node *offset,
        struct hlsl_ir_node *rhs, unsigned int writemask, struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_node *new_binary_expr(enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg1,
        struct hlsl_ir_node *arg2) DECLSPEC_HIDDEN;
struct hlsl_ir_expr *new_cast(struct hlsl_ir_node *node, struct hlsl_type *type,
        struct source_location *loc) DECLSPEC_HIDDEN;
struct hlsl_ir_function_decl *new_func_decl(struct hlsl_type *return_type,
        struct list *parameters, const char *semantic, struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_if *new_if(struct hlsl_ir_node *condition, struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_assignment *new_simple_assignment(struct hlsl_ir_var *lhs, struct hlsl_ir_node *rhs) DECLSPEC_HIDDEN;
struct hlsl_type *new_struct_type(const char *name, struct list *fields) DECLSPEC_HIDDEN;
struct hlsl_ir_swizzle *new_swizzle(DWORD s, unsigned int components,
        struct hlsl_ir_node *val, struct source_location *loc) DECLSPEC_HIDDEN;
struct hlsl_ir_var *new_synthetic_var(const char *name, struct hlsl_type *type,
        const struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_constant *new_uint_constant(unsigned int n, const struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_node *new_unary_expr(enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg,
        struct source_location loc) DECLSPEC_HIDDEN;
struct hlsl_ir_var *new_var(const char *name, struct hlsl_type *type, const struct source_location loc,
        const char *semantic, unsigned int modifiers, const struct reg_reservation *reg_reservation) DECLSPEC_HIDDEN;
struct hlsl_ir_load *new_var_load(struct hlsl_ir_var *var, const struct source_location loc) DECLSPEC_HIDDEN;

BOOL add_declaration(struct hlsl_scope *scope, struct hlsl_ir_var *decl, BOOL local_var) DECLSPEC_HIDDEN;
struct hlsl_ir_var *get_variable(struct hlsl_scope *scope, const char *name) DECLSPEC_HIDDEN;
void free_declaration(struct hlsl_ir_var *decl) DECLSPEC_HIDDEN;
struct hlsl_type *new_hlsl_type(const char *name, enum hlsl_type_class type_class,
        enum hlsl_base_type base_type, unsigned dimx, unsigned dimy) DECLSPEC_HIDDEN;
struct hlsl_type *clone_hlsl_type(struct hlsl_type *old, unsigned int default_majority) DECLSPEC_HIDDEN;
struct hlsl_type *get_type(struct hlsl_scope *scope, const char *name, BOOL recursive) DECLSPEC_HIDDEN;
BOOL is_row_major(const struct hlsl_type *type) DECLSPEC_HIDDEN;
BOOL find_function(const char *name) DECLSPEC_HIDDEN;
unsigned int components_count_type(struct hlsl_type *type) DECLSPEC_HIDDEN;
BOOL compare_hlsl_types(const struct hlsl_type *t1, const struct hlsl_type *t2) DECLSPEC_HIDDEN;
BOOL compatible_data_types(struct hlsl_type *s1, struct hlsl_type *s2) DECLSPEC_HIDDEN;
void push_scope(struct hlsl_parse_ctx *ctx) DECLSPEC_HIDDEN;
BOOL pop_scope(struct hlsl_parse_ctx *ctx) DECLSPEC_HIDDEN;
void init_functions_tree(struct rb_tree *funcs) DECLSPEC_HIDDEN;
void add_function_decl(struct rb_tree *funcs, char *name, struct hlsl_ir_function_decl *decl,
        BOOL intrinsic) DECLSPEC_HIDDEN;
BOOL type_is_void(const struct hlsl_type *type) DECLSPEC_HIDDEN;

int hlsl_lexer_compile(const char *text, enum vkd3d_shader_type type, DWORD major, DWORD minor, const char *entrypoint,
        struct vkd3d_shader_code *dxbc, struct vkd3d_shader_message_context *message_context) DECLSPEC_HIDDEN;
int hlsl_parser_compile(enum vkd3d_shader_type type, DWORD major, DWORD minor, const char *entrypoint,
        struct vkd3d_shader_code *dxbc, struct vkd3d_shader_message_context *message_context) DECLSPEC_HIDDEN;

const char *debug_base_type(const struct hlsl_type *type) DECLSPEC_HIDDEN;
const char *debug_hlsl_type(const struct hlsl_type *type) DECLSPEC_HIDDEN;
const char *debug_modifiers(DWORD modifiers) DECLSPEC_HIDDEN;
const char *debug_node_type(enum hlsl_ir_node_type type) DECLSPEC_HIDDEN;
void debug_dump_ir_function_decl(const struct hlsl_ir_function_decl *func) DECLSPEC_HIDDEN;

void free_hlsl_type(struct hlsl_type *type) DECLSPEC_HIDDEN;
void free_instr(struct hlsl_ir_node *node) DECLSPEC_HIDDEN;
void free_instr_list(struct list *list) DECLSPEC_HIDDEN;
void free_function_rb(struct rb_entry *entry, void *context) DECLSPEC_HIDDEN;

#endif
