/*
 * HLSL parser
 *
 * Copyright 2008 Stefan Dösinger
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

%code requires
{

#include "hlsl.h"
#include <stdio.h>

#define HLSL_YYLTYPE struct vkd3d_shader_location

struct parse_parameter
{
    struct hlsl_type *type;
    const char *name;
    struct hlsl_semantic semantic;
    const struct hlsl_reg_reservation *reg_reservation;
    unsigned int modifiers;
};

struct parse_colon_attribute
{
    struct hlsl_semantic semantic;
    struct hlsl_reg_reservation *reg_reservation;
};

struct parse_initializer
{
    struct hlsl_ir_node **args;
    unsigned int args_count;
    struct list *instrs;
};

struct parse_array_sizes
{
    uint32_t *sizes; /* innermost first */
    unsigned int count;
};

struct parse_variable_def
{
    struct list entry;
    struct vkd3d_shader_location loc;

    char *name;
    struct parse_array_sizes arrays;
    struct hlsl_semantic semantic;
    struct hlsl_reg_reservation *reg_reservation;
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

}

%code provides
{

int yylex(HLSL_YYSTYPE *yylval_param, HLSL_YYLTYPE *yylloc_param, void *yyscanner);

}

%code
{

#define YYLLOC_DEFAULT(cur, rhs, n) (cur) = YYRHSLOC(rhs, !!n)

static void yyerror(YYLTYPE *loc, void *scanner, struct hlsl_ctx *ctx, const char *s)
{
    hlsl_error(ctx, *loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SYNTAX, "%s", s);
}

static struct hlsl_ir_node *node_from_list(struct list *list)
{
    return LIST_ENTRY(list_tail(list), struct hlsl_ir_node, entry);
}

static struct list *make_empty_list(struct hlsl_ctx *ctx)
{
    struct list *list;

    if ((list = hlsl_alloc(ctx, sizeof(*list))))
        list_init(list);
    return list;
}

static void check_invalid_matrix_modifiers(struct hlsl_ctx *ctx, DWORD modifiers, struct vkd3d_shader_location loc)
{
    if (modifiers & HLSL_MODIFIERS_MAJORITY_MASK)
        hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                "'row_major' and 'column_major' modifiers are only allowed for matrices.");
}

static bool convertible_data_type(struct hlsl_type *type)
{
    return type->type != HLSL_CLASS_OBJECT;
}

static bool compatible_data_types(struct hlsl_type *t1, struct hlsl_type *t2)
{
   if (!convertible_data_type(t1) || !convertible_data_type(t2))
        return false;

    if (t1->type <= HLSL_CLASS_LAST_NUMERIC)
    {
        /* Scalar vars can be cast to pretty much everything */
        if (t1->dimx == 1 && t1->dimy == 1)
            return true;

        if (t1->type == HLSL_CLASS_VECTOR && t2->type == HLSL_CLASS_VECTOR)
            return t1->dimx >= t2->dimx;
    }

    /* The other way around is true too i.e. whatever to scalar */
    if (t2->type <= HLSL_CLASS_LAST_NUMERIC && t2->dimx == 1 && t2->dimy == 1)
        return true;

    if (t1->type == HLSL_CLASS_ARRAY)
    {
        if (hlsl_types_are_equal(t1->e.array.type, t2))
            /* e.g. float4[3] to float4 is allowed */
            return true;

        if (t2->type == HLSL_CLASS_ARRAY || t2->type == HLSL_CLASS_STRUCT)
            return hlsl_type_component_count(t1) >= hlsl_type_component_count(t2);
        else
            return hlsl_type_component_count(t1) == hlsl_type_component_count(t2);
    }

    if (t1->type == HLSL_CLASS_STRUCT)
        return hlsl_type_component_count(t1) >= hlsl_type_component_count(t2);

    if (t2->type == HLSL_CLASS_ARRAY || t2->type == HLSL_CLASS_STRUCT)
        return hlsl_type_component_count(t1) == hlsl_type_component_count(t2);

    if (t1->type == HLSL_CLASS_MATRIX || t2->type == HLSL_CLASS_MATRIX)
    {
        if (t1->type == HLSL_CLASS_MATRIX && t2->type == HLSL_CLASS_MATRIX && t1->dimx >= t2->dimx && t1->dimy >= t2->dimy)
            return true;

        /* Matrix-vector conversion is apparently allowed if they have the same components count */
        if ((t1->type == HLSL_CLASS_VECTOR || t2->type == HLSL_CLASS_VECTOR)
                && hlsl_type_component_count(t1) == hlsl_type_component_count(t2))
            return true;
        return false;
    }

    if (hlsl_type_component_count(t1) >= hlsl_type_component_count(t2))
        return true;
    return false;
}

static bool implicit_compatible_data_types(struct hlsl_type *t1, struct hlsl_type *t2)
{
    if (!convertible_data_type(t1) || !convertible_data_type(t2))
        return false;

    if (t1->type <= HLSL_CLASS_LAST_NUMERIC)
    {
        /* Scalar vars can be converted to any other numeric data type */
        if (t1->dimx == 1 && t1->dimy == 1 && t2->type <= HLSL_CLASS_LAST_NUMERIC)
            return true;
        /* The other way around is true too */
        if (t2->dimx == 1 && t2->dimy == 1 && t2->type <= HLSL_CLASS_LAST_NUMERIC)
            return true;
    }

    if (t1->type == HLSL_CLASS_ARRAY && t2->type == HLSL_CLASS_ARRAY)
    {
        return hlsl_type_component_count(t1) == hlsl_type_component_count(t2);
    }

    if ((t1->type == HLSL_CLASS_ARRAY && t2->type <= HLSL_CLASS_LAST_NUMERIC)
            || (t1->type <= HLSL_CLASS_LAST_NUMERIC && t2->type == HLSL_CLASS_ARRAY))
    {
        /* e.g. float4[3] to float4 is allowed */
        if (t1->type == HLSL_CLASS_ARRAY && hlsl_types_are_equal(t1->e.array.type, t2))
            return true;
        if (hlsl_type_component_count(t1) == hlsl_type_component_count(t2))
            return true;
        return false;
    }

    if (t1->type <= HLSL_CLASS_VECTOR && t2->type <= HLSL_CLASS_VECTOR)
    {
        if (t1->dimx >= t2->dimx)
            return true;
        return false;
    }

    if (t1->type == HLSL_CLASS_MATRIX || t2->type == HLSL_CLASS_MATRIX)
    {
        if (t1->type == HLSL_CLASS_MATRIX && t2->type == HLSL_CLASS_MATRIX
                && t1->dimx >= t2->dimx && t1->dimy >= t2->dimy)
            return true;

        /* Matrix-vector conversion is apparently allowed if they have the same components count */
        if ((t1->type == HLSL_CLASS_VECTOR || t2->type == HLSL_CLASS_VECTOR)
                && hlsl_type_component_count(t1) == hlsl_type_component_count(t2))
            return true;
        return false;
    }

    if (t1->type == HLSL_CLASS_STRUCT && t2->type == HLSL_CLASS_STRUCT)
        return hlsl_types_are_equal(t1, t2);

    return false;
}

static struct hlsl_ir_node *add_implicit_conversion(struct hlsl_ctx *ctx, struct list *instrs,
        struct hlsl_ir_node *node, struct hlsl_type *dst_type, struct vkd3d_shader_location *loc)
{
    struct hlsl_type *src_type = node->data_type;
    struct hlsl_ir_expr *cast;

    if (hlsl_types_are_equal(src_type, dst_type))
        return node;

    if (!implicit_compatible_data_types(src_type, dst_type))
    {
        struct vkd3d_string_buffer *src_string, *dst_string;

        src_string = hlsl_type_to_string(ctx, src_type);
        dst_string = hlsl_type_to_string(ctx, dst_type);
        if (src_string && dst_string)
            hlsl_error(ctx, *loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                    "Can't implicitly convert from %s to %s.", src_string->buffer, dst_string->buffer);
        hlsl_release_string_buffer(ctx, src_string);
        hlsl_release_string_buffer(ctx, dst_string);
        return NULL;
    }

    if (dst_type->dimx * dst_type->dimy < src_type->dimx * src_type->dimy)
        hlsl_warning(ctx, *loc, VKD3D_SHADER_WARNING_HLSL_IMPLICIT_TRUNCATION, "Implicit truncation of %s type.",
                src_type->type == HLSL_CLASS_VECTOR ? "vector" : "matrix");

    if (!(cast = hlsl_new_cast(ctx, node, dst_type, loc)))
        return NULL;
    list_add_tail(instrs, &cast->node.entry);
    return &cast->node;
}

static DWORD add_modifiers(struct hlsl_ctx *ctx, DWORD modifiers, DWORD mod, const struct vkd3d_shader_location loc)
{
    if (modifiers & mod)
    {
        struct vkd3d_string_buffer *string;

        if ((string = hlsl_modifiers_to_string(ctx, mod)))
            hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                    "Modifier '%s' was already specified.", string->buffer);
        hlsl_release_string_buffer(ctx, string);
        return modifiers;
    }
    if ((mod & HLSL_MODIFIERS_MAJORITY_MASK) && (modifiers & HLSL_MODIFIERS_MAJORITY_MASK))
    {
        hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                "'row_major' and 'column_major' modifiers are mutually exclusive.");
        return modifiers;
    }
    return modifiers | mod;
}

static bool append_conditional_break(struct hlsl_ctx *ctx, struct list *cond_list)
{
    struct hlsl_ir_node *condition, *not;
    struct hlsl_ir_jump *jump;
    struct hlsl_ir_if *iff;

    /* E.g. "for (i = 0; ; ++i)". */
    if (!list_count(cond_list))
        return true;

    condition = node_from_list(cond_list);
    if (!(not = hlsl_new_unary_expr(ctx, HLSL_IR_UNOP_LOGIC_NOT, condition, condition->loc)))
        return false;
    list_add_tail(cond_list, &not->entry);

    if (!(iff = hlsl_new_if(ctx, not, condition->loc)))
        return false;
    list_add_tail(cond_list, &iff->node.entry);

    if (!(jump = hlsl_new_jump(ctx, HLSL_IR_JUMP_BREAK, condition->loc)))
        return false;
    list_add_head(&iff->then_instrs, &jump->node.entry);
    return true;
}

enum loop_type
{
    LOOP_FOR,
    LOOP_WHILE,
    LOOP_DO_WHILE
};

static struct list *create_loop(struct hlsl_ctx *ctx, enum loop_type type, struct list *init, struct list *cond,
        struct list *iter, struct list *body, struct vkd3d_shader_location loc)
{
    struct list *list = NULL;
    struct hlsl_ir_loop *loop = NULL;
    struct hlsl_ir_if *cond_jump = NULL;

    if (!(list = make_empty_list(ctx)))
        goto oom;

    if (init)
        list_move_head(list, init);

    if (!(loop = hlsl_new_loop(ctx, loc)))
        goto oom;
    list_add_tail(list, &loop->node.entry);

    if (!append_conditional_break(ctx, cond))
        goto oom;

    if (type != LOOP_DO_WHILE)
        list_move_tail(&loop->body, cond);

    list_move_tail(&loop->body, body);

    if (iter)
        list_move_tail(&loop->body, iter);

    if (type == LOOP_DO_WHILE)
        list_move_tail(&loop->body, cond);

    vkd3d_free(init);
    vkd3d_free(cond);
    vkd3d_free(body);
    return list;

oom:
    vkd3d_free(loop);
    vkd3d_free(cond_jump);
    vkd3d_free(list);
    hlsl_free_instr_list(init);
    hlsl_free_instr_list(cond);
    hlsl_free_instr_list(iter);
    hlsl_free_instr_list(body);
    return NULL;
}

static unsigned int initializer_size(const struct parse_initializer *initializer)
{
    unsigned int count = 0, i;

    for (i = 0; i < initializer->args_count; ++i)
    {
        count += hlsl_type_component_count(initializer->args[i]->data_type);
    }
    return count;
}

static void free_parse_initializer(struct parse_initializer *initializer)
{
    hlsl_free_instr_list(initializer->instrs);
    vkd3d_free(initializer->args);
}

static struct hlsl_ir_swizzle *get_swizzle(struct hlsl_ctx *ctx, struct hlsl_ir_node *value, const char *swizzle,
        struct vkd3d_shader_location *loc)
{
    unsigned int len = strlen(swizzle), component = 0;
    unsigned int i, set, swiz = 0;
    bool valid;

    if (value->data_type->type == HLSL_CLASS_MATRIX)
    {
        /* Matrix swizzle */
        bool m_swizzle;
        unsigned int inc, x, y;

        if (len < 3 || swizzle[0] != '_')
            return NULL;
        m_swizzle = swizzle[1] == 'm';
        inc = m_swizzle ? 4 : 3;

        if (len % inc || len > inc * 4)
            return NULL;

        for (i = 0; i < len; i += inc)
        {
            if (swizzle[i] != '_')
                return NULL;
            if (m_swizzle)
            {
                if (swizzle[i + 1] != 'm')
                    return NULL;
                y = swizzle[i + 2] - '0';
                x = swizzle[i + 3] - '0';
            }
            else
            {
                y = swizzle[i + 1] - '1';
                x = swizzle[i + 2] - '1';
            }

            if (x >= value->data_type->dimx || y >= value->data_type->dimy)
                return NULL;
            swiz |= (y << 4 | x) << component * 8;
            component++;
        }
        return hlsl_new_swizzle(ctx, swiz, component, value, loc);
    }

    /* Vector swizzle */
    if (len > 4)
        return NULL;

    for (set = 0; set < 2; ++set)
    {
        valid = true;
        component = 0;
        for (i = 0; i < len; ++i)
        {
            char c[2][4] = {{'x', 'y', 'z', 'w'}, {'r', 'g', 'b', 'a'}};
            unsigned int s = 0;

            for (s = 0; s < 4; ++s)
            {
                if (swizzle[i] == c[set][s])
                    break;
            }
            if (s == 4)
            {
                valid = false;
                break;
            }

            if (s >= value->data_type->dimx)
                return NULL;
            swiz |= s << component * 2;
            component++;
        }
        if (valid)
            return hlsl_new_swizzle(ctx, swiz, component, value, loc);
    }

    return NULL;
}

static struct hlsl_ir_jump *add_return(struct hlsl_ctx *ctx, struct list *instrs,
        struct hlsl_ir_node *return_value, struct vkd3d_shader_location loc)
{
    struct hlsl_type *return_type = ctx->cur_function->return_type;
    struct hlsl_ir_jump *jump;

    if (return_value)
    {
        struct hlsl_ir_store *store;

        if (!(return_value = add_implicit_conversion(ctx, instrs, return_value, return_type, &loc)))
            return NULL;

        if (!(store = hlsl_new_simple_store(ctx, ctx->cur_function->return_var, return_value)))
            return NULL;
        list_add_after(&return_value->entry, &store->node.entry);
    }
    else if (!hlsl_type_is_void(return_type))
    {
        hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_RETURN, "Non-void function must return a value.");
        return NULL;
    }

    if (!(jump = hlsl_new_jump(ctx, HLSL_IR_JUMP_RETURN, loc)))
        return NULL;
    list_add_tail(instrs, &jump->node.entry);

    return jump;
}

static struct hlsl_ir_load *add_load(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_node *var_node,
        struct hlsl_ir_node *offset, struct hlsl_type *data_type, const struct vkd3d_shader_location loc)
{
    struct hlsl_ir_node *add = NULL;
    struct hlsl_ir_load *load;
    struct hlsl_ir_var *var;

    if (var_node->type == HLSL_IR_LOAD)
    {
        const struct hlsl_deref *src = &hlsl_ir_load(var_node)->src;

        var = src->var;
        if (src->offset.node)
        {
            if (!(add = hlsl_new_binary_expr(ctx, HLSL_IR_BINOP_ADD, src->offset.node, offset)))
                return NULL;
            list_add_tail(instrs, &add->entry);
            offset = add;
        }
    }
    else
    {
        struct hlsl_ir_store *store;
        char name[27];

        sprintf(name, "<deref-%p>", var_node);
        if (!(var = hlsl_new_synthetic_var(ctx, name, var_node->data_type, var_node->loc)))
            return NULL;

        if (!(store = hlsl_new_simple_store(ctx, var, var_node)))
            return NULL;

        list_add_tail(instrs, &store->node.entry);
    }

    if (!(load = hlsl_new_load(ctx, var, offset, data_type, loc)))
        return NULL;
    list_add_tail(instrs, &load->node.entry);
    return load;
}

static struct hlsl_ir_load *add_record_load(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_node *record,
        const struct hlsl_struct_field *field, const struct vkd3d_shader_location loc)
{
    struct hlsl_ir_constant *c;

    if (!(c = hlsl_new_uint_constant(ctx, field->reg_offset * 4, loc)))
        return NULL;
    list_add_tail(instrs, &c->node.entry);

    return add_load(ctx, instrs, record, &c->node, field->type, loc);
}

static struct hlsl_ir_load *add_array_load(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_node *array,
        struct hlsl_ir_node *index, const struct vkd3d_shader_location loc)
{
    const struct hlsl_type *expr_type = array->data_type;
    struct hlsl_type *data_type;
    struct hlsl_ir_constant *c;
    struct hlsl_ir_node *mul;

    if (expr_type->type == HLSL_CLASS_ARRAY)
    {
        data_type = expr_type->e.array.type;
    }
    else if (expr_type->type == HLSL_CLASS_MATRIX || expr_type->type == HLSL_CLASS_VECTOR)
    {
        /* This needs to be lowered now, while we still have type information. */
        FIXME("Index of matrix or vector type.\n");
        return NULL;
    }
    else
    {
        if (expr_type->type == HLSL_CLASS_SCALAR)
            hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_INDEX, "Scalar expressions cannot be array-indexed.");
        else
            hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_INDEX, "Expression cannot be array-indexed.");
        return NULL;
    }

    if (!(c = hlsl_new_uint_constant(ctx, data_type->reg_size * 4, loc)))
        return NULL;
    list_add_tail(instrs, &c->node.entry);
    if (!(mul = hlsl_new_binary_expr(ctx, HLSL_IR_BINOP_MUL, index, &c->node)))
        return NULL;
    list_add_tail(instrs, &mul->entry);
    index = mul;

    return add_load(ctx, instrs, array, index, data_type, loc);
}

static struct hlsl_struct_field *get_struct_field(struct list *fields, const char *name)
{
    struct hlsl_struct_field *f;

    LIST_FOR_EACH_ENTRY(f, fields, struct hlsl_struct_field, entry)
    {
        if (!strcmp(f->name, name))
            return f;
    }
    return NULL;
}

bool hlsl_type_is_row_major(const struct hlsl_type *type)
{
    /* Default to column-major if the majority isn't explicitly set, which can
     * happen for anonymous nodes. */
    return !!(type->modifiers & HLSL_MODIFIER_ROW_MAJOR);
}

static struct hlsl_type *apply_type_modifiers(struct hlsl_ctx *ctx, struct hlsl_type *type,
        unsigned int *modifiers, struct vkd3d_shader_location loc)
{
    unsigned int default_majority = 0;
    struct hlsl_type *new_type;

    /* This function is only used for declarations (i.e. variables and struct
     * fields), which should inherit the matrix majority. We only explicitly set
     * the default majority for declarations—typedefs depend on this—but we
     * want to always set it, so that an hlsl_type object is never used to
     * represent two different majorities (and thus can be used to store its
     * register size, etc.) */
    if (!(*modifiers & HLSL_MODIFIERS_MAJORITY_MASK)
            && !(type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK)
            && type->type == HLSL_CLASS_MATRIX)
    {
        if (ctx->matrix_majority == HLSL_COLUMN_MAJOR)
            default_majority = HLSL_MODIFIER_COLUMN_MAJOR;
        else
            default_majority = HLSL_MODIFIER_ROW_MAJOR;
    }

    if (!default_majority && !(*modifiers & HLSL_TYPE_MODIFIERS_MASK))
        return type;

    if (!(new_type = hlsl_type_clone(ctx, type, default_majority)))
        return NULL;

    new_type->modifiers = add_modifiers(ctx, new_type->modifiers, *modifiers, loc);
    *modifiers &= ~HLSL_TYPE_MODIFIERS_MASK;

    if (new_type->type == HLSL_CLASS_MATRIX)
        new_type->reg_size = hlsl_type_is_row_major(new_type) ? new_type->dimy : new_type->dimx;
    return new_type;
}

static struct list *gen_struct_fields(struct hlsl_ctx *ctx, struct hlsl_type *type, struct list *fields)
{
    struct parse_variable_def *v, *v_next;
    struct hlsl_struct_field *field;
    struct list *list;

    if (type->type == HLSL_CLASS_MATRIX)
        assert(type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK);

    if (!(list = make_empty_list(ctx)))
        return NULL;
    LIST_FOR_EACH_ENTRY_SAFE(v, v_next, fields, struct parse_variable_def, entry)
    {
        unsigned int i;

        if (!(field = hlsl_alloc(ctx, sizeof(*field))))
        {
            vkd3d_free(v);
            return list;
        }

        field->type = type;
        for (i = 0; i < v->arrays.count; ++i)
            field->type = hlsl_new_array_type(ctx, field->type, v->arrays.sizes[i]);
        field->loc = v->loc;
        field->name = v->name;
        field->semantic = v->semantic;
        if (v->initializer.args_count)
        {
            hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SYNTAX, "Illegal initializer on a struct field.");
            free_parse_initializer(&v->initializer);
        }
        list_add_tail(list, &field->entry);
        vkd3d_free(v);
    }
    vkd3d_free(fields);
    return list;
}

static bool add_typedef(struct hlsl_ctx *ctx, DWORD modifiers, struct hlsl_type *orig_type, struct list *list)
{
    struct parse_variable_def *v, *v_next;
    struct hlsl_type *type;
    unsigned int i;
    bool ret;

    LIST_FOR_EACH_ENTRY_SAFE(v, v_next, list, struct parse_variable_def, entry)
    {
        if (!v->arrays.count)
        {
            if (!(type = hlsl_type_clone(ctx, orig_type, 0)))
                return false;
        }
        else
        {
            type = orig_type;
        }

        for (i = 0; i < v->arrays.count; ++i)
        {
            if (!(type = hlsl_new_array_type(ctx, type, v->arrays.sizes[i])))
                return false;
        }

        vkd3d_free((void *)type->name);
        type->name = v->name;
        type->modifiers |= modifiers;

        if (type->type != HLSL_CLASS_MATRIX)
            check_invalid_matrix_modifiers(ctx, type->modifiers, v->loc);
        else
            type->reg_size = hlsl_type_is_row_major(type) ? type->dimy : type->dimx;

        if ((type->modifiers & HLSL_MODIFIER_COLUMN_MAJOR)
                && (type->modifiers & HLSL_MODIFIER_ROW_MAJOR))
            hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                    "'row_major' and 'column_major' modifiers are mutually exclusive.");

        ret = hlsl_scope_add_type(ctx->cur_scope, type);
        if (!ret)
            hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                    "Type '%s' is already defined.", v->name);
        vkd3d_free(v);
    }
    vkd3d_free(list);
    return true;
}

static bool add_func_parameter(struct hlsl_ctx *ctx, struct list *list,
        struct parse_parameter *param, const struct vkd3d_shader_location loc)
{
    struct hlsl_ir_var *var;

    if (param->type->type == HLSL_CLASS_MATRIX)
        assert(param->type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK);

    if ((param->modifiers & HLSL_STORAGE_OUT) && (param->modifiers & HLSL_STORAGE_UNIFORM))
        hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                "Parameter '%s' is declared as both \"out\" and \"uniform\".", param->name);

    if (!(var = hlsl_new_var(ctx, param->name, param->type, loc, &param->semantic, param->modifiers, param->reg_reservation)))
        return false;
    var->is_param = 1;

    if (!hlsl_add_var(ctx, var, false))
    {
        hlsl_free_var(var);
        return false;
    }
    list_add_tail(list, &var->param_entry);
    return true;
}

static struct hlsl_reg_reservation *parse_reg_reservation(struct hlsl_ctx *ctx, const char *reg_string)
{
    enum vkd3d_shader_register_type type;
    struct hlsl_reg_reservation *reg_res;
    DWORD regnum = 0;

    switch (reg_string[0])
    {
        case 'c':
            type = VKD3DSPR_CONST;
            break;
        case 'i':
            type = VKD3DSPR_CONSTINT;
            break;
        case 'b':
            type = VKD3DSPR_CONSTBOOL;
            break;
        case 's':
            type = VKD3DSPR_SAMPLER;
            break;
        default:
            FIXME("Unsupported register type.\n");
            return NULL;
     }

    if (!sscanf(reg_string + 1, "%u", &regnum))
    {
        FIXME("Unsupported register reservation syntax.\n");
        return NULL;
    }

    if (!(reg_res = hlsl_alloc(ctx, sizeof(*reg_res))))
        return NULL;
    reg_res->type = type;
    reg_res->regnum = regnum;
    return reg_res;
}

static const struct hlsl_ir_function_decl *get_overloaded_func(struct rb_tree *funcs, char *name,
        struct list *params, bool exact_signature)
{
    struct hlsl_ir_function *func;
    struct rb_entry *entry;

    entry = rb_get(funcs, name);
    if (entry)
    {
        func = RB_ENTRY_VALUE(entry, struct hlsl_ir_function, entry);

        entry = rb_get(&func->overloads, params);
        if (!entry)
        {
            if (!exact_signature)
                FIXME("No exact match, search for a compatible overloaded function (if any).\n");
            return NULL;
        }
        return RB_ENTRY_VALUE(entry, struct hlsl_ir_function_decl, entry);
    }
    return NULL;
}

static struct list *make_list(struct hlsl_ctx *ctx, struct hlsl_ir_node *node)
{
    struct list *list;

    if (!(list = make_empty_list(ctx)))
    {
        hlsl_free_instr(node);
        return NULL;
    }
    list_add_tail(list, &node->entry);
    return list;
}

static unsigned int evaluate_array_dimension(struct hlsl_ir_node *node)
{
    if (node->data_type->type != HLSL_CLASS_SCALAR)
        return 0;

    switch (node->type)
    {
        case HLSL_IR_CONSTANT:
        {
            struct hlsl_ir_constant *constant = hlsl_ir_constant(node);

            switch (constant->node.data_type->base_type)
            {
                case HLSL_TYPE_UINT:
                    return constant->value.u[0];
                case HLSL_TYPE_INT:
                    return constant->value.i[0];
                case HLSL_TYPE_FLOAT:
                case HLSL_TYPE_HALF:
                    return constant->value.f[0];
                case HLSL_TYPE_DOUBLE:
                    return constant->value.d[0];
                case HLSL_TYPE_BOOL:
                    return constant->value.b[0];
                default:
                    assert(0);
                    return 0;
            }
        }

        case HLSL_IR_EXPR:
        case HLSL_IR_LOAD:
        case HLSL_IR_SWIZZLE:
            FIXME("Unhandled type %s.\n", hlsl_node_type_to_string(node->type));
            return 0;

        case HLSL_IR_STORE:
        default:
            WARN("Invalid node type %s.\n", hlsl_node_type_to_string(node->type));
            return 0;
    }
}

static bool expr_compatible_data_types(struct hlsl_type *t1, struct hlsl_type *t2)
{
    if (t1->base_type > HLSL_TYPE_LAST_SCALAR || t2->base_type > HLSL_TYPE_LAST_SCALAR)
        return false;

    /* Scalar vars can be converted to pretty much everything */
    if ((t1->dimx == 1 && t1->dimy == 1) || (t2->dimx == 1 && t2->dimy == 1))
        return true;

    if (t1->type == HLSL_CLASS_VECTOR && t2->type == HLSL_CLASS_VECTOR)
        return true;

    if (t1->type == HLSL_CLASS_MATRIX || t2->type == HLSL_CLASS_MATRIX)
    {
        /* Matrix-vector conversion is apparently allowed if either they have the same components
           count or the matrix is nx1 or 1xn */
        if (t1->type == HLSL_CLASS_VECTOR || t2->type == HLSL_CLASS_VECTOR)
        {
            if (hlsl_type_component_count(t1) == hlsl_type_component_count(t2))
                return true;

            return (t1->type == HLSL_CLASS_MATRIX && (t1->dimx == 1 || t1->dimy == 1))
                    || (t2->type == HLSL_CLASS_MATRIX && (t2->dimx == 1 || t2->dimy == 1));
        }

        /* Both matrices */
        if ((t1->dimx >= t2->dimx && t1->dimy >= t2->dimy)
                || (t1->dimx <= t2->dimx && t1->dimy <= t2->dimy))
            return true;
    }

    return false;
}

static enum hlsl_base_type expr_common_base_type(enum hlsl_base_type t1, enum hlsl_base_type t2)
{
    static const enum hlsl_base_type types[] =
    {
        HLSL_TYPE_BOOL,
        HLSL_TYPE_INT,
        HLSL_TYPE_UINT,
        HLSL_TYPE_HALF,
        HLSL_TYPE_FLOAT,
        HLSL_TYPE_DOUBLE,
    };
    int t1_idx = -1, t2_idx = -1, i;

    for (i = 0; i < ARRAY_SIZE(types); ++i)
    {
        /* Always convert away from HLSL_TYPE_HALF */
        if (t1 == types[i])
            t1_idx = t1 == HLSL_TYPE_HALF ? i + 1 : i;
        if (t2 == types[i])
            t2_idx = t2 == HLSL_TYPE_HALF ? i + 1 : i;

        if (t1_idx != -1 && t2_idx != -1)
            break;
    }
    if (t1_idx == -1 || t2_idx == -1)
    {
        FIXME("Unexpected base type.\n");
        return HLSL_TYPE_FLOAT;
    }
    return t1_idx >= t2_idx ? t1 : t2;
}

static struct hlsl_type *expr_common_type(struct hlsl_ctx *ctx, struct hlsl_type *t1, struct hlsl_type *t2,
        struct vkd3d_shader_location *loc)
{
    enum hlsl_type_class type;
    enum hlsl_base_type base;
    unsigned int dimx, dimy;

    if (t1->type > HLSL_CLASS_LAST_NUMERIC)
    {
        struct vkd3d_string_buffer *string;

        if ((string = hlsl_type_to_string(ctx, t1)))
            hlsl_error(ctx, *loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                    "Expression of type \"%s\" cannot be used in a numeric expression.", string->buffer);
        hlsl_release_string_buffer(ctx, string);
        return NULL;
    }

    if (t2->type > HLSL_CLASS_LAST_NUMERIC)
    {
        struct vkd3d_string_buffer *string;

        if ((string = hlsl_type_to_string(ctx, t2)))
            hlsl_error(ctx, *loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                    "Expression of type \"%s\" cannot be used in a numeric expression.", string->buffer);
        hlsl_release_string_buffer(ctx, string);
        return NULL;
    }

    if (hlsl_types_are_equal(t1, t2))
        return t1;

    if (!expr_compatible_data_types(t1, t2))
    {
        struct vkd3d_string_buffer *t1_string = hlsl_type_to_string(ctx, t1);
        struct vkd3d_string_buffer *t2_string = hlsl_type_to_string(ctx, t2);

        if (t1_string && t2_string)
            hlsl_error(ctx, *loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                    "Expression data types \"%s\" and \"%s\" are incompatible.",
                    t1_string->buffer, t2_string->buffer);
        hlsl_release_string_buffer(ctx, t1_string);
        hlsl_release_string_buffer(ctx, t2_string);
        return NULL;
    }

    if (t1->base_type == t2->base_type)
        base = t1->base_type;
    else
        base = expr_common_base_type(t1->base_type, t2->base_type);

    if (t1->dimx == 1 && t1->dimy == 1)
    {
        type = t2->type;
        dimx = t2->dimx;
        dimy = t2->dimy;
    }
    else if (t2->dimx == 1 && t2->dimy == 1)
    {
        type = t1->type;
        dimx = t1->dimx;
        dimy = t1->dimy;
    }
    else if (t1->type == HLSL_CLASS_MATRIX && t2->type == HLSL_CLASS_MATRIX)
    {
        type = HLSL_CLASS_MATRIX;
        dimx = min(t1->dimx, t2->dimx);
        dimy = min(t1->dimy, t2->dimy);
    }
    else
    {
        /* Two vectors or a vector and a matrix (matrix must be 1xn or nx1) */
        unsigned int max_dim_1, max_dim_2;

        max_dim_1 = max(t1->dimx, t1->dimy);
        max_dim_2 = max(t2->dimx, t2->dimy);
        if (t1->dimx * t1->dimy == t2->dimx * t2->dimy)
        {
            type = HLSL_CLASS_VECTOR;
            dimx = max(t1->dimx, t2->dimx);
            dimy = 1;
        }
        else if (max_dim_1 <= max_dim_2)
        {
            type = t1->type;
            if (type == HLSL_CLASS_VECTOR)
            {
                dimx = max_dim_1;
                dimy = 1;
            }
            else
            {
                dimx = t1->dimx;
                dimy = t1->dimy;
            }
        }
        else
        {
            type = t2->type;
            if (type == HLSL_CLASS_VECTOR)
            {
                dimx = max_dim_2;
                dimy = 1;
            }
            else
            {
                dimx = t2->dimx;
                dimy = t2->dimy;
            }
        }
    }

    if (type == HLSL_CLASS_SCALAR)
        return ctx->builtin_types.scalar[base];
    if (type == HLSL_CLASS_VECTOR)
        return ctx->builtin_types.vector[base][dimx - 1];
    return hlsl_new_type(ctx, NULL, type, base, dimx, dimy);
}

static struct hlsl_ir_expr *add_expr(struct hlsl_ctx *ctx, struct list *instrs,
        enum hlsl_ir_expr_op op, struct hlsl_ir_node *operands[3], struct vkd3d_shader_location *loc)
{
    struct hlsl_ir_expr *expr;
    struct hlsl_type *type;
    unsigned int i;

    type = operands[0]->data_type;
    for (i = 1; i <= 2; ++i)
    {
        if (!operands[i])
            break;
        type = expr_common_type(ctx, type, operands[i]->data_type, loc);
        if (!type)
            return NULL;
    }
    for (i = 0; i <= 2; ++i)
    {
        struct hlsl_ir_expr *cast;

        if (!operands[i])
            break;
        if (hlsl_types_are_equal(operands[i]->data_type, type))
            continue;
        if (operands[i]->data_type->dimx * operands[i]->data_type->dimy != 1
                && operands[i]->data_type->dimx * operands[i]->data_type->dimy != type->dimx * type->dimy)
            hlsl_warning(ctx, operands[i]->loc, VKD3D_SHADER_WARNING_HLSL_IMPLICIT_TRUNCATION,
                    "Implicit truncation of %s type.",
                    operands[i]->data_type->type == HLSL_CLASS_VECTOR ? "vector" : "matrix");

        if (!(cast = hlsl_new_cast(ctx, operands[i], type, &operands[i]->loc)))
            return NULL;
        list_add_after(&operands[i]->entry, &cast->node.entry);
        operands[i] = &cast->node;
    }

    if (!(expr = hlsl_alloc(ctx, sizeof(*expr))))
        return NULL;
    init_node(&expr->node, HLSL_IR_EXPR, type, *loc);
    expr->op = op;
    for (i = 0; i <= 2; ++i)
        hlsl_src_from_node(&expr->operands[i], operands[i]);
    list_add_tail(instrs, &expr->node.entry);

    return expr;
}

static struct list *append_unop(struct list *list, struct hlsl_ir_node *node)
{
    list_add_tail(list, &node->entry);
    return list;
}

static struct list *add_binary_expr(struct hlsl_ctx *ctx, struct list *list1, struct list *list2,
        enum hlsl_ir_expr_op op, struct vkd3d_shader_location loc)
{
    struct hlsl_ir_node *args[3] = {node_from_list(list1), node_from_list(list2)};

    list_move_tail(list1, list2);
    vkd3d_free(list2);
    add_expr(ctx, list1, op, args, &loc);
    return list1;
}

static enum hlsl_ir_expr_op op_from_assignment(enum parse_assign_op op)
{
    static const enum hlsl_ir_expr_op ops[] =
    {
        0,
        HLSL_IR_BINOP_ADD,
        HLSL_IR_BINOP_SUB,
        HLSL_IR_BINOP_MUL,
        HLSL_IR_BINOP_DIV,
        HLSL_IR_BINOP_MOD,
        HLSL_IR_BINOP_LSHIFT,
        HLSL_IR_BINOP_RSHIFT,
        HLSL_IR_BINOP_BIT_AND,
        HLSL_IR_BINOP_BIT_OR,
        HLSL_IR_BINOP_BIT_XOR,
    };

    return ops[op];
}

static bool invert_swizzle(unsigned int *swizzle, unsigned int *writemask, unsigned int *ret_width)
{
    unsigned int i, j, bit = 0, inverted = 0, width, new_writemask = 0, new_swizzle = 0;

    /* Apply the writemask to the swizzle to get a new writemask and swizzle. */
    for (i = 0; i < 4; ++i)
    {
        if (*writemask & (1 << i))
        {
            unsigned int s = (*swizzle >> (i * 2)) & 3;
            new_swizzle |= s << (bit++ * 2);
            if (new_writemask & (1 << s))
                return false;
            new_writemask |= 1 << s;
        }
    }
    width = bit;

    /* Invert the swizzle. */
    bit = 0;
    for (i = 0; i < 4; ++i)
    {
        for (j = 0; j < width; ++j)
        {
            unsigned int s = (new_swizzle >> (j * 2)) & 3;
            if (s == i)
                inverted |= j << (bit++ * 2);
        }
    }

    *swizzle = inverted;
    *writemask = new_writemask;
    *ret_width = width;
    return true;
}

static struct hlsl_ir_node *add_assignment(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_node *lhs,
        enum parse_assign_op assign_op, struct hlsl_ir_node *rhs)
{
    struct hlsl_type *lhs_type = lhs->data_type;
    struct hlsl_ir_store *store;
    struct hlsl_ir_expr *copy;
    DWORD writemask = 0;

    if (assign_op != ASSIGN_OP_ASSIGN)
    {
        enum hlsl_ir_expr_op op = op_from_assignment(assign_op);
        struct hlsl_ir_node *args[3] = {lhs, rhs};
        struct hlsl_ir_expr *expr;

        if (!(expr = add_expr(ctx, instrs, op, args, &rhs->loc)))
            return NULL;
        rhs = &expr->node;
    }

    if (lhs_type->type <= HLSL_CLASS_LAST_NUMERIC)
    {
        writemask = (1 << lhs_type->dimx) - 1;

        if (!(rhs = add_implicit_conversion(ctx, instrs, rhs, lhs_type, &rhs->loc)))
            return NULL;
    }

    if (!(store = hlsl_alloc(ctx, sizeof(*store))))
        return NULL;

    while (lhs->type != HLSL_IR_LOAD)
    {
        if (lhs->type == HLSL_IR_EXPR && hlsl_ir_expr(lhs)->op == HLSL_IR_UNOP_CAST)
        {
            FIXME("Cast on the lhs.\n");
            vkd3d_free(store);
            return NULL;
        }
        else if (lhs->type == HLSL_IR_SWIZZLE)
        {
            struct hlsl_ir_swizzle *swizzle = hlsl_ir_swizzle(lhs), *new_swizzle;
            unsigned int width, s = swizzle->swizzle;

            if (lhs->data_type->type == HLSL_CLASS_MATRIX)
                FIXME("Assignments with writemasks and matrices on lhs are not supported yet.\n");

            if (!invert_swizzle(&s, &writemask, &width))
            {
                hlsl_error(ctx, lhs->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_WRITEMASK, "Invalid writemask.");
                vkd3d_free(store);
                return NULL;
            }

            if (!(new_swizzle = hlsl_new_swizzle(ctx, s, width, rhs, &swizzle->node.loc)))
            {
                vkd3d_free(store);
                return NULL;
            }
            list_add_tail(instrs, &new_swizzle->node.entry);

            lhs = swizzle->val.node;
            rhs = &new_swizzle->node;
        }
        else
        {
            hlsl_error(ctx, lhs->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_LVALUE, "Invalid lvalue.");
            vkd3d_free(store);
            return NULL;
        }
    }

    init_node(&store->node, HLSL_IR_STORE, NULL, lhs->loc);
    store->writemask = writemask;
    store->lhs.var = hlsl_ir_load(lhs)->src.var;
    hlsl_src_from_node(&store->lhs.offset, hlsl_ir_load(lhs)->src.offset.node);
    hlsl_src_from_node(&store->rhs, rhs);
    list_add_tail(instrs, &store->node.entry);

    /* Don't use the instruction itself as a source, as this makes structure
     * splitting easier. Instead copy it here. Since we retrieve sources from
     * the last instruction in the list, we do need to copy. */
    if (!(copy = hlsl_new_copy(ctx, rhs)))
        return NULL;
    list_add_tail(instrs, &copy->node.entry);
    return &copy->node;
}

static bool add_increment(struct hlsl_ctx *ctx, struct list *instrs, bool decrement, bool post,
        struct vkd3d_shader_location loc)
{
    struct hlsl_ir_node *lhs = node_from_list(instrs);
    struct hlsl_ir_constant *one;

    if (lhs->data_type->modifiers & HLSL_MODIFIER_CONST)
        hlsl_error(ctx, loc, VKD3D_SHADER_ERROR_HLSL_MODIFIES_CONST,
                "Argument to %s%screment operator is const.", post ? "post" : "pre", decrement ? "de" : "in");

    if (!(one = hlsl_new_uint_constant(ctx, 1, loc)))
        return false;
    list_add_tail(instrs, &one->node.entry);

    if (!add_assignment(ctx, instrs, lhs, decrement ? ASSIGN_OP_SUB : ASSIGN_OP_ADD, &one->node))
        return false;

    if (post)
    {
        struct hlsl_ir_expr *copy;

        if (!(copy = hlsl_new_copy(ctx, lhs)))
            return false;
        list_add_tail(instrs, &copy->node.entry);

        /* Post increment/decrement expressions are considered const. */
        if (!(copy->node.data_type = hlsl_type_clone(ctx, copy->node.data_type, 0)))
            return false;
        copy->node.data_type->modifiers |= HLSL_MODIFIER_CONST;
    }

    return true;
}

static void struct_var_initializer(struct hlsl_ctx *ctx, struct list *list, struct hlsl_ir_var *var,
        struct parse_initializer *initializer)
{
    struct hlsl_type *type = var->data_type;
    struct hlsl_struct_field *field;
    unsigned int i = 0;

    if (initializer_size(initializer) != hlsl_type_component_count(type))
    {
        hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_WRONG_PARAMETER_COUNT,
                "Expected %u components in initializer, but got %u.",
                hlsl_type_component_count(type), initializer_size(initializer));
        free_parse_initializer(initializer);
        return;
    }

    list_move_tail(list, initializer->instrs);
    vkd3d_free(initializer->instrs);

    LIST_FOR_EACH_ENTRY(field, type->e.elements, struct hlsl_struct_field, entry)
    {
        struct hlsl_ir_node *node = initializer->args[i];
        struct hlsl_ir_store *store;
        struct hlsl_ir_constant *c;

        if (i++ >= initializer->args_count)
            break;

        if (hlsl_type_component_count(field->type) == hlsl_type_component_count(node->data_type))
        {
            if (!(c = hlsl_new_uint_constant(ctx, field->reg_offset * 4, node->loc)))
                break;
            list_add_tail(list, &c->node.entry);

            if (!(store = hlsl_new_store(ctx, var, &c->node, node, 0, node->loc)))
                break;
            list_add_tail(list, &store->node.entry);
        }
        else
            FIXME("Initializing with \"mismatched\" fields is not supported yet.\n");
    }

    vkd3d_free(initializer->args);
}

static void free_parse_variable_def(struct parse_variable_def *v)
{
    free_parse_initializer(&v->initializer);
    vkd3d_free(v->arrays.sizes);
    vkd3d_free(v->name);
    vkd3d_free((void *)v->semantic.name);
    vkd3d_free(v->reg_reservation);
    vkd3d_free(v);
}

static struct list *declare_vars(struct hlsl_ctx *ctx, struct hlsl_type *basic_type,
        DWORD modifiers, struct list *var_list)
{
    struct parse_variable_def *v, *v_next;
    struct hlsl_ir_function_decl *func;
    struct list *statements_list;
    struct hlsl_ir_var *var;
    struct hlsl_type *type;
    bool local = true;

    if (basic_type->type == HLSL_CLASS_MATRIX)
        assert(basic_type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK);

    if (!(statements_list = make_empty_list(ctx)))
    {
        LIST_FOR_EACH_ENTRY_SAFE(v, v_next, var_list, struct parse_variable_def, entry)
            free_parse_variable_def(v);
        vkd3d_free(var_list);
        return NULL;
    }

    if (!var_list)
        return statements_list;

    LIST_FOR_EACH_ENTRY_SAFE(v, v_next, var_list, struct parse_variable_def, entry)
    {
        unsigned int i;

        type = basic_type;
        for (i = 0; i < v->arrays.count; ++i)
            type = hlsl_new_array_type(ctx, type, v->arrays.sizes[i]);

        if (type->type != HLSL_CLASS_MATRIX)
            check_invalid_matrix_modifiers(ctx, modifiers, v->loc);

        if (!(var = hlsl_new_var(ctx, v->name, type, v->loc, &v->semantic, modifiers, v->reg_reservation)))
        {
            free_parse_variable_def(v);
            continue;
        }

        if (ctx->cur_scope == ctx->globals)
        {
            local = false;

            if ((modifiers & HLSL_STORAGE_UNIFORM) && (modifiers & HLSL_STORAGE_STATIC))
                hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                        "Variable '%s' is declared as both \"uniform\" and \"static\".", var->name);

            /* Mark it as uniform. We need to do this here since synthetic
             * variables also get put in the global scope, but shouldn't be
             * considered uniforms, and we have no way of telling otherwise. */
            if (!(modifiers & HLSL_STORAGE_STATIC))
                var->modifiers |= HLSL_STORAGE_UNIFORM;

            if ((func = hlsl_get_func_decl(ctx, var->name)))
            {
                hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                        "'%s' is already defined as a function.", var->name);
                hlsl_note(ctx, func->loc, VKD3D_SHADER_LOG_ERROR,
                        "'%s' was previously defined here.", var->name);
            }
        }
        else
        {
            static const unsigned int invalid = HLSL_STORAGE_EXTERN | HLSL_STORAGE_SHARED
                    | HLSL_STORAGE_GROUPSHARED | HLSL_STORAGE_UNIFORM;

            if (modifiers & invalid)
            {
                struct vkd3d_string_buffer *string;

                if ((string = hlsl_modifiers_to_string(ctx, modifiers & invalid)))
                    hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                            "Modifiers '%s' are not allowed on local variables.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
            }
            if (var->semantic.name)
                hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SEMANTIC,
                        "Semantics are not allowed on local variables.");
        }

        if ((type->modifiers & HLSL_MODIFIER_CONST) && !v->initializer.args_count
                && !(modifiers & (HLSL_STORAGE_STATIC | HLSL_STORAGE_UNIFORM)))
        {
            hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_INITIALIZER,
                    "Const variable \"%s\" is missing an initializer.", var->name);
            hlsl_free_var(var);
            vkd3d_free(v);
            continue;
        }

        if (!hlsl_add_var(ctx, var, local))
        {
            struct hlsl_ir_var *old = hlsl_get_var(ctx->cur_scope, var->name);

            hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                    "Variable \"%s\" was already declared in this scope.", var->name);
            hlsl_note(ctx, old->loc, VKD3D_SHADER_LOG_ERROR, "\"%s\" was previously declared here.", old->name);
            hlsl_free_var(var);
            vkd3d_free(v);
            continue;
        }

        if (v->initializer.args_count)
        {
            unsigned int size = initializer_size(&v->initializer);
            struct hlsl_ir_load *load;

            if (type->type <= HLSL_CLASS_LAST_NUMERIC
                    && type->dimx * type->dimy != size && size != 1)
            {
                if (size < type->dimx * type->dimy)
                {
                    hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_WRONG_PARAMETER_COUNT,
                            "Expected %u components in numeric initializer, but got %u.",
                            type->dimx * type->dimy, size);
                    free_parse_initializer(&v->initializer);
                    vkd3d_free(v);
                    continue;
                }
            }
            if ((type->type == HLSL_CLASS_STRUCT || type->type == HLSL_CLASS_ARRAY)
                    && hlsl_type_component_count(type) != size)
            {
                hlsl_error(ctx, v->loc, VKD3D_SHADER_ERROR_HLSL_WRONG_PARAMETER_COUNT,
                        "Expected %u components in initializer, but got %u.", hlsl_type_component_count(type), size);
                free_parse_initializer(&v->initializer);
                vkd3d_free(v);
                continue;
            }

            if (type->type == HLSL_CLASS_STRUCT)
            {
                struct_var_initializer(ctx, statements_list, var, &v->initializer);
                vkd3d_free(v);
                continue;
            }
            if (type->type > HLSL_CLASS_LAST_NUMERIC)
            {
                FIXME("Initializers for non scalar/struct variables not supported yet.\n");
                free_parse_initializer(&v->initializer);
                vkd3d_free(v);
                continue;
            }
            if (v->arrays.count)
            {
                FIXME("Initializing arrays is not supported yet.\n");
                free_parse_initializer(&v->initializer);
                vkd3d_free(v);
                continue;
            }
            if (v->initializer.args_count > 1)
            {
                FIXME("Complex initializers are not supported yet.\n");
                free_parse_initializer(&v->initializer);
                vkd3d_free(v);
                continue;
            }

            load = hlsl_new_var_load(ctx, var, var->loc);
            list_add_tail(v->initializer.instrs, &load->node.entry);
            add_assignment(ctx, v->initializer.instrs, &load->node, ASSIGN_OP_ASSIGN, v->initializer.args[0]);
            vkd3d_free(v->initializer.args);

            if (modifiers & HLSL_STORAGE_STATIC)
                list_move_tail(&ctx->static_initializers, v->initializer.instrs);
            else
                list_move_tail(statements_list, v->initializer.instrs);
            vkd3d_free(v->initializer.instrs);
        }
        vkd3d_free(v);
    }
    vkd3d_free(var_list);
    return statements_list;
}

}

%locations
%define parse.error verbose
%define api.prefix {hlsl_yy}
%define api.pure full
%expect 1
%lex-param {yyscan_t scanner}
%parse-param {void *scanner}
%parse-param {struct hlsl_ctx *ctx}

%union
{
    struct hlsl_type *type;
    INT intval;
    FLOAT floatval;
    BOOL boolval;
    char *name;
    DWORD modifiers;
    struct hlsl_ir_node *instr;
    struct list *list;
    struct parse_function function;
    struct parse_parameter parameter;
    struct parse_initializer initializer;
    struct parse_array_sizes arrays;
    struct parse_variable_def *variable_def;
    struct parse_if_body if_body;
    enum parse_unary_op unary_op;
    enum parse_assign_op assign_op;
    struct hlsl_reg_reservation *reg_reservation;
    struct parse_colon_attribute colon_attribute;
    struct hlsl_semantic semantic;
}

%token KW_BLENDSTATE
%token KW_BREAK
%token KW_BUFFER
%token KW_CBUFFER
%token KW_COLUMN_MAJOR
%token KW_COMPILE
%token KW_CONST
%token KW_CONTINUE
%token KW_DEPTHSTENCILSTATE
%token KW_DEPTHSTENCILVIEW
%token KW_DISCARD
%token KW_DO
%token KW_DOUBLE
%token KW_ELSE
%token KW_EXTERN
%token KW_FALSE
%token KW_FOR
%token KW_GEOMETRYSHADER
%token KW_GROUPSHARED
%token KW_IF
%token KW_IN
%token KW_INLINE
%token KW_INOUT
%token KW_MATRIX
%token KW_NAMESPACE
%token KW_NOINTERPOLATION
%token KW_OUT
%token KW_PASS
%token KW_PIXELSHADER
%token KW_PRECISE
%token KW_RASTERIZERSTATE
%token KW_RENDERTARGETVIEW
%token KW_RETURN
%token KW_REGISTER
%token KW_ROW_MAJOR
%token KW_SAMPLER
%token KW_SAMPLER1D
%token KW_SAMPLER2D
%token KW_SAMPLER3D
%token KW_SAMPLERCUBE
%token KW_SAMPLER_STATE
%token KW_SAMPLERCOMPARISONSTATE
%token KW_SHARED
%token KW_STATEBLOCK
%token KW_STATEBLOCK_STATE
%token KW_STATIC
%token KW_STRING
%token KW_STRUCT
%token KW_SWITCH
%token KW_TBUFFER
%token KW_TECHNIQUE
%token KW_TECHNIQUE10
%token KW_TEXTURE
%token KW_TEXTURE1D
%token KW_TEXTURE1DARRAY
%token KW_TEXTURE2D
%token KW_TEXTURE2DARRAY
%token KW_TEXTURE2DMS
%token KW_TEXTURE2DMSARRAY
%token KW_TEXTURE3D
%token KW_TEXTURE3DARRAY
%token KW_TEXTURECUBE
%token KW_TRUE
%token KW_TYPEDEF
%token KW_UNIFORM
%token KW_VECTOR
%token KW_VERTEXSHADER
%token KW_VOID
%token KW_VOLATILE
%token KW_WHILE

%token OP_INC
%token OP_DEC
%token OP_AND
%token OP_OR
%token OP_EQ
%token OP_LEFTSHIFT
%token OP_LEFTSHIFTASSIGN
%token OP_RIGHTSHIFT
%token OP_RIGHTSHIFTASSIGN
%token OP_ELLIPSIS
%token OP_LE
%token OP_GE
%token OP_NE
%token OP_ADDASSIGN
%token OP_SUBASSIGN
%token OP_MULASSIGN
%token OP_DIVASSIGN
%token OP_MODASSIGN
%token OP_ANDASSIGN
%token OP_ORASSIGN
%token OP_XORASSIGN
%token OP_UNKNOWN1
%token OP_UNKNOWN2
%token OP_UNKNOWN3
%token OP_UNKNOWN4

%token <floatval> C_FLOAT

%token <intval> C_INTEGER
%token <intval> PRE_LINE

%type <list> add_expr
%type <list> assignment_expr
%type <list> bitand_expr
%type <list> bitor_expr
%type <list> bitxor_expr
%type <list> compound_statement
%type <list> conditional_expr
%type <list> declaration
%type <list> declaration_statement
%type <list> equality_expr
%type <list> expr
%type <list> expr_statement
%type <list> field
%type <list> fields_list
%type <list> initializer_expr
%type <list> jump_statement
%type <list> logicand_expr
%type <list> logicor_expr
%type <list> loop_statement
%type <list> mul_expr
%type <list> param_list
%type <list> parameters
%type <list> postfix_expr
%type <list> primary_expr
%type <list> relational_expr
%type <list> selection_statement
%type <list> shift_expr
%type <list> statement
%type <list> statement_list
%type <list> struct_declaration
%type <list> type_specs
%type <list> unary_expr
%type <list> variables_def
%type <list> variables_def_optional

%token <name> VAR_IDENTIFIER
%token <name> NEW_IDENTIFIER
%token <name> STRING
%token <name> TYPE_IDENTIFIER

%type <arrays> arrays

%type <assign_op> assign_op

%type <boolval> boolean

%type <colon_attribute> colon_attribute

%type <function> func_declaration
%type <function> func_prototype

%type <initializer> complex_initializer
%type <initializer> initializer_expr_list

%type <if_body> if_body

%type <modifiers> input_mod
%type <modifiers> input_mods
%type <modifiers> var_modifiers

%type <name> any_identifier
%type <name> var_identifier

%type <parameter> parameter

%type <reg_reservation> register_opt

%type <semantic> semantic

%type <type> base_type
%type <type> field_type
%type <type> named_struct_spec
%type <type> unnamed_struct_spec
%type <type> struct_spec
%type <type> type
%type <type> typedef_type

%type <unary_op> unary_op

%type <variable_def> type_spec
%type <variable_def> variable_def

%%

hlsl_prog:
      %empty
    | hlsl_prog func_declaration
        {
            const struct hlsl_ir_function_decl *decl;

            decl = get_overloaded_func(&ctx->functions, $2.name, $2.decl->parameters, true);
            if (decl && !decl->func->intrinsic)
            {
                if (decl->body && $2.decl->body)
                {
                    hlsl_error(ctx, $2.decl->loc, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                            "Function \"%s\" is already defined.", $2.name);
                    hlsl_note(ctx, decl->loc, VKD3D_SHADER_LOG_ERROR, "\"%s\" was previously defined here.", $2.name);
                    YYABORT;
                }
                else if (!hlsl_types_are_equal(decl->return_type, $2.decl->return_type))
                {
                    hlsl_error(ctx, $2.decl->loc, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                            "Function \"%s\" was already declared with a different return type.", $2.name);
                    hlsl_note(ctx, decl->loc, VKD3D_SHADER_LOG_ERROR, "\"%s\" was previously declared here.", $2.name);
                    YYABORT;
                }
            }

            hlsl_add_function(ctx, $2.name, $2.decl, false);
        }
    | hlsl_prog declaration_statement
        {
            if (!list_empty($2))
                FIXME("Uniform initializer.\n");
            hlsl_free_instr_list($2);
        }
    | hlsl_prog preproc_directive
    | hlsl_prog ';'

preproc_directive:
      PRE_LINE STRING
        {
            const char **new_array = NULL;

            ctx->location.line = $1;
            if (strcmp($2, ctx->location.source_name))
                new_array = vkd3d_realloc(ctx->source_files,
                        sizeof(*ctx->source_files) * (ctx->source_files_count + 1));

            if (new_array)
            {
                ctx->source_files = new_array;
                ctx->source_files[ctx->source_files_count++] = $2;
                ctx->location.source_name = $2;
            }
            else
            {
                vkd3d_free($2);
            }
        }

struct_declaration:
      var_modifiers struct_spec variables_def_optional ';'
        {
            struct hlsl_type *type;
            DWORD modifiers = $1;

            if (!$3)
            {
                if (!$2->name)
                    hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_INVALID_SYNTAX,
                            "Anonymous struct type must declare a variable.");
                if (modifiers)
                    hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                            "Modifiers are not allowed on struct type declataions.");
            }

            if (!(type = apply_type_modifiers(ctx, $2, &modifiers, @1)))
                YYABORT;
            $$ = declare_vars(ctx, type, modifiers, $3);
        }

struct_spec:
      named_struct_spec
    | unnamed_struct_spec

named_struct_spec:
      KW_STRUCT any_identifier '{' fields_list '}'
        {
            bool ret;

            $$ = hlsl_new_struct_type(ctx, $2, $4);

            if (hlsl_get_var(ctx->cur_scope, $2))
            {
                hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_REDEFINED, "\"%s\" is already declared as a variable.", $2);
                YYABORT;
            }

            ret = hlsl_scope_add_type(ctx->cur_scope, $$);
            if (!ret)
            {
                hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_REDEFINED, "Struct \"%s\" is already defined.", $2);
                YYABORT;
            }
        }

unnamed_struct_spec:
      KW_STRUCT '{' fields_list '}'
        {
            $$ = hlsl_new_struct_type(ctx, NULL, $3);
        }

any_identifier:
      VAR_IDENTIFIER
    | TYPE_IDENTIFIER
    | NEW_IDENTIFIER

fields_list:
      %empty
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
        }
    | fields_list field
        {
            struct hlsl_struct_field *field, *next, *existing;

            $$ = $1;
            LIST_FOR_EACH_ENTRY_SAFE(field, next, $2, struct hlsl_struct_field, entry)
            {
                if ((existing = get_struct_field($$, field->name)))
                {
                    hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                            "Field \"%s\" is already defined.", field->name);
                    hlsl_note(ctx, existing->loc, VKD3D_SHADER_LOG_ERROR,
                            "'%s' was previously defined here.", field->name);
                    vkd3d_free(field);
                }
                else
                {
                    list_add_tail($$, &field->entry);
                }
            }
            vkd3d_free($2);
        }

field_type:
      type
    | unnamed_struct_spec

field:
      var_modifiers field_type variables_def ';'
        {
            struct hlsl_type *type;
            DWORD modifiers = $1;

            if (!(type = apply_type_modifiers(ctx, $2, &modifiers, @1)))
                YYABORT;
            if (modifiers)
            {
                struct vkd3d_string_buffer *string;

                if ((string = hlsl_modifiers_to_string(ctx, modifiers)))
                    hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                            "Modifiers '%s' are not allowed on struct fields.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
            }
            $$ = gen_struct_fields(ctx, type, $3);
        }

func_declaration:
      func_prototype compound_statement
        {
            $$ = $1;
            $$.decl->body = $2;
            hlsl_pop_scope(ctx);
        }
    | func_prototype ';'
        {
            $$ = $1;
            hlsl_pop_scope(ctx);
        }

func_prototype:
    /* var_modifiers is necessary to avoid shift/reduce conflicts. */
      var_modifiers type var_identifier '(' parameters ')' colon_attribute
        {
            struct hlsl_ir_var *var;

            if ($1)
            {
                hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                        "Modifiers are not allowed on functions.");
                YYABORT;
            }
            if ((var = hlsl_get_var(ctx->globals, $3)))
            {
                hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                        "\"%s\" is already declared as a variable.", $3);
                hlsl_note(ctx, var->loc, VKD3D_SHADER_LOG_ERROR,
                        "\"%s\" was previously declared here.", $3);
                YYABORT;
            }
            if (hlsl_type_is_void($2) && $7.semantic.name)
            {
                hlsl_error(ctx, @7, VKD3D_SHADER_ERROR_HLSL_INVALID_SEMANTIC,
                        "Semantics are not allowed on void functions.");
            }

            if ($7.reg_reservation)
            {
                FIXME("Unexpected register reservation for a function.\n");
                vkd3d_free($7.reg_reservation);
            }
            if (!($$.decl = hlsl_new_func_decl(ctx, $2, $5, &$7.semantic, @3)))
                YYABORT;
            $$.name = $3;
            ctx->cur_function = $$.decl;
        }

compound_statement:
      '{' '}'
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
        }
    | '{' scope_start statement_list '}'
        {
            hlsl_pop_scope(ctx);
            $$ = $3;
        }

scope_start:
      %empty
        {
            hlsl_push_scope(ctx);
        }

var_identifier:
      VAR_IDENTIFIER
    | NEW_IDENTIFIER

colon_attribute:
      %empty
        {
            $$.semantic.name = NULL;
            $$.reg_reservation = NULL;
        }
    | semantic
        {
            $$.semantic = $1;
            $$.reg_reservation = NULL;
        }
    | register_opt
        {
            $$.semantic.name = NULL;
            $$.reg_reservation = $1;
        }

semantic:
      ':' any_identifier
        {
            char *p;

            for (p = $2 + strlen($2); p > $2 && isdigit(p[-1]); --p)
                ;
            $$.name = $2;
            $$.index = atoi(p);
            *p = 0;
        }

/* FIXME: Writemasks */
register_opt:
      ':' KW_REGISTER '(' any_identifier ')'
        {
            $$ = parse_reg_reservation(ctx, $4);
            vkd3d_free($4);
        }
    | ':' KW_REGISTER '(' any_identifier ',' any_identifier ')'
        {
            FIXME("Ignoring shader target %s in a register reservation.\n", debugstr_a($4));
            vkd3d_free($4);

            $$ = parse_reg_reservation(ctx, $6);
            vkd3d_free($6);
        }

parameters:
      scope_start
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
        }
    | scope_start param_list
        {
            $$ = $2;
        }

param_list:
      parameter
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
            if (!add_func_parameter(ctx, $$, &$1, @1))
            {
                ERR("Error adding function parameter %s.\n", $1.name);
                YYABORT;
            }
        }
    | param_list ',' parameter
        {
            $$ = $1;
            if (!add_func_parameter(ctx, $$, &$3, @3))
            {
                hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_REDEFINED,
                        "Parameter \"%s\" is already declared.", $3.name);
                YYABORT;
            }
        }

parameter:
      input_mods var_modifiers type any_identifier colon_attribute
        {
            struct hlsl_type *type;
            DWORD modifiers = $2;

            if (!(type = apply_type_modifiers(ctx, $3, &modifiers, @2)))
                YYABORT;

            $$.modifiers = $1 ? $1 : HLSL_STORAGE_IN;
            $$.modifiers |= modifiers;
            $$.type = type;
            $$.name = $4;
            $$.semantic = $5.semantic;
            $$.reg_reservation = $5.reg_reservation;
        }

input_mods:
      %empty
        {
            $$ = 0;
        }
    | input_mods input_mod
        {
            if ($1 & $2)
            {
                struct vkd3d_string_buffer *string;

                if ((string = hlsl_modifiers_to_string(ctx, $2)))
                    hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                            "Modifier \"%s\" was already specified.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
                YYABORT;
            }
            $$ = $1 | $2;
        }

input_mod:
      KW_IN
        {
            $$ = HLSL_STORAGE_IN;
        }
    | KW_OUT
        {
            $$ = HLSL_STORAGE_OUT;
        }
    | KW_INOUT
        {
            $$ = HLSL_STORAGE_IN | HLSL_STORAGE_OUT;
        }

type:
      base_type
        {
            $$ = $1;
        }
    | KW_VECTOR '<' base_type ',' C_INTEGER '>'
        {
            if ($3->type != HLSL_CLASS_SCALAR)
            {
                struct vkd3d_string_buffer *string;

                string = hlsl_type_to_string(ctx, $3);
                if (string)
                    hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                            "Vector base type %s is not scalar.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
                YYABORT;
            }
            if ($5 < 1 || $5 > 4)
            {
                hlsl_error(ctx, @5, VKD3D_SHADER_ERROR_HLSL_INVALID_SIZE,
                        "Vector size %d is not between 1 and 4.", $5);
                YYABORT;
            }

            $$ = hlsl_new_type(ctx, NULL, HLSL_CLASS_VECTOR, $3->base_type, $5, 1);
        }
    | KW_MATRIX '<' base_type ',' C_INTEGER ',' C_INTEGER '>'
        {
            if ($3->type != HLSL_CLASS_SCALAR)
            {
                struct vkd3d_string_buffer *string;

                string = hlsl_type_to_string(ctx, $3);
                if (string)
                    hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                            "Matrix base type %s is not scalar.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
                YYABORT;
            }
            if ($5 < 1 || $5 > 4)
            {
                hlsl_error(ctx, @5, VKD3D_SHADER_ERROR_HLSL_INVALID_SIZE,
                        "Matrix row count %d is not between 1 and 4.", $5);
                YYABORT;
            }
            if ($7 < 1 || $7 > 4)
            {
                hlsl_error(ctx, @7, VKD3D_SHADER_ERROR_HLSL_INVALID_SIZE,
                        "Matrix column count %d is not between 1 and 4.", $7);
                YYABORT;
            }

            $$ = hlsl_new_type(ctx, NULL, HLSL_CLASS_MATRIX, $3->base_type, $7, $5);
        }

base_type:
      KW_VOID
        {
            $$ = ctx->builtin_types.Void;
        }
    | KW_SAMPLER
        {
            $$ = ctx->builtin_types.sampler[HLSL_SAMPLER_DIM_GENERIC];
        }
    | KW_SAMPLER1D
        {
            $$ = ctx->builtin_types.sampler[HLSL_SAMPLER_DIM_1D];
        }
    | KW_SAMPLER2D
        {
            $$ = ctx->builtin_types.sampler[HLSL_SAMPLER_DIM_2D];
        }
    | KW_SAMPLER3D
        {
            $$ = ctx->builtin_types.sampler[HLSL_SAMPLER_DIM_3D];
        }
    | KW_SAMPLERCUBE
        {
            $$ = ctx->builtin_types.sampler[HLSL_SAMPLER_DIM_3D];
        }
    | TYPE_IDENTIFIER
        {
            $$ = hlsl_get_type(ctx->cur_scope, $1, true);
            vkd3d_free($1);
        }
    | KW_STRUCT TYPE_IDENTIFIER
        {
            $$ = hlsl_get_type(ctx->cur_scope, $2, true);
            if ($$->type != HLSL_CLASS_STRUCT)
                hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_REDEFINED, "\"%s\" redefined as a structure.", $2);
            vkd3d_free($2);
        }

declaration_statement:
      declaration
    | struct_declaration
    | typedef
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
        }

typedef_type:
      type
    | struct_spec

typedef:
      KW_TYPEDEF var_modifiers typedef_type type_specs ';'
        {
            if ($2 & ~HLSL_TYPE_MODIFIERS_MASK)
            {
                struct parse_variable_def *v, *v_next;
                hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                        "Storage modifiers are not allowed on typedefs.");
                LIST_FOR_EACH_ENTRY_SAFE(v, v_next, $4, struct parse_variable_def, entry)
                    vkd3d_free(v);
                vkd3d_free($4);
                YYABORT;
            }
            if (!add_typedef(ctx, $2, $3, $4))
                YYABORT;
        }

type_specs:
      type_spec
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
            list_add_head($$, &$1->entry);
        }
    | type_specs ',' type_spec
        {
            $$ = $1;
            list_add_tail($$, &$3->entry);
        }

type_spec:
      any_identifier arrays
        {
            $$ = hlsl_alloc(ctx, sizeof(*$$));
            $$->loc = @1;
            $$->name = $1;
            $$->arrays = $2;
        }

declaration:
      var_modifiers type variables_def ';'
        {
            struct hlsl_type *type;
            DWORD modifiers = $1;

            if (!(type = apply_type_modifiers(ctx, $2, &modifiers, @1)))
                YYABORT;
            $$ = declare_vars(ctx, type, modifiers, $3);
        }

variables_def_optional:
      %empty
        {
            $$ = NULL;
        }
    | variables_def

variables_def:
      variable_def
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
            list_add_head($$, &$1->entry);
        }
    | variables_def ',' variable_def
        {
            $$ = $1;
            list_add_tail($$, &$3->entry);
        }

variable_def:
      any_identifier arrays colon_attribute
        {
            $$ = hlsl_alloc(ctx, sizeof(*$$));
            $$->loc = @1;
            $$->name = $1;
            $$->arrays = $2;
            $$->semantic = $3.semantic;
            $$->reg_reservation = $3.reg_reservation;
        }
    | any_identifier arrays colon_attribute '=' complex_initializer
        {
            $$ = hlsl_alloc(ctx, sizeof(*$$));
            $$->loc = @1;
            $$->name = $1;
            $$->arrays = $2;
            $$->semantic = $3.semantic;
            $$->reg_reservation = $3.reg_reservation;
            $$->initializer = $5;
        }

arrays:
      %empty
        {
            $$.sizes = NULL;
            $$.count = 0;
        }
    | '[' expr ']' arrays
        {
            unsigned int size = evaluate_array_dimension(node_from_list($2));
            uint32_t *new_array;

            hlsl_free_instr_list($2);

            $$ = $4;

            if (!size)
            {
                hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_INVALID_SIZE,
                        "Array size is not a positive integer constant.");
                vkd3d_free($$.sizes);
                YYABORT;
            }

            if (size > 65536)
            {
                hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_INVALID_SIZE,
                        "Array size %u is not between 1 and 65536.", size);
                vkd3d_free($$.sizes);
                YYABORT;
            }

            if (!(new_array = vkd3d_realloc($$.sizes, ($$.count + 1) * sizeof(*new_array))))
            {
                vkd3d_free($$.sizes);
                YYABORT;
            }
            $$.sizes = new_array;
            $$.sizes[$$.count++] = size;
        }

var_modifiers:
      %empty
        {
            $$ = 0;
        }
    | KW_EXTERN var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_EXTERN, @1);
        }
    | KW_NOINTERPOLATION var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_NOINTERPOLATION, @1);
        }
    | KW_PRECISE var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_MODIFIER_PRECISE, @1);
        }
    | KW_SHARED var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_SHARED, @1);
        }
    | KW_GROUPSHARED var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_GROUPSHARED, @1);
        }
    | KW_STATIC var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_STATIC, @1);
        }
    | KW_UNIFORM var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_UNIFORM, @1);
        }
    | KW_VOLATILE var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_STORAGE_VOLATILE, @1);
        }
    | KW_CONST var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_MODIFIER_CONST, @1);
        }
    | KW_ROW_MAJOR var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_MODIFIER_ROW_MAJOR, @1);
        }
    | KW_COLUMN_MAJOR var_modifiers
        {
            $$ = add_modifiers(ctx, $2, HLSL_MODIFIER_COLUMN_MAJOR, @1);
        }

complex_initializer:
      initializer_expr
        {
            $$.args_count = 1;
            if (!($$.args = hlsl_alloc(ctx, sizeof(*$$.args))))
                YYABORT;
            $$.args[0] = node_from_list($1);
            $$.instrs = $1;
        }
    | '{' initializer_expr_list '}'
        {
            $$ = $2;
        }
    | '{' initializer_expr_list ',' '}'
        {
            $$ = $2;
        }

initializer_expr:
      assignment_expr

initializer_expr_list:
      initializer_expr
        {
            $$.args_count = 1;
            if (!($$.args = hlsl_alloc(ctx, sizeof(*$$.args))))
                YYABORT;
            $$.args[0] = node_from_list($1);
            $$.instrs = $1;
        }
    | initializer_expr_list ',' initializer_expr
        {
            $$ = $1;
            if (!($$.args = vkd3d_realloc($$.args, ($$.args_count + 1) * sizeof(*$$.args))))
                YYABORT;
            $$.args[$$.args_count++] = node_from_list($3);
            list_move_tail($$.instrs, $3);
            vkd3d_free($3);
        }

boolean:
      KW_TRUE
        {
            $$ = TRUE;
        }
    | KW_FALSE
        {
            $$ = FALSE;
        }

statement_list:
      statement
    | statement_list statement
        {
            $$ = $1;
            list_move_tail($$, $2);
            vkd3d_free($2);
        }

statement:
      declaration_statement
    | expr_statement
    | compound_statement
    | jump_statement
    | selection_statement
    | loop_statement

jump_statement:
      KW_RETURN expr ';'
        {
            if (!add_return(ctx, $2, node_from_list($2), @1))
                YYABORT;
            $$ = $2;
        }
    | KW_RETURN ';'
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
            if (!add_return(ctx, $$, NULL, @1))
                YYABORT;
        }

selection_statement:
      KW_IF '(' expr ')' if_body
        {
            struct hlsl_ir_node *condition = node_from_list($3);
            struct hlsl_ir_if *instr;

            if (!(instr = hlsl_new_if(ctx, condition, @1)))
                YYABORT;
            list_move_tail(&instr->then_instrs, $5.then_instrs);
            list_move_tail(&instr->else_instrs, $5.else_instrs);
            vkd3d_free($5.then_instrs);
            vkd3d_free($5.else_instrs);
            if (condition->data_type->dimx > 1 || condition->data_type->dimy > 1)
            {
                struct vkd3d_string_buffer *string;

                if ((string = hlsl_type_to_string(ctx, condition->data_type)))
                    hlsl_error(ctx, instr->node.loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                            "if condition type %s is not scalar.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
            }
            $$ = $3;
            list_add_tail($$, &instr->node.entry);
        }

if_body:
      statement
        {
            $$.then_instrs = $1;
            $$.else_instrs = NULL;
        }
    | statement KW_ELSE statement
        {
            $$.then_instrs = $1;
            $$.else_instrs = $3;
        }

loop_statement:
      KW_WHILE '(' expr ')' statement
        {
            $$ = create_loop(ctx, LOOP_WHILE, NULL, $3, NULL, $5, @1);
        }
    | KW_DO statement KW_WHILE '(' expr ')' ';'
        {
            $$ = create_loop(ctx, LOOP_DO_WHILE, NULL, $5, NULL, $2, @1);
        }
    | KW_FOR '(' scope_start expr_statement expr_statement expr ')' statement
        {
            $$ = create_loop(ctx, LOOP_FOR, $4, $5, $6, $8, @1);
            hlsl_pop_scope(ctx);
        }
    | KW_FOR '(' scope_start declaration expr_statement expr ')' statement
        {
            $$ = create_loop(ctx, LOOP_FOR, $4, $5, $6, $8, @1);
            hlsl_pop_scope(ctx);
        }

expr_statement:
      ';'
        {
            if (!($$ = make_empty_list(ctx)))
                YYABORT;
        }
    | expr ';'
        {
            $$ = $1;
        }

primary_expr:
      C_FLOAT
        {
            struct hlsl_ir_constant *c;

            if (!(c = hlsl_alloc(ctx, sizeof(*c))))
                YYABORT;
            init_node(&c->node, HLSL_IR_CONSTANT, ctx->builtin_types.scalar[HLSL_TYPE_FLOAT], @1);
            c->value.f[0] = $1;
            if (!($$ = make_list(ctx, &c->node)))
                YYABORT;
        }
    | C_INTEGER
        {
            struct hlsl_ir_constant *c;

            if (!(c = hlsl_alloc(ctx, sizeof(*c))))
                YYABORT;
            init_node(&c->node, HLSL_IR_CONSTANT, ctx->builtin_types.scalar[HLSL_TYPE_INT], @1);
            c->value.i[0] = $1;
            if (!($$ = make_list(ctx, &c->node)))
                YYABORT;
        }
    | boolean
        {
            struct hlsl_ir_constant *c;

            if (!(c = hlsl_alloc(ctx, sizeof(*c))))
                YYABORT;
            init_node(&c->node, HLSL_IR_CONSTANT, ctx->builtin_types.scalar[HLSL_TYPE_BOOL], @1);
            c->value.b[0] = $1;
            if (!($$ = make_list(ctx, &c->node)))
                YYABORT;
        }
    | VAR_IDENTIFIER
        {
            struct hlsl_ir_load *load;
            struct hlsl_ir_var *var;

            if (!(var = hlsl_get_var(ctx->cur_scope, $1)))
            {
                hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_NOT_DEFINED, "Variable \"%s\" is not defined.", $1);
                YYABORT;
            }
            if ((load = hlsl_new_var_load(ctx, var, @1)))
            {
                if (!($$ = make_list(ctx, &load->node)))
                    YYABORT;
            }
            else
                $$ = NULL;
        }
    | '(' expr ')'
        {
            $$ = $2;
        }

postfix_expr:
      primary_expr
    | postfix_expr OP_INC
        {
            if (!add_increment(ctx, $1, false, true, @2))
            {
                hlsl_free_instr_list($1);
                YYABORT;
            }
            $$ = $1;
        }
    | postfix_expr OP_DEC
        {
            if (!add_increment(ctx, $1, true, true, @2))
            {
                hlsl_free_instr_list($1);
                YYABORT;
            }
            $$ = $1;
        }
    | postfix_expr '.' any_identifier
        {
            struct hlsl_ir_node *node = node_from_list($1);

            if (node->data_type->type == HLSL_CLASS_STRUCT)
            {
                struct hlsl_type *type = node->data_type;
                struct hlsl_struct_field *field;

                if (!(field = get_struct_field(type->e.elements, $3)))
                {
                    hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_NOT_DEFINED, "Field \"%s\" is not defined.", $3);
                    YYABORT;
                }

                if (!add_record_load(ctx, $1, node, field, @2))
                    YYABORT;
                $$ = $1;
            }
            else if (node->data_type->type <= HLSL_CLASS_LAST_NUMERIC)
            {
                struct hlsl_ir_swizzle *swizzle;

                if (!(swizzle = get_swizzle(ctx, node, $3, &@3)))
                {
                    hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_SYNTAX, "Invalid swizzle \"%s\".", $3);
                    YYABORT;
                }
                $$ = append_unop($1, &swizzle->node);
            }
            else
            {
                hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_SYNTAX, "Invalid subscript \"%s\".", $3);
                YYABORT;
            }
        }
    | postfix_expr '[' expr ']'
        {
            struct hlsl_ir_node *array = node_from_list($1), *index = node_from_list($3);
            struct hlsl_ir_expr *cast;

            list_move_tail($1, $3);
            vkd3d_free($3);

            if (index->data_type->type != HLSL_CLASS_SCALAR)
            {
                hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE, "Array index is not scalar.");
                hlsl_free_instr_list($1);
                YYABORT;
            }

            if (!(cast = hlsl_new_cast(ctx, index, ctx->builtin_types.scalar[HLSL_TYPE_UINT], &index->loc)))
            {
                hlsl_free_instr_list($1);
                YYABORT;
            }
            list_add_tail($1, &cast->node.entry);

            if (!add_array_load(ctx, $1, array, &cast->node, @2))
            {
                hlsl_free_instr_list($1);
                YYABORT;
            }
            $$ = $1;
        }

    /* var_modifiers is necessary to avoid shift/reduce conflicts. */
    | var_modifiers type '(' initializer_expr_list ')'
        {
            unsigned int i, writemask_offset = 0;
            struct hlsl_ir_store *store;
            static unsigned int counter;
            struct hlsl_ir_load *load;
            struct hlsl_ir_var *var;
            char name[23];

            if ($1)
            {
                hlsl_error(ctx, @1, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                        "Modifiers are not allowed on constructors.");
                YYABORT;
            }
            if ($2->type > HLSL_CLASS_LAST_NUMERIC)
            {
                struct vkd3d_string_buffer *string;

                if ((string = hlsl_type_to_string(ctx, $2)))
                    hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                            "Constructor data type %s is not numeric.", string->buffer);
                hlsl_release_string_buffer(ctx, string);
                YYABORT;
            }
            if ($2->dimx * $2->dimy != initializer_size(&$4))
            {
                hlsl_error(ctx, @4, VKD3D_SHADER_ERROR_HLSL_WRONG_PARAMETER_COUNT,
                        "Expected %u components in constructor, but got %u.",
                        $2->dimx * $2->dimy, initializer_size(&$4));
                YYABORT;
            }

            if ($2->type == HLSL_CLASS_MATRIX)
                FIXME("Matrix constructors are not supported yet.\n");

            sprintf(name, "<constructor-%x>", counter++);
            if (!(var = hlsl_new_synthetic_var(ctx, name, $2, @2)))
                YYABORT;
            for (i = 0; i < $4.args_count; ++i)
            {
                struct hlsl_ir_node *arg = $4.args[i];
                unsigned int width;

                if (arg->data_type->type == HLSL_CLASS_OBJECT)
                {
                    struct vkd3d_string_buffer *string;

                    if ((string = hlsl_type_to_string(ctx, arg->data_type)))
                        hlsl_error(ctx, arg->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                                "Invalid type %s for constructor argument.", string->buffer);
                    hlsl_release_string_buffer(ctx, string);
                    continue;
                }
                width = hlsl_type_component_count(arg->data_type);

                if (width > 4)
                {
                    FIXME("Constructor argument with %u components.\n", width);
                    continue;
                }

                if (!(arg = add_implicit_conversion(ctx, $4.instrs, arg,
                        ctx->builtin_types.vector[$2->base_type][width - 1], &arg->loc)))
                    continue;

                if (!(store = hlsl_new_store(ctx, var, NULL, arg,
                        ((1 << width) - 1) << writemask_offset, arg->loc)))
                    YYABORT;
                writemask_offset += width;
                list_add_tail($4.instrs, &store->node.entry);
            }
            vkd3d_free($4.args);
            if (!(load = hlsl_new_var_load(ctx, var, @2)))
                YYABORT;
            $$ = append_unop($4.instrs, &load->node);
        }

unary_expr:
      postfix_expr
    | OP_INC unary_expr
        {
            if (!add_increment(ctx, $2, false, false, @1))
            {
                hlsl_free_instr_list($2);
                YYABORT;
            }
            $$ = $2;
        }
    | OP_DEC unary_expr
        {
            if (!add_increment(ctx, $2, true, false, @1))
            {
                hlsl_free_instr_list($2);
                YYABORT;
            }
            $$ = $2;
        }
    | unary_op unary_expr
        {
            enum hlsl_ir_expr_op ops[] = {0, HLSL_IR_UNOP_NEG,
                    HLSL_IR_UNOP_LOGIC_NOT, HLSL_IR_UNOP_BIT_NOT};

            if ($1 == UNARY_OP_PLUS)
                $$ = $2;
            else
                $$ = append_unop($2, hlsl_new_unary_expr(ctx, ops[$1], node_from_list($2), @1));
        }

    /* var_modifiers is necessary to avoid shift/reduce conflicts. */
    | '(' var_modifiers type arrays ')' unary_expr
        {
            struct hlsl_type *src_type = node_from_list($6)->data_type;
            struct hlsl_type *dst_type;
            unsigned int i;

            if ($2)
            {
                hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_MODIFIER,
                        "Modifiers are not allowed on casts.");
                YYABORT;
            }

            dst_type = $3;
            for (i = 0; i < $4.count; ++i)
                dst_type = hlsl_new_array_type(ctx, dst_type, $4.sizes[i]);

            if (!compatible_data_types(src_type, dst_type))
            {
                struct vkd3d_string_buffer *src_string, *dst_string;

                src_string = hlsl_type_to_string(ctx, src_type);
                dst_string = hlsl_type_to_string(ctx, dst_type);
                if (src_string && dst_string)
                    hlsl_error(ctx, @3, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE, "Can't cast from %s to %s.",
                            src_string->buffer, dst_string->buffer);
                hlsl_release_string_buffer(ctx, src_string);
                hlsl_release_string_buffer(ctx, dst_string);
                YYABORT;
            }

            $$ = append_unop($6, &hlsl_new_cast(ctx, node_from_list($6), dst_type, &@3)->node);
        }

unary_op:
      '+'
        {
            $$ = UNARY_OP_PLUS;
        }
    | '-'
        {
            $$ = UNARY_OP_MINUS;
        }
    | '!'
        {
            $$ = UNARY_OP_LOGICNOT;
        }
    | '~'
        {
            $$ = UNARY_OP_BITNOT;
        }

mul_expr:
      unary_expr
    | mul_expr '*' unary_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_MUL, @2);
        }
    | mul_expr '/' unary_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_DIV, @2);
        }
    | mul_expr '%' unary_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_MOD, @2);
        }

add_expr:
      mul_expr
    | add_expr '+' mul_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_ADD, @2);
        }
    | add_expr '-' mul_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_SUB, @2);
        }

shift_expr:
      add_expr
    | shift_expr OP_LEFTSHIFT add_expr
        {
            FIXME("Left shift.\n");
        }
    | shift_expr OP_RIGHTSHIFT add_expr
        {
            FIXME("Right shift.\n");
        }

relational_expr:
      shift_expr
    | relational_expr '<' shift_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_LESS, @2);
        }
    | relational_expr '>' shift_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_GREATER, @2);
        }
    | relational_expr OP_LE shift_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_LEQUAL, @2);
        }
    | relational_expr OP_GE shift_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_GEQUAL, @2);
        }

equality_expr:
      relational_expr
    | equality_expr OP_EQ relational_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_EQUAL, @2);
        }
    | equality_expr OP_NE relational_expr
        {
            $$ = add_binary_expr(ctx, $1, $3, HLSL_IR_BINOP_NEQUAL, @2);
        }

bitand_expr:
      equality_expr
    | bitand_expr '&' equality_expr
        {
            FIXME("Bitwise AND.\n");
        }

bitxor_expr:
      bitand_expr
    | bitxor_expr '^' bitand_expr
        {
            FIXME("Bitwise XOR.\n");
        }

bitor_expr:
      bitxor_expr
    | bitor_expr '|' bitxor_expr
        {
            FIXME("Bitwise OR.\n");
        }

logicand_expr:
      bitor_expr
    | logicand_expr OP_AND bitor_expr
        {
            FIXME("Logical AND.\n");
        }

logicor_expr:
      logicand_expr
    | logicor_expr OP_OR logicand_expr
        {
            FIXME("Logical OR.\n");
        }

conditional_expr:
      logicor_expr
    | logicor_expr '?' expr ':' assignment_expr
        {
            FIXME("Ternary operator.\n");
        }

assignment_expr:

      conditional_expr
    | unary_expr assign_op assignment_expr
        {
            struct hlsl_ir_node *lhs = node_from_list($1), *rhs = node_from_list($3);

            if (lhs->data_type->modifiers & HLSL_MODIFIER_CONST)
            {
                hlsl_error(ctx, @2, VKD3D_SHADER_ERROR_HLSL_MODIFIES_CONST, "Statement modifies a const expression.");
                YYABORT;
            }
            list_move_tail($3, $1);
            vkd3d_free($1);
            if (!add_assignment(ctx, $3, lhs, $2, rhs))
                YYABORT;
            $$ = $3;
        }

assign_op:
      '='
        {
            $$ = ASSIGN_OP_ASSIGN;
        }
    | OP_ADDASSIGN
        {
            $$ = ASSIGN_OP_ADD;
        }
    | OP_SUBASSIGN
        {
            $$ = ASSIGN_OP_SUB;
        }
    | OP_MULASSIGN
        {
            $$ = ASSIGN_OP_MUL;
        }
    | OP_DIVASSIGN
        {
            $$ = ASSIGN_OP_DIV;
        }
    | OP_MODASSIGN
        {
            $$ = ASSIGN_OP_MOD;
        }
    | OP_LEFTSHIFTASSIGN
        {
            $$ = ASSIGN_OP_LSHIFT;
        }
    | OP_RIGHTSHIFTASSIGN
        {
            $$ = ASSIGN_OP_RSHIFT;
        }
    | OP_ANDASSIGN
        {
            $$ = ASSIGN_OP_AND;
        }
    | OP_ORASSIGN
        {
            $$ = ASSIGN_OP_OR;
        }
    | OP_XORASSIGN
        {
            $$ = ASSIGN_OP_XOR;
        }

expr:
      assignment_expr
    | expr ',' assignment_expr
        {
            $$ = $1;
            list_move_tail($$, $3);
            vkd3d_free($3);
        }
