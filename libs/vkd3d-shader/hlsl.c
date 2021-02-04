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

#include "hlsl.h"
#include <stdio.h>

void hlsl_message(const char *fmt, ...)
{
    /* FIXME */
}

void hlsl_report_message(const struct source_location loc,
        enum hlsl_error_level level, const char *fmt, ...)
{
    /* FIXME */

    if (level == HLSL_LEVEL_ERROR)
        set_parse_status(&hlsl_ctx.status, PARSE_ERR);
    else if (level == HLSL_LEVEL_WARNING)
        set_parse_status(&hlsl_ctx.status, PARSE_WARN);
}

bool hlsl_add_var(struct hlsl_scope *scope, struct hlsl_ir_var *decl, bool local_var)
{
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
    {
        if (!strcmp(decl->name, var->name))
            return false;
    }
    if (local_var && scope->upper->upper == hlsl_ctx.globals)
    {
        /* Check whether the variable redefines a function parameter. */
        LIST_FOR_EACH_ENTRY(var, &scope->upper->vars, struct hlsl_ir_var, scope_entry)
        {
            if (!strcmp(decl->name, var->name))
                return false;
        }
    }

    list_add_tail(&scope->vars, &decl->scope_entry);
    return true;
}

struct hlsl_ir_var *hlsl_get_var(struct hlsl_scope *scope, const char *name)
{
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
    {
        if (!strcmp(name, var->name))
            return var;
    }
    if (!scope->upper)
        return NULL;
    return hlsl_get_var(scope->upper, name);
}

void hlsl_free_var(struct hlsl_ir_var *decl)
{
    vkd3d_free((void *)decl->name);
    vkd3d_free((void *)decl->semantic);
    vkd3d_free((void *)decl->reg_reservation);
    vkd3d_free(decl);
}

struct hlsl_type *hlsl_new_type(const char *name, enum hlsl_type_class type_class,
        enum hlsl_base_type base_type, unsigned dimx, unsigned dimy)
{
    struct hlsl_type *type;

    if (!(type = vkd3d_calloc(1, sizeof(*type))))
        return NULL;
    type->name = name;
    type->type = type_class;
    type->base_type = base_type;
    type->dimx = dimx;
    type->dimy = dimy;
    if (type_class == HLSL_CLASS_MATRIX)
        type->reg_size = hlsl_type_is_row_major(type) ? dimy : dimx;
    else
        type->reg_size = 1;

    list_add_tail(&hlsl_ctx.types, &type->entry);

    return type;
}

struct hlsl_type *hlsl_new_array_type(struct hlsl_type *basic_type, unsigned int array_size)
{
    struct hlsl_type *type = hlsl_new_type(NULL, HLSL_CLASS_ARRAY, HLSL_TYPE_FLOAT, 1, 1);

    if (!type)
        return NULL;

    type->modifiers = basic_type->modifiers;
    type->e.array.elements_count = array_size;
    type->e.array.type = basic_type;
    type->reg_size = basic_type->reg_size * array_size;
    type->dimx = basic_type->dimx;
    type->dimy = basic_type->dimy;
    return type;
}

static DWORD get_array_size(const struct hlsl_type *type)
{
    if (type->type == HLSL_CLASS_ARRAY)
        return get_array_size(type->e.array.type) * type->e.array.elements_count;
    return 1;
}

struct hlsl_type *hlsl_new_struct_type(const char *name, struct list *fields)
{
    struct hlsl_struct_field *field;
    unsigned int reg_size = 0;
    struct hlsl_type *type;

    if (!(type = vkd3d_calloc(1, sizeof(*type))))
        return NULL;
    type->type = HLSL_CLASS_STRUCT;
    type->base_type = HLSL_TYPE_VOID;
    type->name = name;
    type->dimx = 0;
    type->dimy = 1;
    type->e.elements = fields;

    LIST_FOR_EACH_ENTRY(field, fields, struct hlsl_struct_field, entry)
    {
        field->reg_offset = reg_size;
        reg_size += field->type->reg_size;
        type->dimx += field->type->dimx * field->type->dimy * get_array_size(field->type);
    }
    type->reg_size = reg_size;

    list_add_tail(&hlsl_ctx.types, &type->entry);

    return type;
}

struct hlsl_type *hlsl_get_type(struct hlsl_scope *scope, const char *name, bool recursive)
{
    struct rb_entry *entry = rb_get(&scope->types, name);

    if (entry)
        return RB_ENTRY_VALUE(entry, struct hlsl_type, scope_entry);

    if (recursive && scope->upper)
        return hlsl_get_type(scope->upper, name, recursive);
    return NULL;
}

bool hlsl_get_function(const char *name)
{
    return rb_get(&hlsl_ctx.functions, name) != NULL;
}

unsigned int hlsl_type_component_count(struct hlsl_type *type)
{
    struct hlsl_struct_field *field;
    unsigned int count = 0;

    if (type->type <= HLSL_CLASS_LAST_NUMERIC)
    {
        return type->dimx * type->dimy;
    }
    if (type->type == HLSL_CLASS_ARRAY)
    {
        return hlsl_type_component_count(type->e.array.type) * type->e.array.elements_count;
    }
    if (type->type != HLSL_CLASS_STRUCT)
    {
        ERR("Unexpected data type %s.\n", debug_hlsl_type(type));
        return 0;
    }

    LIST_FOR_EACH_ENTRY(field, type->e.elements, struct hlsl_struct_field, entry)
    {
        count += hlsl_type_component_count(field->type);
    }
    return count;
}

bool hlsl_type_compare(const struct hlsl_type *t1, const struct hlsl_type *t2)
{
    if (t1 == t2)
        return true;

    if (t1->type != t2->type)
        return false;
    if (t1->base_type != t2->base_type)
        return false;
    if (t1->base_type == HLSL_TYPE_SAMPLER && t1->sampler_dim != t2->sampler_dim)
        return false;
    if ((t1->modifiers & HLSL_MODIFIERS_MAJORITY_MASK)
            != (t2->modifiers & HLSL_MODIFIERS_MAJORITY_MASK))
        return false;
    if (t1->dimx != t2->dimx)
        return false;
    if (t1->dimy != t2->dimy)
        return false;
    if (t1->type == HLSL_CLASS_STRUCT)
    {
        struct list *t1cur, *t2cur;
        struct hlsl_struct_field *t1field, *t2field;

        t1cur = list_head(t1->e.elements);
        t2cur = list_head(t2->e.elements);
        while (t1cur && t2cur)
        {
            t1field = LIST_ENTRY(t1cur, struct hlsl_struct_field, entry);
            t2field = LIST_ENTRY(t2cur, struct hlsl_struct_field, entry);
            if (!hlsl_type_compare(t1field->type, t2field->type))
                return false;
            if (strcmp(t1field->name, t2field->name))
                return false;
            t1cur = list_next(t1->e.elements, t1cur);
            t2cur = list_next(t2->e.elements, t2cur);
        }
        if (t1cur != t2cur)
            return false;
    }
    if (t1->type == HLSL_CLASS_ARRAY)
        return t1->e.array.elements_count == t2->e.array.elements_count
                && hlsl_type_compare(t1->e.array.type, t2->e.array.type);

    return true;
}

struct hlsl_type *hlsl_type_clone(struct hlsl_type *old, unsigned int default_majority)
{
    struct hlsl_struct_field *old_field, *field;
    struct hlsl_type *type;

    if (!(type = vkd3d_calloc(1, sizeof(*type))))
        return NULL;

    if (old->name)
    {
        type->name = vkd3d_strdup(old->name);
        if (!type->name)
        {
            vkd3d_free(type);
            return NULL;
        }
    }
    type->type = old->type;
    type->base_type = old->base_type;
    type->dimx = old->dimx;
    type->dimy = old->dimy;
    type->modifiers = old->modifiers;
    if (!(type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK))
        type->modifiers |= default_majority;
    type->sampler_dim = old->sampler_dim;
    switch (old->type)
    {
        case HLSL_CLASS_ARRAY:
            type->e.array.type = hlsl_type_clone(old->e.array.type, default_majority);
            type->e.array.elements_count = old->e.array.elements_count;
            type->reg_size = type->e.array.elements_count * type->e.array.type->reg_size;
            break;

        case HLSL_CLASS_STRUCT:
        {
            unsigned int reg_size = 0;

            if (!(type->e.elements = vkd3d_malloc(sizeof(*type->e.elements))))
            {
                vkd3d_free((void *)type->name);
                vkd3d_free(type);
                return NULL;
            }
            list_init(type->e.elements);
            LIST_FOR_EACH_ENTRY(old_field, old->e.elements, struct hlsl_struct_field, entry)
            {
                if (!(field = vkd3d_calloc(1, sizeof(*field))))
                {
                    LIST_FOR_EACH_ENTRY_SAFE(field, old_field, type->e.elements, struct hlsl_struct_field, entry)
                    {
                        vkd3d_free((void *)field->semantic);
                        vkd3d_free((void *)field->name);
                        vkd3d_free(field);
                    }
                    vkd3d_free(type->e.elements);
                    vkd3d_free((void *)type->name);
                    vkd3d_free(type);
                    return NULL;
                }
                field->type = hlsl_type_clone(old_field->type, default_majority);
                field->name = vkd3d_strdup(old_field->name);
                if (old_field->semantic)
                    field->semantic = vkd3d_strdup(old_field->semantic);
                field->modifiers = old_field->modifiers;
                field->reg_offset = reg_size;
                reg_size += field->type->reg_size;
                list_add_tail(type->e.elements, &field->entry);
            }
            type->reg_size = reg_size;
            break;
        }

        case HLSL_CLASS_MATRIX:
            type->reg_size = hlsl_type_is_row_major(type) ? type->dimy : type->dimx;
            break;

        default:
            type->reg_size = 1;
            break;
    }

    list_add_tail(&hlsl_ctx.types, &type->entry);
    return type;
}

bool hlsl_scope_add_type(struct hlsl_scope *scope, struct hlsl_type *type)
{
    if (hlsl_get_type(scope, type->name, false))
        return false;

    rb_put(&scope->types, type->name, &type->scope_entry);
    return true;
}

struct hlsl_ir_expr *hlsl_new_cast(struct hlsl_ir_node *node, struct hlsl_type *type,
        struct source_location *loc)
{
    struct hlsl_ir_node *cast;

    cast = hlsl_new_unary_expr(HLSL_IR_UNOP_CAST, node, *loc);
    if (cast)
        cast->data_type = type;
    return hlsl_ir_expr(cast);
}

struct hlsl_ir_var *hlsl_new_var(const char *name, struct hlsl_type *type, const struct source_location loc,
        const char *semantic, unsigned int modifiers, const struct hlsl_reg_reservation *reg_reservation)
{
    struct hlsl_ir_var *var;

    if (!(var = vkd3d_calloc(1, sizeof(*var))))
    {
        hlsl_ctx.status = PARSE_ERR;
        return NULL;
    }

    var->name = name;
    var->data_type = type;
    var->loc = loc;
    var->semantic = semantic;
    var->modifiers = modifiers;
    var->reg_reservation = reg_reservation;
    return var;
}

struct hlsl_ir_var *hlsl_new_synthetic_var(const char *name, struct hlsl_type *type, const struct source_location loc)
{
    struct hlsl_ir_var *var = hlsl_new_var(vkd3d_strdup(name), type, loc, NULL, 0, NULL);

    if (var)
        list_add_tail(&hlsl_ctx.globals->vars, &var->scope_entry);
    return var;
}

static bool type_is_single_reg(const struct hlsl_type *type)
{
    return type->type == HLSL_CLASS_SCALAR || type->type == HLSL_CLASS_VECTOR;
}

struct hlsl_ir_assignment *hlsl_new_assignment(struct hlsl_ir_var *var, struct hlsl_ir_node *offset,
        struct hlsl_ir_node *rhs, unsigned int writemask, struct source_location loc)
{
    struct hlsl_ir_assignment *assign;

    if (!writemask && type_is_single_reg(rhs->data_type))
        writemask = (1 << rhs->data_type->dimx) - 1;

    if (!(assign = vkd3d_malloc(sizeof(*assign))))
        return NULL;

    init_node(&assign->node, HLSL_IR_ASSIGNMENT, NULL, loc);
    assign->lhs.var = var;
    hlsl_src_from_node(&assign->lhs.offset, offset);
    hlsl_src_from_node(&assign->rhs, rhs);
    assign->writemask = writemask;
    return assign;
}

struct hlsl_ir_assignment *hlsl_new_simple_assignment(struct hlsl_ir_var *lhs, struct hlsl_ir_node *rhs)
{
    return hlsl_new_assignment(lhs, NULL, rhs, 0, rhs->loc);
}

struct hlsl_ir_constant *hlsl_new_uint_constant(unsigned int n, const struct source_location loc)
{
    struct hlsl_ir_constant *c;

    if (!(c = vkd3d_malloc(sizeof(*c))))
        return NULL;
    init_node(&c->node, HLSL_IR_CONSTANT, hlsl_ctx.builtin_types.scalar[HLSL_TYPE_UINT], loc);
    c->value.u[0] = n;
    return c;
}

struct hlsl_ir_node *hlsl_new_unary_expr(enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg, struct source_location loc)
{
    struct hlsl_ir_expr *expr;

    if (!(expr = vkd3d_calloc(1, sizeof(*expr))))
        return NULL;
    init_node(&expr->node, HLSL_IR_EXPR, arg->data_type, loc);
    expr->op = op;
    hlsl_src_from_node(&expr->operands[0], arg);
    return &expr->node;
}

struct hlsl_ir_node *hlsl_new_binary_expr(enum hlsl_ir_expr_op op, struct hlsl_ir_node *arg1, struct hlsl_ir_node *arg2)
{
    struct hlsl_ir_expr *expr;

    assert(hlsl_type_compare(arg1->data_type, arg2->data_type));

    if (!(expr = vkd3d_calloc(1, sizeof(*expr))))
        return NULL;
    init_node(&expr->node, HLSL_IR_EXPR, arg1->data_type, arg1->loc);
    expr->op = op;
    hlsl_src_from_node(&expr->operands[0], arg1);
    hlsl_src_from_node(&expr->operands[1], arg2);
    return &expr->node;
}

struct hlsl_ir_if *hlsl_new_if(struct hlsl_ir_node *condition, struct source_location loc)
{
    struct hlsl_ir_if *iff;

    if (!(iff = vkd3d_malloc(sizeof(*iff))))
        return NULL;
    init_node(&iff->node, HLSL_IR_IF, NULL, loc);
    hlsl_src_from_node(&iff->condition, condition);
    list_init(&iff->then_instrs);
    list_init(&iff->else_instrs);
    return iff;
}

struct hlsl_ir_load *hlsl_new_var_load(struct hlsl_ir_var *var, const struct source_location loc)
{
    struct hlsl_ir_load *load;

    if (!(load = vkd3d_calloc(1, sizeof(*load))))
        return NULL;
    init_node(&load->node, HLSL_IR_LOAD, var->data_type, loc);
    load->src.var = var;
    return load;
}

struct hlsl_ir_swizzle *hlsl_new_swizzle(DWORD s, unsigned int components,
        struct hlsl_ir_node *val, struct source_location *loc)
{
    struct hlsl_ir_swizzle *swizzle;

    if (!(swizzle = vkd3d_malloc(sizeof(*swizzle))))
        return NULL;
    init_node(&swizzle->node, HLSL_IR_SWIZZLE,
            hlsl_new_type(NULL, HLSL_CLASS_VECTOR, val->data_type->base_type, components, 1), *loc);
    hlsl_src_from_node(&swizzle->val, val);
    swizzle->swizzle = s;
    return swizzle;
}

bool hlsl_type_is_void(const struct hlsl_type *type)
{
    return type->type == HLSL_CLASS_OBJECT && type->base_type == HLSL_TYPE_VOID;
}

struct hlsl_ir_function_decl *hlsl_new_func_decl(struct hlsl_type *return_type,
        struct list *parameters, const char *semantic, struct source_location loc)
{
    struct hlsl_ir_function_decl *decl;

    if (!(decl = vkd3d_calloc(1, sizeof(*decl))))
        return NULL;
    decl->return_type = return_type;
    decl->parameters = parameters;
    decl->semantic = semantic;
    decl->loc = loc;

    if (!hlsl_type_is_void(return_type))
    {
        struct hlsl_ir_var *return_var;
        char name[28];

        sprintf(name, "<retval-%p>", decl);
        if (!(return_var = hlsl_new_synthetic_var(name, return_type, loc)))
        {
            vkd3d_free(decl);
            return NULL;
        }
        decl->return_var = return_var;
    }

    return decl;
}

static int compare_hlsl_types_rb(const void *key, const struct rb_entry *entry)
{
    const struct hlsl_type *type = RB_ENTRY_VALUE(entry, const struct hlsl_type, scope_entry);
    const char *name = key;

    if (name == type->name)
        return 0;

    if (!name || !type->name)
    {
        ERR("hlsl_type without a name in a scope?\n");
        return -1;
    }
    return strcmp(name, type->name);
}

void hlsl_push_scope(struct hlsl_parse_ctx *ctx)
{
    struct hlsl_scope *new_scope;

    if (!(new_scope = vkd3d_malloc(sizeof(*new_scope))))
        return;
    TRACE("Pushing a new scope.\n");
    list_init(&new_scope->vars);
    rb_init(&new_scope->types, compare_hlsl_types_rb);
    new_scope->upper = ctx->cur_scope;
    ctx->cur_scope = new_scope;
    list_add_tail(&ctx->scopes, &new_scope->entry);
}

void hlsl_pop_scope(struct hlsl_parse_ctx *ctx)
{
    struct hlsl_scope *prev_scope = ctx->cur_scope->upper;

    assert(prev_scope);
    TRACE("Popping current scope.\n");
    ctx->cur_scope = prev_scope;
}

static int compare_param_hlsl_types(const struct hlsl_type *t1, const struct hlsl_type *t2)
{
    if (t1->type != t2->type)
    {
        if (!((t1->type == HLSL_CLASS_SCALAR && t2->type == HLSL_CLASS_VECTOR)
                || (t1->type == HLSL_CLASS_VECTOR && t2->type == HLSL_CLASS_SCALAR)))
            return t1->type - t2->type;
    }
    if (t1->base_type != t2->base_type)
        return t1->base_type - t2->base_type;
    if (t1->base_type == HLSL_TYPE_SAMPLER && t1->sampler_dim != t2->sampler_dim)
        return t1->sampler_dim - t2->sampler_dim;
    if (t1->dimx != t2->dimx)
        return t1->dimx - t2->dimx;
    if (t1->dimy != t2->dimy)
        return t1->dimx - t2->dimx;
    if (t1->type == HLSL_CLASS_STRUCT)
    {
        struct list *t1cur, *t2cur;
        struct hlsl_struct_field *t1field, *t2field;
        int r;

        t1cur = list_head(t1->e.elements);
        t2cur = list_head(t2->e.elements);
        while (t1cur && t2cur)
        {
            t1field = LIST_ENTRY(t1cur, struct hlsl_struct_field, entry);
            t2field = LIST_ENTRY(t2cur, struct hlsl_struct_field, entry);
            if ((r = compare_param_hlsl_types(t1field->type, t2field->type)))
                return r;
            if ((r = strcmp(t1field->name, t2field->name)))
                return r;
            t1cur = list_next(t1->e.elements, t1cur);
            t2cur = list_next(t2->e.elements, t2cur);
        }
        if (t1cur != t2cur)
            return t1cur ? 1 : -1;
        return 0;
    }
    if (t1->type == HLSL_CLASS_ARRAY)
    {
        if (t1->e.array.elements_count != t2->e.array.elements_count)
            return t1->e.array.elements_count - t2->e.array.elements_count;
        return compare_param_hlsl_types(t1->e.array.type, t2->e.array.type);
    }

    return 0;
}

static int compare_function_decl_rb(const void *key, const struct rb_entry *entry)
{
    const struct list *params = key;
    const struct hlsl_ir_function_decl *decl = RB_ENTRY_VALUE(entry, const struct hlsl_ir_function_decl, entry);
    int decl_params_count = decl->parameters ? list_count(decl->parameters) : 0;
    int params_count = params ? list_count(params) : 0;
    struct list *p1cur, *p2cur;
    int r;

    if (params_count != decl_params_count)
        return params_count - decl_params_count;

    p1cur = params ? list_head(params) : NULL;
    p2cur = decl->parameters ? list_head(decl->parameters) : NULL;
    while (p1cur && p2cur)
    {
        struct hlsl_ir_var *p1, *p2;
        p1 = LIST_ENTRY(p1cur, struct hlsl_ir_var, param_entry);
        p2 = LIST_ENTRY(p2cur, struct hlsl_ir_var, param_entry);
        if ((r = compare_param_hlsl_types(p1->data_type, p2->data_type)))
            return r;
        p1cur = list_next(params, p1cur);
        p2cur = list_next(decl->parameters, p2cur);
    }
    return 0;
}

const char *hlsl_base_type_to_string(const struct hlsl_type *type)
{
    const char *name = "(unknown)";

    switch (type->base_type)
    {
        case HLSL_TYPE_FLOAT:        name = "float";         break;
        case HLSL_TYPE_HALF:         name = "half";          break;
        case HLSL_TYPE_DOUBLE:       name = "double";        break;
        case HLSL_TYPE_INT:          name = "int";           break;
        case HLSL_TYPE_UINT:         name = "uint";          break;
        case HLSL_TYPE_BOOL:         name = "bool";          break;
        case HLSL_TYPE_SAMPLER:
            switch (type->sampler_dim)
            {
                case HLSL_SAMPLER_DIM_GENERIC: name = "sampler";       break;
                case HLSL_SAMPLER_DIM_1D:      name = "sampler1D";     break;
                case HLSL_SAMPLER_DIM_2D:      name = "sampler2D";     break;
                case HLSL_SAMPLER_DIM_3D:      name = "sampler3D";     break;
                case HLSL_SAMPLER_DIM_CUBE:    name = "samplerCUBE";   break;
            }
            break;
        default:
            FIXME("Unhandled case %u.\n", type->base_type);
    }
    return name;
}

const char *debug_hlsl_type(const struct hlsl_type *type)
{
    const char *name;

    if (type->name)
        return debugstr_a(type->name);

    if (type->type == HLSL_CLASS_STRUCT)
        return "<anonymous struct>";

    if (type->type == HLSL_CLASS_ARRAY)
    {
        return vkd3d_dbg_sprintf("%s[%u]", hlsl_base_type_to_string(type->e.array.type),
                type->e.array.elements_count);
    }

    name = hlsl_base_type_to_string(type);

    if (type->type == HLSL_CLASS_SCALAR)
        return vkd3d_dbg_sprintf("%s", name);
    if (type->type == HLSL_CLASS_VECTOR)
        return vkd3d_dbg_sprintf("%s%u", name, type->dimx);
    if (type->type == HLSL_CLASS_MATRIX)
        return vkd3d_dbg_sprintf("%s%ux%u", name, type->dimx, type->dimy);
    return "unexpected_type";
}

const char *hlsl_debug_modifiers(DWORD modifiers)
{
    char string[110];

    string[0] = 0;
    if (modifiers & HLSL_STORAGE_EXTERN)
        strcat(string, " extern");                       /* 7 */
    if (modifiers & HLSL_STORAGE_NOINTERPOLATION)
        strcat(string, " nointerpolation");              /* 16 */
    if (modifiers & HLSL_MODIFIER_PRECISE)
        strcat(string, " precise");                      /* 8 */
    if (modifiers & HLSL_STORAGE_SHARED)
        strcat(string, " shared");                       /* 7 */
    if (modifiers & HLSL_STORAGE_GROUPSHARED)
        strcat(string, " groupshared");                  /* 12 */
    if (modifiers & HLSL_STORAGE_STATIC)
        strcat(string, " static");                       /* 7 */
    if (modifiers & HLSL_STORAGE_UNIFORM)
        strcat(string, " uniform");                      /* 8 */
    if (modifiers & HLSL_STORAGE_VOLATILE)
        strcat(string, " volatile");                     /* 9 */
    if (modifiers & HLSL_MODIFIER_CONST)
        strcat(string, " const");                        /* 6 */
    if (modifiers & HLSL_MODIFIER_ROW_MAJOR)
        strcat(string, " row_major");                    /* 10 */
    if (modifiers & HLSL_MODIFIER_COLUMN_MAJOR)
        strcat(string, " column_major");                 /* 13 */
    if ((modifiers & (HLSL_STORAGE_IN | HLSL_STORAGE_OUT)) == (HLSL_STORAGE_IN | HLSL_STORAGE_OUT))
        strcat(string, " inout");                        /* 6 */
    else if (modifiers & HLSL_STORAGE_IN)
        strcat(string, " in");                           /* 3 */
    else if (modifiers & HLSL_STORAGE_OUT)
        strcat(string, " out");                          /* 4 */

    return vkd3d_dbg_sprintf("%s", string[0] ? string + 1 : "");
}

const char *hlsl_node_type_to_string(enum hlsl_ir_node_type type)
{
    static const char * const names[] =
    {
        "HLSL_IR_ASSIGNMENT",
        "HLSL_IR_CONSTANT",
        "HLSL_IR_EXPR",
        "HLSL_IR_IF",
        "HLSL_IR_LOAD",
        "HLSL_IR_LOOP",
        "HLSL_IR_JUMP",
        "HLSL_IR_SWIZZLE",
    };

    if (type >= ARRAY_SIZE(names))
        return "Unexpected node type";
    return names[type];
}

static void dump_instr(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_node *instr);

static void dump_instr_list(struct vkd3d_string_buffer *buffer, const struct list *list)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, list, struct hlsl_ir_node, entry)
    {
        dump_instr(buffer, instr);
        vkd3d_string_buffer_printf(buffer, "\n");
    }
}

static void dump_src(struct vkd3d_string_buffer *buffer, const struct hlsl_src *src)
{
    if (src->node->index)
        vkd3d_string_buffer_printf(buffer, "@%u", src->node->index);
    else
        vkd3d_string_buffer_printf(buffer, "%p", src->node);
}

static void dump_ir_var(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_var *var)
{
    if (var->modifiers)
        vkd3d_string_buffer_printf(buffer, "%s ", hlsl_debug_modifiers(var->modifiers));
    vkd3d_string_buffer_printf(buffer, "%s %s", debug_hlsl_type(var->data_type), var->name);
    if (var->semantic)
        vkd3d_string_buffer_printf(buffer, " : %s", var->semantic);
}

static void dump_deref(struct vkd3d_string_buffer *buffer, const struct hlsl_deref *deref)
{
    if (deref->offset.node)
        /* Print the variable's type for convenience. */
        vkd3d_string_buffer_printf(buffer, "(%s %s)", debug_hlsl_type(deref->var->data_type), deref->var->name);
    else
        vkd3d_string_buffer_printf(buffer, "%s", deref->var->name);
    if (deref->offset.node)
    {
        vkd3d_string_buffer_printf(buffer, "[");
        dump_src(buffer, &deref->offset);
        vkd3d_string_buffer_printf(buffer, "]");
    }
}

static const char *debug_writemask(DWORD writemask)
{
    static const char components[] = {'x', 'y', 'z', 'w'};
    char string[5];
    unsigned int i = 0, pos = 0;

    assert(!(writemask & ~VKD3DSP_WRITEMASK_ALL));

    while (writemask)
    {
        if (writemask & 1)
            string[pos++] = components[i];
        writemask >>= 1;
        i++;
    }
    string[pos] = '\0';
    return vkd3d_dbg_sprintf(".%s", string);
}

static void dump_ir_assignment(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_assignment *assign)
{
    vkd3d_string_buffer_printf(buffer, "= (");
    dump_deref(buffer, &assign->lhs);
    if (assign->writemask != VKD3DSP_WRITEMASK_ALL)
        vkd3d_string_buffer_printf(buffer, "%s", debug_writemask(assign->writemask));
    vkd3d_string_buffer_printf(buffer, " ");
    dump_src(buffer, &assign->rhs);
    vkd3d_string_buffer_printf(buffer, ")");
}

static void dump_ir_constant(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_constant *constant)
{
    struct hlsl_type *type = constant->node.data_type;
    unsigned int x;

    if (type->dimx != 1)
        vkd3d_string_buffer_printf(buffer, "{");
    for (x = 0; x < type->dimx; ++x)
    {
        switch (type->base_type)
        {
            case HLSL_TYPE_BOOL:
                vkd3d_string_buffer_printf(buffer, "%s ", constant->value.b[x] ? "true" : "false");
                break;

            case HLSL_TYPE_DOUBLE:
                vkd3d_string_buffer_printf(buffer, "%.16e ", constant->value.d[x]);
                break;

            case HLSL_TYPE_FLOAT:
                vkd3d_string_buffer_printf(buffer, "%.8e ", constant->value.f[x]);
                break;

            case HLSL_TYPE_INT:
                vkd3d_string_buffer_printf(buffer, "%d ", constant->value.i[x]);
                break;

            case HLSL_TYPE_UINT:
                vkd3d_string_buffer_printf(buffer, "%u ", constant->value.u[x]);
                break;

            default:
                vkd3d_string_buffer_printf(buffer, "Constants of type %s not supported\n",
                        hlsl_base_type_to_string(type));
        }
    }
    if (type->dimx != 1)
        vkd3d_string_buffer_printf(buffer, "}");
}

static const char *debug_expr_op(const struct hlsl_ir_expr *expr)
{
    static const char * const op_names[] =
    {
        "~",
        "!",
        "-",
        "abs",
        "sign",
        "rcp",
        "rsq",
        "sqrt",
        "nrm",
        "exp2",
        "log2",

        "cast",

        "fract",

        "sin",
        "cos",
        "sin_reduced",
        "cos_reduced",

        "dsx",
        "dsy",

        "sat",

        "pre++",
        "pre--",
        "post++",
        "post--",

        "+",
        "-",
        "*",
        "/",

        "%",

        "<",
        ">",
        "<=",
        ">=",
        "==",
        "!=",

        "&&",
        "||",

        "<<",
        ">>",
        "&",
        "|",
        "^",

        "dot",
        "crs",
        "min",
        "max",

        "pow",

        "lerp",

        ",",
    };

    if (expr->op == HLSL_IR_UNOP_CAST)
        return debug_hlsl_type(expr->node.data_type);

    return op_names[expr->op];
}

static void dump_ir_expr(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_expr *expr)
{
    unsigned int i;

    vkd3d_string_buffer_printf(buffer, "%s (", debug_expr_op(expr));
    for (i = 0; i < 3 && expr->operands[i].node; ++i)
    {
        dump_src(buffer, &expr->operands[i]);
        vkd3d_string_buffer_printf(buffer, " ");
    }
    vkd3d_string_buffer_printf(buffer, ")");
}

static void dump_ir_if(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_if *if_node)
{
    vkd3d_string_buffer_printf(buffer, "if (");
    dump_src(buffer, &if_node->condition);
    vkd3d_string_buffer_printf(buffer, ")\n{\n");
    dump_instr_list(buffer, &if_node->then_instrs);
    vkd3d_string_buffer_printf(buffer, "}\nelse\n{\n");
    dump_instr_list(buffer, &if_node->else_instrs);
    vkd3d_string_buffer_printf(buffer, "}\n");
}

static void dump_ir_jump(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_jump *jump)
{
    switch (jump->type)
    {
        case HLSL_IR_JUMP_BREAK:
            vkd3d_string_buffer_printf(buffer, "break");
            break;

        case HLSL_IR_JUMP_CONTINUE:
            vkd3d_string_buffer_printf(buffer, "continue");
            break;

        case HLSL_IR_JUMP_DISCARD:
            vkd3d_string_buffer_printf(buffer, "discard");
            break;

        case HLSL_IR_JUMP_RETURN:
            vkd3d_string_buffer_printf(buffer, "return");
            break;
    }
}

static void dump_ir_loop(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_loop *loop)
{
    vkd3d_string_buffer_printf(buffer, "for (;;)\n{\n");
    dump_instr_list(buffer, &loop->body);
    vkd3d_string_buffer_printf(buffer, "}\n");
}

static void dump_ir_swizzle(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_swizzle *swizzle)
{
    unsigned int i;

    dump_src(buffer, &swizzle->val);
    vkd3d_string_buffer_printf(buffer, ".");
    if (swizzle->val.node->data_type->dimy > 1)
    {
        for (i = 0; i < swizzle->node.data_type->dimx; ++i)
            vkd3d_string_buffer_printf(buffer, "_m%u%u", (swizzle->swizzle >> i * 8) & 0xf, (swizzle->swizzle >> (i * 8 + 4)) & 0xf);
    }
    else
    {
        static const char c[] = {'x', 'y', 'z', 'w'};

        for (i = 0; i < swizzle->node.data_type->dimx; ++i)
            vkd3d_string_buffer_printf(buffer, "%c", c[(swizzle->swizzle >> i * 2) & 0x3]);
    }
}

static void dump_instr(struct vkd3d_string_buffer *buffer, const struct hlsl_ir_node *instr)
{
    if (instr->index)
        vkd3d_string_buffer_printf(buffer, "%4u: ", instr->index);
    else
        vkd3d_string_buffer_printf(buffer, "%p: ", instr);

    vkd3d_string_buffer_printf(buffer, "%10s | ", instr->data_type ? debug_hlsl_type(instr->data_type) : "");

    switch (instr->type)
    {
        case HLSL_IR_ASSIGNMENT:
            dump_ir_assignment(buffer, hlsl_ir_assignment(instr));
            break;

        case HLSL_IR_CONSTANT:
            dump_ir_constant(buffer, hlsl_ir_constant(instr));
            break;

        case HLSL_IR_EXPR:
            dump_ir_expr(buffer, hlsl_ir_expr(instr));
            break;

        case HLSL_IR_IF:
            dump_ir_if(buffer, hlsl_ir_if(instr));
            break;

        case HLSL_IR_JUMP:
            dump_ir_jump(buffer, hlsl_ir_jump(instr));
            break;

        case HLSL_IR_LOAD:
            dump_deref(buffer, &hlsl_ir_load(instr)->src);
            break;

        case HLSL_IR_LOOP:
            dump_ir_loop(buffer, hlsl_ir_loop(instr));
            break;

        case HLSL_IR_SWIZZLE:
            dump_ir_swizzle(buffer, hlsl_ir_swizzle(instr));
            break;

        default:
            vkd3d_string_buffer_printf(buffer, "<No dump function for %s>", hlsl_node_type_to_string(instr->type));
    }
}

void hlsl_dump_function(const struct hlsl_ir_function_decl *func)
{
    struct vkd3d_string_buffer buffer;
    struct hlsl_ir_var *param;

    vkd3d_string_buffer_init(&buffer);
    vkd3d_string_buffer_printf(&buffer, "Dumping function %s.\n", func->func->name);
    vkd3d_string_buffer_printf(&buffer, "Function parameters:\n");
    LIST_FOR_EACH_ENTRY(param, func->parameters, struct hlsl_ir_var, param_entry)
    {
        dump_ir_var(&buffer, param);
        vkd3d_string_buffer_printf(&buffer, "\n");
    }
    if (func->semantic)
        vkd3d_string_buffer_printf(&buffer, "Function semantic: %s\n", func->semantic);
    if (func->body)
        dump_instr_list(&buffer, func->body);

    vkd3d_string_buffer_trace(&buffer);
    vkd3d_string_buffer_cleanup(&buffer);
}

void hlsl_free_type(struct hlsl_type *type)
{
    struct hlsl_struct_field *field, *next_field;

    vkd3d_free((void *)type->name);
    if (type->type == HLSL_CLASS_STRUCT)
    {
        LIST_FOR_EACH_ENTRY_SAFE(field, next_field, type->e.elements, struct hlsl_struct_field, entry)
        {
            vkd3d_free((void *)field->name);
            vkd3d_free((void *)field->semantic);
            vkd3d_free(field);
        }
    }
    vkd3d_free(type);
}

void hlsl_free_instr_list(struct list *list)
{
    struct hlsl_ir_node *node, *next_node;

    if (!list)
        return;
    /* Iterate in reverse, to avoid use-after-free when unlinking sources from
     * the "uses" list. */
    LIST_FOR_EACH_ENTRY_SAFE_REV(node, next_node, list, struct hlsl_ir_node, entry)
        hlsl_free_instr(node);
    vkd3d_free(list);
}

static void free_ir_assignment(struct hlsl_ir_assignment *assignment)
{
    hlsl_src_remove(&assignment->rhs);
    hlsl_src_remove(&assignment->lhs.offset);
    vkd3d_free(assignment);
}

static void free_ir_constant(struct hlsl_ir_constant *constant)
{
    vkd3d_free(constant);
}

static void free_ir_expr(struct hlsl_ir_expr *expr)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(expr->operands); ++i)
        hlsl_src_remove(&expr->operands[i]);
    vkd3d_free(expr);
}

static void free_ir_if(struct hlsl_ir_if *if_node)
{
    struct hlsl_ir_node *node, *next_node;

    LIST_FOR_EACH_ENTRY_SAFE(node, next_node, &if_node->then_instrs, struct hlsl_ir_node, entry)
        hlsl_free_instr(node);
    LIST_FOR_EACH_ENTRY_SAFE(node, next_node, &if_node->else_instrs, struct hlsl_ir_node, entry)
        hlsl_free_instr(node);
    hlsl_src_remove(&if_node->condition);
    vkd3d_free(if_node);
}

static void free_ir_jump(struct hlsl_ir_jump *jump)
{
    vkd3d_free(jump);
}

static void free_ir_load(struct hlsl_ir_load *load)
{
    hlsl_src_remove(&load->src.offset);
    vkd3d_free(load);
}

static void free_ir_loop(struct hlsl_ir_loop *loop)
{
    struct hlsl_ir_node *node, *next_node;

    LIST_FOR_EACH_ENTRY_SAFE(node, next_node, &loop->body, struct hlsl_ir_node, entry)
        hlsl_free_instr(node);
    vkd3d_free(loop);
}

static void free_ir_swizzle(struct hlsl_ir_swizzle *swizzle)
{
    hlsl_src_remove(&swizzle->val);
    vkd3d_free(swizzle);
}

void hlsl_free_instr(struct hlsl_ir_node *node)
{
    switch (node->type)
    {
        case HLSL_IR_ASSIGNMENT:
            free_ir_assignment(hlsl_ir_assignment(node));
            break;

        case HLSL_IR_CONSTANT:
            free_ir_constant(hlsl_ir_constant(node));
            break;

        case HLSL_IR_EXPR:
            free_ir_expr(hlsl_ir_expr(node));
            break;

        case HLSL_IR_IF:
            free_ir_if(hlsl_ir_if(node));
            break;

        case HLSL_IR_JUMP:
            free_ir_jump(hlsl_ir_jump(node));
            break;

        case HLSL_IR_LOAD:
            free_ir_load(hlsl_ir_load(node));
            break;

        case HLSL_IR_LOOP:
            free_ir_loop(hlsl_ir_loop(node));
            break;

        case HLSL_IR_SWIZZLE:
            free_ir_swizzle(hlsl_ir_swizzle(node));
            break;

        default:
            FIXME("Unsupported node type %s.\n", hlsl_node_type_to_string(node->type));
    }
}

static void free_function_decl(struct hlsl_ir_function_decl *decl)
{
    vkd3d_free((void *)decl->semantic);
    vkd3d_free(decl->parameters);
    hlsl_free_instr_list(decl->body);
    vkd3d_free(decl);
}

static void free_function_decl_rb(struct rb_entry *entry, void *context)
{
    free_function_decl(RB_ENTRY_VALUE(entry, struct hlsl_ir_function_decl, entry));
}

static void free_function(struct hlsl_ir_function *func)
{
    rb_destroy(&func->overloads, free_function_decl_rb, NULL);
    vkd3d_free((void *)func->name);
    vkd3d_free(func);
}

static void free_function_rb(struct rb_entry *entry, void *context)
{
    free_function(RB_ENTRY_VALUE(entry, struct hlsl_ir_function, entry));
}

void hlsl_add_function(struct rb_tree *funcs, char *name, struct hlsl_ir_function_decl *decl, bool intrinsic)
{
    struct hlsl_ir_function *func;
    struct rb_entry *func_entry, *old_entry;

    func_entry = rb_get(funcs, name);
    if (func_entry)
    {
        func = RB_ENTRY_VALUE(func_entry, struct hlsl_ir_function, entry);
        if (intrinsic != func->intrinsic)
        {
            if (intrinsic)
            {
                ERR("Redeclaring a user defined function as an intrinsic.\n");
                return;
            }
            TRACE("Function %s redeclared as a user defined function.\n", debugstr_a(name));
            func->intrinsic = intrinsic;
            rb_destroy(&func->overloads, free_function_decl_rb, NULL);
            rb_init(&func->overloads, compare_function_decl_rb);
        }
        decl->func = func;
        if ((old_entry = rb_get(&func->overloads, decl->parameters)))
        {
            struct hlsl_ir_function_decl *old_decl =
                    RB_ENTRY_VALUE(old_entry, struct hlsl_ir_function_decl, entry);

            if (!decl->body)
            {
                free_function_decl(decl);
                vkd3d_free(name);
                return;
            }
            rb_remove(&func->overloads, old_entry);
            free_function_decl(old_decl);
        }
        rb_put(&func->overloads, decl->parameters, &decl->entry);
        vkd3d_free(name);
        return;
    }
    func = vkd3d_malloc(sizeof(*func));
    func->name = name;
    rb_init(&func->overloads, compare_function_decl_rb);
    decl->func = func;
    rb_put(&func->overloads, decl->parameters, &decl->entry);
    func->intrinsic = intrinsic;
    rb_put(funcs, func->name, &func->entry);
}

struct hlsl_profile_info
{
    const char *name;
    enum vkd3d_shader_type type;
    DWORD sm_major;
    DWORD sm_minor;
    DWORD level_major;
    DWORD level_minor;
    bool sw;
};

static const struct hlsl_profile_info *get_target_info(const char *target)
{
    unsigned int i;

    static const struct hlsl_profile_info profiles[] =
    {
        {"cs_4_0",              VKD3D_SHADER_TYPE_COMPUTE,  4, 0, 0, 0, false},
        {"cs_4_1",              VKD3D_SHADER_TYPE_COMPUTE,  4, 1, 0, 0, false},
        {"cs_5_0",              VKD3D_SHADER_TYPE_COMPUTE,  5, 0, 0, 0, false},
        {"ds_5_0",              VKD3D_SHADER_TYPE_DOMAIN,   5, 0, 0, 0, false},
        {"fx_2_0",              VKD3D_SHADER_TYPE_EFFECT,   2, 0, 0, 0, false},
        {"fx_4_0",              VKD3D_SHADER_TYPE_EFFECT,   4, 0, 0, 0, false},
        {"fx_4_1",              VKD3D_SHADER_TYPE_EFFECT,   4, 1, 0, 0, false},
        {"fx_5_0",              VKD3D_SHADER_TYPE_EFFECT,   5, 0, 0, 0, false},
        {"gs_4_0",              VKD3D_SHADER_TYPE_GEOMETRY, 4, 0, 0, 0, false},
        {"gs_4_1",              VKD3D_SHADER_TYPE_GEOMETRY, 4, 1, 0, 0, false},
        {"gs_5_0",              VKD3D_SHADER_TYPE_GEOMETRY, 5, 0, 0, 0, false},
        {"hs_5_0",              VKD3D_SHADER_TYPE_HULL,     5, 0, 0, 0, false},
        {"ps.1.0",              VKD3D_SHADER_TYPE_PIXEL,    1, 0, 0, 0, false},
        {"ps.1.1",              VKD3D_SHADER_TYPE_PIXEL,    1, 1, 0, 0, false},
        {"ps.1.2",              VKD3D_SHADER_TYPE_PIXEL,    1, 2, 0, 0, false},
        {"ps.1.3",              VKD3D_SHADER_TYPE_PIXEL,    1, 3, 0, 0, false},
        {"ps.1.4",              VKD3D_SHADER_TYPE_PIXEL,    1, 4, 0, 0, false},
        {"ps.2.0",              VKD3D_SHADER_TYPE_PIXEL,    2, 0, 0, 0, false},
        {"ps.2.a",              VKD3D_SHADER_TYPE_PIXEL,    2, 1, 0, 0, false},
        {"ps.2.b",              VKD3D_SHADER_TYPE_PIXEL,    2, 2, 0, 0, false},
        {"ps.2.sw",             VKD3D_SHADER_TYPE_PIXEL,    2, 0, 0, 0, true},
        {"ps.3.0",              VKD3D_SHADER_TYPE_PIXEL,    3, 0, 0, 0, false},
        {"ps_1_0",              VKD3D_SHADER_TYPE_PIXEL,    1, 0, 0, 0, false},
        {"ps_1_1",              VKD3D_SHADER_TYPE_PIXEL,    1, 1, 0, 0, false},
        {"ps_1_2",              VKD3D_SHADER_TYPE_PIXEL,    1, 2, 0, 0, false},
        {"ps_1_3",              VKD3D_SHADER_TYPE_PIXEL,    1, 3, 0, 0, false},
        {"ps_1_4",              VKD3D_SHADER_TYPE_PIXEL,    1, 4, 0, 0, false},
        {"ps_2_0",              VKD3D_SHADER_TYPE_PIXEL,    2, 0, 0, 0, false},
        {"ps_2_a",              VKD3D_SHADER_TYPE_PIXEL,    2, 1, 0, 0, false},
        {"ps_2_b",              VKD3D_SHADER_TYPE_PIXEL,    2, 2, 0, 0, false},
        {"ps_2_sw",             VKD3D_SHADER_TYPE_PIXEL,    2, 0, 0, 0, true},
        {"ps_3_0",              VKD3D_SHADER_TYPE_PIXEL,    3, 0, 0, 0, false},
        {"ps_3_sw",             VKD3D_SHADER_TYPE_PIXEL,    3, 0, 0, 0, true},
        {"ps_4_0",              VKD3D_SHADER_TYPE_PIXEL,    4, 0, 0, 0, false},
        {"ps_4_0_level_9_0",    VKD3D_SHADER_TYPE_PIXEL,    4, 0, 9, 0, false},
        {"ps_4_0_level_9_1",    VKD3D_SHADER_TYPE_PIXEL,    4, 0, 9, 1, false},
        {"ps_4_0_level_9_3",    VKD3D_SHADER_TYPE_PIXEL,    4, 0, 9, 3, false},
        {"ps_4_1",              VKD3D_SHADER_TYPE_PIXEL,    4, 1, 0, 0, false},
        {"ps_5_0",              VKD3D_SHADER_TYPE_PIXEL,    5, 0, 0, 0, false},
        {"tx_1_0",              VKD3D_SHADER_TYPE_TEXTURE,  1, 0, 0, 0, false},
        {"vs.1.0",              VKD3D_SHADER_TYPE_VERTEX,   1, 0, 0, 0, false},
        {"vs.1.1",              VKD3D_SHADER_TYPE_VERTEX,   1, 1, 0, 0, false},
        {"vs.2.0",              VKD3D_SHADER_TYPE_VERTEX,   2, 0, 0, 0, false},
        {"vs.2.a",              VKD3D_SHADER_TYPE_VERTEX,   2, 1, 0, 0, false},
        {"vs.2.sw",             VKD3D_SHADER_TYPE_VERTEX,   2, 0, 0, 0, true},
        {"vs.3.0",              VKD3D_SHADER_TYPE_VERTEX,   3, 0, 0, 0, false},
        {"vs.3.sw",             VKD3D_SHADER_TYPE_VERTEX,   3, 0, 0, 0, true},
        {"vs_1_0",              VKD3D_SHADER_TYPE_VERTEX,   1, 0, 0, 0, false},
        {"vs_1_1",              VKD3D_SHADER_TYPE_VERTEX,   1, 1, 0, 0, false},
        {"vs_2_0",              VKD3D_SHADER_TYPE_VERTEX,   2, 0, 0, 0, false},
        {"vs_2_a",              VKD3D_SHADER_TYPE_VERTEX,   2, 1, 0, 0, false},
        {"vs_2_sw",             VKD3D_SHADER_TYPE_VERTEX,   2, 0, 0, 0, true},
        {"vs_3_0",              VKD3D_SHADER_TYPE_VERTEX,   3, 0, 0, 0, false},
        {"vs_3_sw",             VKD3D_SHADER_TYPE_VERTEX,   3, 0, 0, 0, true},
        {"vs_4_0",              VKD3D_SHADER_TYPE_VERTEX,   4, 0, 0, 0, false},
        {"vs_4_0_level_9_0",    VKD3D_SHADER_TYPE_VERTEX,   4, 0, 9, 0, false},
        {"vs_4_0_level_9_1",    VKD3D_SHADER_TYPE_VERTEX,   4, 0, 9, 1, false},
        {"vs_4_0_level_9_3",    VKD3D_SHADER_TYPE_VERTEX,   4, 0, 9, 3, false},
        {"vs_4_1",              VKD3D_SHADER_TYPE_VERTEX,   4, 1, 0, 0, false},
        {"vs_5_0",              VKD3D_SHADER_TYPE_VERTEX,   5, 0, 0, 0, false},
    };

    for (i = 0; i < ARRAY_SIZE(profiles); ++i)
    {
        if (!strcmp(target, profiles[i].name))
            return &profiles[i];
    }

    return NULL;
}

static int compare_function_rb(const void *key, const struct rb_entry *entry)
{
    const char *name = key;
    const struct hlsl_ir_function *func = RB_ENTRY_VALUE(entry, const struct hlsl_ir_function,entry);

    return strcmp(name, func->name);
}

static void declare_predefined_types(struct hlsl_scope *scope)
{
    struct hlsl_type *type;
    unsigned int x, y, bt;
    static const char * const names[] =
    {
        "float",
        "half",
        "double",
        "int",
        "uint",
        "bool",
    };
    char name[10];

    static const char *const sampler_names[] =
    {
        "sampler",
        "sampler1D",
        "sampler2D",
        "sampler3D",
        "samplerCUBE"
    };

    for (bt = 0; bt <= HLSL_TYPE_LAST_SCALAR; ++bt)
    {
        for (y = 1; y <= 4; ++y)
        {
            for (x = 1; x <= 4; ++x)
            {
                sprintf(name, "%s%ux%u", names[bt], y, x);
                type = hlsl_new_type(vkd3d_strdup(name), HLSL_CLASS_MATRIX, bt, x, y);
                hlsl_scope_add_type(scope, type);

                if (y == 1)
                {
                    sprintf(name, "%s%u", names[bt], x);
                    type = hlsl_new_type(vkd3d_strdup(name), HLSL_CLASS_VECTOR, bt, x, y);
                    hlsl_scope_add_type(scope, type);
                    hlsl_ctx.builtin_types.vector[bt][x - 1] = type;

                    if (x == 1)
                    {
                        sprintf(name, "%s", names[bt]);
                        type = hlsl_new_type(vkd3d_strdup(name), HLSL_CLASS_SCALAR, bt, x, y);
                        hlsl_scope_add_type(scope, type);
                        hlsl_ctx.builtin_types.scalar[bt] = type;
                    }
                }
            }
        }
    }

    for (bt = 0; bt <= HLSL_SAMPLER_DIM_MAX; ++bt)
    {
        type = hlsl_new_type(vkd3d_strdup(sampler_names[bt]), HLSL_CLASS_OBJECT, HLSL_TYPE_SAMPLER, 1, 1);
        type->sampler_dim = bt;
        hlsl_ctx.builtin_types.sampler[bt] = type;
    }

    hlsl_ctx.builtin_types.Void = hlsl_new_type(vkd3d_strdup("void"), HLSL_CLASS_OBJECT, HLSL_TYPE_VOID, 1, 1);

    /* DX8 effects predefined types */
    type = hlsl_new_type(vkd3d_strdup("DWORD"), HLSL_CLASS_SCALAR, HLSL_TYPE_INT, 1, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("FLOAT"), HLSL_CLASS_SCALAR, HLSL_TYPE_FLOAT, 1, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("VECTOR"), HLSL_CLASS_VECTOR, HLSL_TYPE_FLOAT, 4, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("MATRIX"), HLSL_CLASS_MATRIX, HLSL_TYPE_FLOAT, 4, 4);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("STRING"), HLSL_CLASS_OBJECT, HLSL_TYPE_STRING, 1, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("TEXTURE"), HLSL_CLASS_OBJECT, HLSL_TYPE_TEXTURE, 1, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("PIXELSHADER"), HLSL_CLASS_OBJECT, HLSL_TYPE_PIXELSHADER, 1, 1);
    hlsl_scope_add_type(scope, type);
    type = hlsl_new_type(vkd3d_strdup("VERTEXSHADER"), HLSL_CLASS_OBJECT, HLSL_TYPE_VERTEXSHADER, 1, 1);
    hlsl_scope_add_type(scope, type);
}

static bool hlsl_ctx_init(struct hlsl_parse_ctx *ctx, struct vkd3d_shader_message_context *message_context)
{
    memset(ctx, 0, sizeof(*ctx));

    ctx->message_context = message_context;

    ctx->line_no = ctx->column = 1;
    if (!(ctx->source_file = vkd3d_strdup("")))
        return false;
    if (!(ctx->source_files = vkd3d_malloc(sizeof(*ctx->source_files))))
    {
        vkd3d_free((void *)ctx->source_file);
        return false;
    }
    ctx->source_files[0] = ctx->source_file;
    ctx->source_files_count = 1;

    ctx->matrix_majority = HLSL_COLUMN_MAJOR;

    list_init(&ctx->scopes);
    hlsl_push_scope(ctx);
    ctx->globals = ctx->cur_scope;

    list_init(&ctx->types);
    declare_predefined_types(ctx->globals);

    rb_init(&ctx->functions, compare_function_rb);

    list_init(&ctx->static_initializers);

    return true;
}

static void hlsl_ctx_cleanup(struct hlsl_parse_ctx *ctx)
{
    struct hlsl_scope *scope, *next_scope;
    struct hlsl_ir_var *var, *next_var;
    struct hlsl_type *type, *next_type;
    unsigned int i;

    for (i = 0; i < ctx->source_files_count; ++i)
        vkd3d_free((void *)ctx->source_files[i]);
    vkd3d_free(ctx->source_files);

    rb_destroy(&ctx->functions, free_function_rb, NULL);

    LIST_FOR_EACH_ENTRY_SAFE(scope, next_scope, &ctx->scopes, struct hlsl_scope, entry)
    {
        LIST_FOR_EACH_ENTRY_SAFE(var, next_var, &scope->vars, struct hlsl_ir_var, scope_entry)
            hlsl_free_var(var);
        rb_destroy(&scope->types, NULL, NULL);
        vkd3d_free(scope);
    }

    LIST_FOR_EACH_ENTRY_SAFE(type, next_type, &ctx->types, struct hlsl_type, entry)
        hlsl_free_type(type);
}

int hlsl_compile_shader(const char *text, const struct vkd3d_shader_compile_info *compile_info,
        struct vkd3d_shader_code *dxbc, struct vkd3d_shader_message_context *message_context)
{
    const struct vkd3d_shader_hlsl_source_info *hlsl_source_info;
    const struct hlsl_profile_info *profile;
    int ret;

    if (!(hlsl_source_info = vkd3d_find_struct(compile_info->next, HLSL_SOURCE_INFO)))
    {
        ERR("No HLSL source info given.\n");
        return VKD3D_ERROR_INVALID_ARGUMENT;
    }

    if (!(profile = get_target_info(hlsl_source_info->profile)))
    {
        FIXME("Unknown compilation target %s.\n", debugstr_a(hlsl_source_info->profile));
        return VKD3D_ERROR_NOT_IMPLEMENTED;
    }

    vkd3d_shader_dump_shader(profile->type, &compile_info->source);

    if (!hlsl_ctx_init(&hlsl_ctx, message_context))
        return VKD3D_ERROR_OUT_OF_MEMORY;

    ret = hlsl_lexer_compile(text, hlsl_source_info->entry_point ? hlsl_source_info->entry_point : "main");

    hlsl_ctx_cleanup(&hlsl_ctx);

    return ret;
}
