/*
 * HLSL optimization and code generation
 *
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
#include "vkd3d_d3dx9shader.h"

/* Split uniforms into two variables representing the constant and temp
 * registers, and copy the former to the latter, so that writes to uniforms
 * work. */
static void prepend_uniform_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *temp)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_ir_var *uniform;
    struct hlsl_ir_store *store;
    struct hlsl_ir_load *load;

    /* Use the synthetic name for the temp, rather than the uniform, so that we
     * can write the uniform name into the shader reflection data. */

    if (!(uniform = hlsl_new_var(ctx, temp->name, temp->data_type, temp->loc, NULL, 0, temp->reg_reservation)))
        return;
    list_add_before(&temp->scope_entry, &uniform->scope_entry);
    list_add_tail(&ctx->extern_vars, &uniform->extern_entry);
    uniform->is_uniform = 1;
    uniform->is_param = temp->is_param;

    if (!(name = hlsl_get_string_buffer(ctx)))
        return;
    vkd3d_string_buffer_printf(name, "<temp-%s>", temp->name);
    temp->name = hlsl_strdup(ctx, name->buffer);
    hlsl_release_string_buffer(ctx, name);

    if (!(load = hlsl_new_var_load(ctx, uniform, temp->loc)))
        return;
    list_add_head(instrs, &load->node.entry);

    if (!(store = hlsl_new_simple_store(ctx, temp, &load->node)))
        return;
    list_add_after(&load->node.entry, &store->node.entry);
}

static void prepend_input_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset, const struct hlsl_semantic *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_semantic new_semantic;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_load *load;
    struct hlsl_ir_var *input;

    if (!(name = hlsl_get_string_buffer(ctx)))
        return;
    vkd3d_string_buffer_printf(name, "<input-%s%u>", semantic->name, semantic->index);
    if (!(new_semantic.name = hlsl_strdup(ctx, semantic->name)))
    {
        hlsl_release_string_buffer(ctx, name);
        return;
    }
    new_semantic.index = semantic->index;
    if (!(input = hlsl_new_var(ctx, hlsl_strdup(ctx, name->buffer), type, var->loc, &new_semantic, 0, NULL)))
    {
        hlsl_release_string_buffer(ctx, name);
        vkd3d_free((void *)new_semantic.name);
        return;
    }
    hlsl_release_string_buffer(ctx, name);
    input->is_input_semantic = 1;
    input->is_param = var->is_param;
    list_add_before(&var->scope_entry, &input->scope_entry);
    list_add_tail(&ctx->extern_vars, &input->extern_entry);

    if (!(load = hlsl_new_var_load(ctx, input, var->loc)))
        return;
    list_add_head(instrs, &load->node.entry);

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset * 4, var->loc)))
        return;
    list_add_after(&load->node.entry, &offset->node.entry);

    if (!(store = hlsl_new_store(ctx, var, &offset->node, &load->node, 0, var->loc)))
        return;
    list_add_after(&offset->node.entry, &store->node.entry);
}

static void prepend_input_struct_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset)
{
    struct hlsl_struct_field *field;

    LIST_FOR_EACH_ENTRY(field, type->e.elements, struct hlsl_struct_field, entry)
    {
        if (field->type->type == HLSL_CLASS_STRUCT)
            prepend_input_struct_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset);
        else if (field->semantic.name)
            prepend_input_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset, &field->semantic);
        else
            hlsl_error(ctx, field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Field '%s' is missing a semantic.", field->name);
    }
}

/* Split inputs into two variables representing the semantic and temp registers,
 * and copy the former to the latter, so that writes to input variables work. */
static void prepend_input_var_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var)
{
    if (var->data_type->type == HLSL_CLASS_STRUCT)
        prepend_input_struct_copy(ctx, instrs, var, var->data_type, 0);
    else if (var->semantic.name)
        prepend_input_copy(ctx, instrs, var, var->data_type, 0, &var->semantic);
}

static void append_output_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset, const struct hlsl_semantic *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_semantic new_semantic;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_var *output;
    struct hlsl_ir_load *load;

    if (!(name = hlsl_get_string_buffer(ctx)))
        return;
    vkd3d_string_buffer_printf(name, "<output-%s%u>", semantic->name, semantic->index);
    if (!(new_semantic.name = hlsl_strdup(ctx, semantic->name)))
    {
        hlsl_release_string_buffer(ctx, name);
        return;
    }
    new_semantic.index = semantic->index;
    if (!(output = hlsl_new_var(ctx, hlsl_strdup(ctx, name->buffer), type, var->loc, &new_semantic, 0, NULL)))
    {
        vkd3d_free((void *)new_semantic.name);
        hlsl_release_string_buffer(ctx, name);
        return;
    }
    hlsl_release_string_buffer(ctx, name);
    output->is_output_semantic = 1;
    output->is_param = var->is_param;
    list_add_before(&var->scope_entry, &output->scope_entry);
    list_add_tail(&ctx->extern_vars, &output->extern_entry);

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset * 4, var->loc)))
        return;
    list_add_tail(instrs, &offset->node.entry);

    if (!(load = hlsl_new_load(ctx, var, &offset->node, type, var->loc)))
        return;
    list_add_after(&offset->node.entry, &load->node.entry);

    if (!(store = hlsl_new_store(ctx, output, NULL, &load->node, 0, var->loc)))
        return;
    list_add_after(&load->node.entry, &store->node.entry);
}

static void append_output_struct_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset)
{
    struct hlsl_struct_field *field;

    LIST_FOR_EACH_ENTRY(field, type->e.elements, struct hlsl_struct_field, entry)
    {
        if (field->type->type == HLSL_CLASS_STRUCT)
            append_output_struct_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset);
        else if (field->semantic.name)
            append_output_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset, &field->semantic);
        else
            hlsl_error(ctx, field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Field '%s' is missing a semantic.", field->name);
    }
}

/* Split outputs into two variables representing the temp and semantic
 * registers, and copy the former to the latter, so that reads from output
 * variables work. */
static void append_output_var_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var)
{
    if (var->data_type->type == HLSL_CLASS_STRUCT)
        append_output_struct_copy(ctx, instrs, var, var->data_type, 0);
    else if (var->semantic.name)
        append_output_copy(ctx, instrs, var, var->data_type, 0, &var->semantic);
}

static bool transform_ir(struct hlsl_ctx *ctx, bool (*func)(struct hlsl_ctx *ctx, struct hlsl_ir_node *, void *),
        struct list *instrs, void *context)
{
    struct hlsl_ir_node *instr, *next;
    bool progress = 0;

    LIST_FOR_EACH_ENTRY_SAFE(instr, next, instrs, struct hlsl_ir_node, entry)
    {
        if (instr->type == HLSL_IR_IF)
        {
            struct hlsl_ir_if *iff = hlsl_ir_if(instr);

            progress |= transform_ir(ctx, func, &iff->then_instrs, context);
            progress |= transform_ir(ctx, func, &iff->else_instrs, context);
        }
        else if (instr->type == HLSL_IR_LOOP)
            progress |= transform_ir(ctx, func, &hlsl_ir_loop(instr)->body, context);

        progress |= func(ctx, instr, context);
    }

    return progress;
}

static void replace_node(struct hlsl_ir_node *old, struct hlsl_ir_node *new)
{
    struct hlsl_src *src, *next;

    LIST_FOR_EACH_ENTRY_SAFE(src, next, &old->uses, struct hlsl_src, entry)
    {
        hlsl_src_remove(src);
        hlsl_src_from_node(src, new);
    }
    list_remove(&old->entry);
    hlsl_free_instr(old);
}

static bool is_vec1(const struct hlsl_type *type)
{
    return (type->type == HLSL_CLASS_SCALAR) || (type->type == HLSL_CLASS_VECTOR && type->dimx == 1);
}

static bool fold_redundant_casts(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    if (instr->type == HLSL_IR_EXPR)
    {
        struct hlsl_ir_expr *expr = hlsl_ir_expr(instr);
        const struct hlsl_type *src_type = expr->operands[0].node->data_type;
        const struct hlsl_type *dst_type = expr->node.data_type;

        if (expr->op != HLSL_IR_UNOP_CAST)
            return false;

        if (hlsl_types_are_equal(src_type, dst_type)
                || (src_type->base_type == dst_type->base_type && is_vec1(src_type) && is_vec1(dst_type)))
        {
            replace_node(&expr->node, expr->operands[0].node);
            return true;
        }
    }

    return false;
}

static bool split_struct_copies(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_struct_field *field;
    const struct hlsl_ir_load *rhs_load;
    const struct hlsl_ir_node *rhs;
    const struct hlsl_type *type;
    struct hlsl_ir_store *store;

    if (instr->type != HLSL_IR_STORE)
        return false;

    store = hlsl_ir_store(instr);
    rhs = store->rhs.node;
    type = rhs->data_type;
    if (type->type != HLSL_CLASS_STRUCT)
        return false;

    rhs_load = hlsl_ir_load(rhs);

    LIST_FOR_EACH_ENTRY(field, type->e.elements, struct hlsl_struct_field, entry)
    {
        struct hlsl_ir_store *field_store;
        struct hlsl_ir_node *offset, *add;
        struct hlsl_ir_load *field_load;
        struct hlsl_ir_constant *c;

        if (!(c = hlsl_new_uint_constant(ctx, field->reg_offset * 4, instr->loc)))
            return false;
        list_add_before(&instr->entry, &c->node.entry);

        offset = &c->node;
        if (rhs_load->src.offset.node)
        {
            if (!(add = hlsl_new_binary_expr(ctx, HLSL_IR_BINOP_ADD, rhs_load->src.offset.node, &c->node)))
                return false;
            list_add_before(&instr->entry, &add->entry);
            offset = add;
        }
        if (!(field_load = hlsl_new_load(ctx, rhs_load->src.var, offset, field->type, instr->loc)))
            return false;
        list_add_before(&instr->entry, &field_load->node.entry);

        offset = &c->node;
        if (store->lhs.offset.node)
        {
            if (!(add = hlsl_new_binary_expr(ctx, HLSL_IR_BINOP_ADD, store->lhs.offset.node, &c->node)))
                return false;
            list_add_before(&instr->entry, &add->entry);
            offset = add;
        }

        if (!(field_store = hlsl_new_store(ctx, store->lhs.var, offset, &field_load->node, 0, instr->loc)))
            return false;
        list_add_before(&instr->entry, &field_store->node.entry);
    }

    /* Remove the store instruction, so that we can split structs which contain
     * other structs. Although assignments produce a value, we don't allow
     * HLSL_IR_STORE to be used as a source. */
    list_remove(&store->node.entry);
    hlsl_free_instr(&store->node);
    return true;
}

static bool fold_constants(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_constant *arg1, *arg2 = NULL, *res;
    struct hlsl_ir_expr *expr;
    unsigned int i;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    expr = hlsl_ir_expr(instr);

    for (i = 0; i < ARRAY_SIZE(expr->operands); ++i)
    {
        if (expr->operands[i].node && expr->operands[i].node->type != HLSL_IR_CONSTANT)
            return false;
    }
    arg1 = hlsl_ir_constant(expr->operands[0].node);
    if (expr->operands[1].node)
        arg2 = hlsl_ir_constant(expr->operands[1].node);

    if (!(res = hlsl_alloc(ctx, sizeof(*res))))
        return false;
    init_node(&res->node, HLSL_IR_CONSTANT, instr->data_type, instr->loc);

    switch (instr->data_type->base_type)
    {
        case HLSL_TYPE_UINT:
        {
            unsigned int i;

            switch (expr->op)
            {
                case HLSL_IR_BINOP_ADD:
                    for (i = 0; i < instr->data_type->dimx; ++i)
                        res->value.u[i] = arg1->value.u[i] + arg2->value.u[i];
                    break;

                case HLSL_IR_BINOP_MUL:
                    for (i = 0; i < instr->data_type->dimx; ++i)
                        res->value.u[i] = arg1->value.u[i] * arg2->value.u[i];
                    break;

                default:
                    FIXME("Fold uint op %#x.\n", expr->op);
                    vkd3d_free(res);
                    return false;
            }
            break;
        }

        default:
            FIXME("Fold type %#x op %#x.\n", instr->data_type->base_type, expr->op);
            vkd3d_free(res);
            return false;
    }

    list_add_before(&expr->node.entry, &res->node.entry);
    replace_node(&expr->node, &res->node);
    return true;
}

/* Lower DIV to RCP + MUL. */
static bool lower_division(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_expr *expr;
    struct hlsl_ir_node *rcp;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    expr = hlsl_ir_expr(instr);
    if (expr->op != HLSL_IR_BINOP_DIV)
        return false;

    if (!(rcp = hlsl_new_unary_expr(ctx, HLSL_IR_UNOP_RCP, expr->operands[1].node, instr->loc)))
        return false;
    list_add_before(&expr->node.entry, &rcp->entry);
    expr->op = HLSL_IR_BINOP_MUL;
    hlsl_src_remove(&expr->operands[1]);
    hlsl_src_from_node(&expr->operands[1], rcp);
    return true;
}

static bool dce(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    switch (instr->type)
    {
        case HLSL_IR_CONSTANT:
        case HLSL_IR_EXPR:
        case HLSL_IR_LOAD:
        case HLSL_IR_SWIZZLE:
            if (list_empty(&instr->uses))
            {
                list_remove(&instr->entry);
                hlsl_free_instr(instr);
                return true;
            }
            break;

        case HLSL_IR_STORE:
        {
            struct hlsl_ir_store *store = hlsl_ir_store(instr);
            struct hlsl_ir_var *var = store->lhs.var;

            if (var->last_read < instr->index)
            {
                list_remove(&instr->entry);
                hlsl_free_instr(instr);
                return true;
            }
            break;
        }

        case HLSL_IR_IF:
        case HLSL_IR_JUMP:
        case HLSL_IR_LOOP:
            break;
    }

    return false;
}

/* Allocate a unique, ordered index to each instruction, which will be used for
 * computing liveness ranges. */
static unsigned int index_instructions(struct list *instrs, unsigned int index)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        instr->index = index++;

        if (instr->type == HLSL_IR_IF)
        {
            struct hlsl_ir_if *iff = hlsl_ir_if(instr);
            index = index_instructions(&iff->then_instrs, index);
            index = index_instructions(&iff->else_instrs, index);
        }
        else if (instr->type == HLSL_IR_LOOP)
        {
            index = index_instructions(&hlsl_ir_loop(instr)->body, index);
            hlsl_ir_loop(instr)->next_index = index;
        }
    }

    return index;
}

static void dump_function_decl(struct rb_entry *entry, void *context)
{
    struct hlsl_ir_function_decl *func = RB_ENTRY_VALUE(entry, struct hlsl_ir_function_decl, entry);
    struct hlsl_ctx *ctx = context;

    if (func->body)
        hlsl_dump_function(ctx, func);
}

static void dump_function(struct rb_entry *entry, void *context)
{
    struct hlsl_ir_function *func = RB_ENTRY_VALUE(entry, struct hlsl_ir_function, entry);
    struct hlsl_ctx *ctx = context;

    rb_for_each_entry(&func->overloads, dump_function_decl, ctx);
}

/* Compute the earliest and latest liveness for each variable. In the case that
 * a variable is accessed inside of a loop, we promote its liveness to extend
 * to at least the range of the entire loop. Note that we don't need to do this
 * for anonymous nodes, since there's currently no way to use a node which was
 * calculated in an earlier iteration of the loop. */
static void compute_liveness_recurse(struct list *instrs, unsigned int loop_first, unsigned int loop_last)
{
    struct hlsl_ir_node *instr;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
        case HLSL_IR_STORE:
        {
            struct hlsl_ir_store *store = hlsl_ir_store(instr);

            var = store->lhs.var;
            if (!var->first_write)
                var->first_write = loop_first ? min(instr->index, loop_first) : instr->index;
            store->rhs.node->last_read = instr->index;
            if (store->lhs.offset.node)
                store->lhs.offset.node->last_read = instr->index;
            break;
        }
        case HLSL_IR_EXPR:
        {
            struct hlsl_ir_expr *expr = hlsl_ir_expr(instr);
            unsigned int i;

            for (i = 0; i < ARRAY_SIZE(expr->operands) && expr->operands[i].node; ++i)
                expr->operands[i].node->last_read = instr->index;
            break;
        }
        case HLSL_IR_IF:
        {
            struct hlsl_ir_if *iff = hlsl_ir_if(instr);

            compute_liveness_recurse(&iff->then_instrs, loop_first, loop_last);
            compute_liveness_recurse(&iff->else_instrs, loop_first, loop_last);
            iff->condition.node->last_read = instr->index;
            break;
        }
        case HLSL_IR_LOAD:
        {
            struct hlsl_ir_load *load = hlsl_ir_load(instr);

            var = load->src.var;
            var->last_read = max(var->last_read, loop_last ? max(instr->index, loop_last) : instr->index);
            if (load->src.offset.node)
                load->src.offset.node->last_read = instr->index;
            break;
        }
        case HLSL_IR_LOOP:
        {
            struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);

            compute_liveness_recurse(&loop->body, loop_first ? loop_first : instr->index,
                    loop_last ? loop_last : loop->next_index);
            break;
        }
        case HLSL_IR_SWIZZLE:
        {
            struct hlsl_ir_swizzle *swizzle = hlsl_ir_swizzle(instr);

            swizzle->val.node->last_read = instr->index;
            break;
        }
        case HLSL_IR_CONSTANT:
        case HLSL_IR_JUMP:
            break;
        }
    }
}

static void compute_liveness(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func)
{
    struct hlsl_scope *scope;
    struct hlsl_ir_var *var;

    /* Index 0 means unused; index 1 means function entry, so start at 2. */
    index_instructions(entry_func->body, 2);

    LIST_FOR_EACH_ENTRY(scope, &ctx->scopes, struct hlsl_scope, entry)
    {
        LIST_FOR_EACH_ENTRY(var, &scope->vars, struct hlsl_ir_var, scope_entry)
            var->first_write = var->last_read = 0;
    }

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_uniform || var->is_input_semantic)
            var->first_write = 1;
        else if (var->is_output_semantic)
            var->last_read = UINT_MAX;
    }

    if (entry_func->return_var)
        entry_func->return_var->last_read = UINT_MAX;

    compute_liveness_recurse(entry_func->body, 0, 0);
}

struct liveness
{
    size_t size;
    struct
    {
        /* 0 if not live yet. */
        unsigned int last_read;
    } *regs;
};

static unsigned int get_available_writemask(struct liveness *liveness,
        unsigned int first_write, unsigned int component_idx, unsigned int component_count)
{
    unsigned int i, writemask = 0, count = 0;

    for (i = 0; i < 4; ++i)
    {
        if (liveness->regs[component_idx + i].last_read <= first_write)
        {
            writemask |= 1u << i;
            if (++count == component_count)
                return writemask;
        }
    }

    return 0;
}

static bool resize_liveness(struct hlsl_ctx *ctx, struct liveness *liveness, size_t new_count)
{
    size_t old_capacity = liveness->size;

    if (!hlsl_array_reserve(ctx, (void **)&liveness->regs, &liveness->size, new_count, sizeof(*liveness->regs)))
        return false;

    if (liveness->size > old_capacity)
        memset(liveness->regs + old_capacity, 0, (liveness->size - old_capacity) * sizeof(*liveness->regs));
    return true;
}

static struct hlsl_reg allocate_register(struct hlsl_ctx *ctx, struct liveness *liveness,
        unsigned int first_write, unsigned int last_read, unsigned int component_count)
{
    unsigned int component_idx, writemask, i;
    struct hlsl_reg ret = {0};

    for (component_idx = 0; component_idx < liveness->size; component_idx += 4)
    {
        if ((writemask = get_available_writemask(liveness, first_write, component_idx, component_count)))
            break;
    }
    if (component_idx == liveness->size)
    {
        if (!resize_liveness(ctx, liveness, component_idx + 4))
            return ret;
        writemask = (1u << component_count) - 1;
    }
    for (i = 0; i < 4; ++i)
    {
        if (writemask & (1u << i))
            liveness->regs[component_idx + i].last_read = last_read;
    }
    ret.id = component_idx / 4;
    ret.writemask = writemask;
    ret.allocated = true;
    return ret;
}

static bool is_range_available(struct liveness *liveness, unsigned int first_write,
        unsigned int component_idx, unsigned int component_count)
{
    unsigned int i;

    for (i = 0; i < component_count; i += 4)
    {
        if (!get_available_writemask(liveness, first_write, component_idx + i, 4))
            return false;
    }
    return true;
}

static struct hlsl_reg allocate_range(struct hlsl_ctx *ctx, struct liveness *liveness,
        unsigned int first_write, unsigned int last_read, unsigned int reg_count)
{
    const unsigned int component_count = reg_count * 4;
    unsigned int i, component_idx;
    struct hlsl_reg ret = {0};

    for (component_idx = 0; component_idx < liveness->size; component_idx += 4)
    {
        if (is_range_available(liveness, first_write, component_idx,
                min(component_count, liveness->size - component_idx)))
            break;
    }
    if (!resize_liveness(ctx, liveness, component_idx + component_count))
        return ret;

    for (i = 0; i < component_count; ++i)
        liveness->regs[component_idx + i].last_read = last_read;
    ret.id = component_idx / 4;
    ret.allocated = true;
    return ret;
}

static const char *debug_register(char class, struct hlsl_reg reg, const struct hlsl_type *type)
{
    if (type->reg_size > 1)
        return vkd3d_dbg_sprintf("%c%u-%c%u", class, reg.id, class,
                reg.id + type->reg_size - 1);
    return vkd3d_dbg_sprintf("%c%u%s", class, reg.id, debug_hlsl_writemask(reg.writemask));
}

static void allocate_variable_temp_register(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, struct liveness *liveness)
{
    if (var->is_input_semantic || var->is_output_semantic || var->is_uniform)
        return;

    if (!var->reg.allocated && var->last_read)
    {
        if (var->data_type->reg_size > 1)
            var->reg = allocate_range(ctx, liveness, var->first_write,
                    var->last_read, var->data_type->reg_size);
        else
            var->reg = allocate_register(ctx, liveness, var->first_write,
                    var->last_read, var->data_type->dimx);
        TRACE("Allocated %s to %s (liveness %u-%u).\n", var->name,
                debug_register('r', var->reg, var->data_type), var->first_write, var->last_read);
    }
}

static void allocate_temp_registers_recurse(struct hlsl_ctx *ctx, struct list *instrs, struct liveness *liveness)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        if (!instr->reg.allocated && instr->last_read)
        {
            if (instr->data_type->reg_size > 1)
                instr->reg = allocate_range(ctx, liveness, instr->index,
                        instr->last_read, instr->data_type->reg_size);
            else
                instr->reg = allocate_register(ctx, liveness, instr->index,
                        instr->last_read, instr->data_type->dimx);
            TRACE("Allocated anonymous expression @%u to %s (liveness %u-%u).\n", instr->index,
                    debug_register('r', instr->reg, instr->data_type), instr->index, instr->last_read);
        }

        switch (instr->type)
        {
            case HLSL_IR_IF:
            {
                struct hlsl_ir_if *iff = hlsl_ir_if(instr);
                allocate_temp_registers_recurse(ctx, &iff->then_instrs, liveness);
                allocate_temp_registers_recurse(ctx, &iff->else_instrs, liveness);
                break;
            }

            case HLSL_IR_LOAD:
            {
                struct hlsl_ir_load *load = hlsl_ir_load(instr);
                /* We need to at least allocate a variable for undefs.
                 * FIXME: We should probably find a way to remove them instead. */
                allocate_variable_temp_register(ctx, load->src.var, liveness);
                break;
            }

            case HLSL_IR_LOOP:
            {
                struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);
                allocate_temp_registers_recurse(ctx, &loop->body, liveness);
                break;
            }

            case HLSL_IR_STORE:
            {
                struct hlsl_ir_store *store = hlsl_ir_store(instr);
                allocate_variable_temp_register(ctx, store->lhs.var, liveness);
                break;
            }

            default:
                break;
        }
    }
}

static void allocate_const_registers_recurse(struct hlsl_ctx *ctx, struct list *instrs, struct liveness *liveness)
{
    struct hlsl_constant_defs *defs = &ctx->constant_defs;
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
            case HLSL_IR_CONSTANT:
            {
                struct hlsl_ir_constant *constant = hlsl_ir_constant(instr);
                const struct hlsl_type *type = instr->data_type;
                unsigned int reg_size = type->reg_size;
                unsigned int x, y, i, writemask;

                if (reg_size > 1)
                    constant->reg = allocate_range(ctx, liveness, 1, UINT_MAX, reg_size);
                else
                    constant->reg = allocate_register(ctx, liveness, 1, UINT_MAX, type->dimx);
                TRACE("Allocated constant @%u to %s.\n", instr->index, debug_register('c', constant->reg, type));

                if (!hlsl_array_reserve(ctx, (void **)&defs->values, &defs->size,
                        constant->reg.id + reg_size, sizeof(*defs->values)))
                    return;
                defs->count = max(defs->count, constant->reg.id + reg_size);

                assert(type->type <= HLSL_CLASS_LAST_NUMERIC);

                if (!(writemask = constant->reg.writemask))
                    writemask = (1u << type->dimx) - 1;

                for (y = 0; y < type->dimy; ++y)
                {
                    for (x = 0, i = 0; x < 4; ++x)
                    {
                        float f;

                        if (!(writemask & (1u << x)))
                            continue;

                        switch (type->base_type)
                        {
                            case HLSL_TYPE_BOOL:
                                f = constant->value.b[i++];
                                break;

                            case HLSL_TYPE_FLOAT:
                            case HLSL_TYPE_HALF:
                                f = constant->value.f[i++];
                                break;

                            case HLSL_TYPE_INT:
                                f = constant->value.i[i++];
                                break;

                            case HLSL_TYPE_UINT:
                                f = constant->value.u[i++];
                                break;

                            case HLSL_TYPE_DOUBLE:
                                FIXME("Double constant.\n");
                                return;

                            default:
                                assert(0);
                                return;
                        }
                        defs->values[constant->reg.id + y].f[x] = f;
                    }
                }

                break;
            }

            case HLSL_IR_IF:
            {
                struct hlsl_ir_if *iff = hlsl_ir_if(instr);
                allocate_const_registers_recurse(ctx, &iff->then_instrs, liveness);
                allocate_const_registers_recurse(ctx, &iff->else_instrs, liveness);
                break;
            }

            case HLSL_IR_LOOP:
            {
                struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);
                allocate_const_registers_recurse(ctx, &loop->body, liveness);
                break;
            }

            default:
                break;
        }
    }
}

static void allocate_const_registers(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func)
{
    struct liveness liveness = {0};
    struct hlsl_ir_var *var;

    allocate_const_registers_recurse(ctx, entry_func->body, &liveness);

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_uniform && var->last_read)
        {
            if (var->data_type->reg_size > 1)
                var->reg = allocate_range(ctx, &liveness, 1, UINT_MAX, var->data_type->reg_size);
            else
            {
                var->reg = allocate_register(ctx, &liveness, 1, UINT_MAX, 4);
                var->reg.writemask = (1u << var->data_type->dimx) - 1;
            }
            TRACE("Allocated %s to %s.\n", var->name, debug_register('c', var->reg, var->data_type));
        }
    }
}

/* Simple greedy temporary register allocation pass that just assigns a unique
 * index to all (simultaneously live) variables or intermediate values. Agnostic
 * as to how many registers are actually available for the current backend, and
 * does not handle constants. */
static void allocate_temp_registers(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func)
{
    struct liveness liveness = {0};
    allocate_temp_registers_recurse(ctx, entry_func->body, &liveness);
}

static bool sm1_register_from_semantic(struct hlsl_ctx *ctx, const struct hlsl_semantic *semantic, bool output,
        D3DSHADER_PARAM_REGISTER_TYPE *type, unsigned int *reg)
{
    unsigned int i;

    static const struct
    {
        const char *semantic;
        bool output;
        enum vkd3d_shader_type shader_type;
        unsigned int major_version;
        D3DSHADER_PARAM_REGISTER_TYPE type;
        DWORD offset;
    }
    register_table[] =
    {
        {"color",       true,  VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_COLOROUT},
        {"depth",       true,  VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_DEPTHOUT},
        {"sv_depth",    true,  VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_DEPTHOUT},
        {"sv_target",   true,  VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_COLOROUT},
        {"color",       false, VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_INPUT},
        {"texcoord",    false, VKD3D_SHADER_TYPE_PIXEL, 2, D3DSPR_TEXTURE},

        {"color",       true,  VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_COLOROUT},
        {"depth",       true,  VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_DEPTHOUT},
        {"sv_depth",    true,  VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_DEPTHOUT},
        {"sv_target",   true,  VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_COLOROUT},
        {"sv_position", false, VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_MISCTYPE,    D3DSMO_POSITION},
        {"vface",       false, VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_MISCTYPE,    D3DSMO_FACE},
        {"vpos",        false, VKD3D_SHADER_TYPE_PIXEL, 3, D3DSPR_MISCTYPE,    D3DSMO_POSITION},

        {"color",       true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_ATTROUT},
        {"fog",         true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_RASTOUT,     D3DSRO_FOG},
        {"position",    true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_RASTOUT,     D3DSRO_POSITION},
        {"psize",       true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_RASTOUT,     D3DSRO_POINT_SIZE},
        {"sv_position", true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_RASTOUT,     D3DSRO_POSITION},
        {"texcoord",    true,  VKD3D_SHADER_TYPE_VERTEX, 1, D3DSPR_TEXCRDOUT},

        {"color",       true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_ATTROUT},
        {"fog",         true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_RASTOUT,     D3DSRO_FOG},
        {"position",    true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_RASTOUT,     D3DSRO_POSITION},
        {"psize",       true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_RASTOUT,     D3DSRO_POINT_SIZE},
        {"sv_position", true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_RASTOUT,     D3DSRO_POSITION},
        {"texcoord",    true,  VKD3D_SHADER_TYPE_VERTEX, 2, D3DSPR_TEXCRDOUT},
    };

    for (i = 0; i < ARRAY_SIZE(register_table); ++i)
    {
        if (!ascii_strcasecmp(semantic->name, register_table[i].semantic)
                && output == register_table[i].output
                && ctx->profile->type == register_table[i].shader_type
                && ctx->profile->major_version == register_table[i].major_version)
        {
            *type = register_table[i].type;
            if (register_table[i].type == D3DSPR_MISCTYPE || register_table[i].type == D3DSPR_RASTOUT)
                *reg = register_table[i].offset;
            else
                *reg = semantic->index;
            return true;
        }
    }

    return false;
}

static bool sm1_usage_from_semantic(const struct hlsl_semantic *semantic, D3DDECLUSAGE *usage, uint32_t *usage_idx)
{
    static const struct
    {
        const char *name;
        D3DDECLUSAGE usage;
    }
    semantics[] =
    {
        {"binormal",        D3DDECLUSAGE_BINORMAL},
        {"blendindices",    D3DDECLUSAGE_BLENDINDICES},
        {"blendweight",     D3DDECLUSAGE_BLENDWEIGHT},
        {"color",           D3DDECLUSAGE_COLOR},
        {"depth",           D3DDECLUSAGE_DEPTH},
        {"fog",             D3DDECLUSAGE_FOG},
        {"normal",          D3DDECLUSAGE_NORMAL},
        {"position",        D3DDECLUSAGE_POSITION},
        {"positiont",       D3DDECLUSAGE_POSITIONT},
        {"psize",           D3DDECLUSAGE_PSIZE},
        {"sample",          D3DDECLUSAGE_SAMPLE},
        {"sv_depth",        D3DDECLUSAGE_DEPTH},
        {"sv_position",     D3DDECLUSAGE_POSITION},
        {"sv_target",       D3DDECLUSAGE_COLOR},
        {"tangent",         D3DDECLUSAGE_TANGENT},
        {"tessfactor",      D3DDECLUSAGE_TESSFACTOR},
        {"texcoord",        D3DDECLUSAGE_TEXCOORD},
    };

    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(semantics); ++i)
    {
        if (!ascii_strcasecmp(semantic->name, semantics[i].name))
        {
            *usage = semantics[i].usage;
            *usage_idx = semantic->index;
            return true;
        }
    }

    return false;
}

static void allocate_semantic_register(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, unsigned int *counter, bool output)
{
    assert(var->semantic.name);

    if (ctx->profile->major_version < 4)
    {
        D3DSHADER_PARAM_REGISTER_TYPE type;
        uint32_t reg, usage_idx;
        D3DDECLUSAGE usage;

        if (!sm1_usage_from_semantic(&var->semantic, &usage, &usage_idx))
        {
            hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SEMANTIC,
                    "Invalid semantic '%s'.", var->semantic.name);
            return;
        }

        if (sm1_register_from_semantic(ctx, &var->semantic, output, &type, &reg))
        {
            TRACE("%s %s semantic %s[%u] matches predefined register %#x[%u].\n",
                    ctx->profile->type == VKD3D_SHADER_TYPE_PIXEL ? "Pixel" : "Vertex", output ? "output" : "input",
                    var->semantic.name, var->semantic.index, type, reg);
        }
        else
        {
            var->reg.allocated = true;
            var->reg.id = (*counter)++;
            var->reg.writemask = (1 << var->data_type->dimx) - 1;
            TRACE("Allocated %s to %s.\n", var->name, debug_register(output ? 'o' : 'v', var->reg, var->data_type));
        }
    }
}

static void allocate_semantic_registers(struct hlsl_ctx *ctx)
{
    unsigned int input_counter = 0, output_counter = 0;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_input_semantic && var->last_read)
            allocate_semantic_register(ctx, var, &input_counter, false);
        if (var->is_output_semantic && var->first_write)
            allocate_semantic_register(ctx, var, &output_counter, true);
    }
}

static unsigned int map_swizzle(unsigned int swizzle, unsigned int writemask)
{
    unsigned int i, ret = 0;

    for (i = 0; i < 4; ++i)
    {
        if (writemask & (1 << i))
        {
            ret |= (swizzle & 3) << (i * 2);
            swizzle >>= 2;
        }
    }
    return ret;
}

static unsigned int swizzle_from_writemask(unsigned int writemask)
{
    static const unsigned int swizzles[16] =
    {
        0,
        HLSL_SWIZZLE(X, X, X, X),
        HLSL_SWIZZLE(Y, Y, Y, Y),
        HLSL_SWIZZLE(X, Y, X, X),
        HLSL_SWIZZLE(Z, Z, Z, Z),
        HLSL_SWIZZLE(X, Z, X, X),
        HLSL_SWIZZLE(Y, Z, X, X),
        HLSL_SWIZZLE(X, Y, Z, X),
        HLSL_SWIZZLE(W, W, W, W),
        HLSL_SWIZZLE(X, W, X, X),
        HLSL_SWIZZLE(Y, W, X, X),
        HLSL_SWIZZLE(X, Y, W, X),
        HLSL_SWIZZLE(Z, W, X, X),
        HLSL_SWIZZLE(X, Z, W, X),
        HLSL_SWIZZLE(Y, Z, W, X),
        HLSL_SWIZZLE(X, Y, Z, W),
    };

    return swizzles[writemask & 0xf];
}

static unsigned int combine_writemasks(unsigned int first, unsigned int second)
{
    unsigned int ret = 0, i, j = 0;

    for (i = 0; i < 4; ++i)
    {
        if (first & (1 << i))
        {
            if (second & (1 << j++))
                ret |= (1 << i);
        }
    }

    return ret;
}

static unsigned int combine_swizzles(unsigned int first, unsigned int second, unsigned int dim)
{
    unsigned int ret = 0, i;
    for (i = 0; i < dim; ++i)
    {
        unsigned int s = (second >> (i * 2)) & 3;
        ret |= ((first >> (s * 2)) & 3) << (i * 2);
    }
    return ret;
}

static bool type_is_single_reg(const struct hlsl_type *type)
{
    return type->type == HLSL_CLASS_SCALAR || type->type == HLSL_CLASS_VECTOR;
}

static struct hlsl_reg hlsl_reg_from_deref(const struct hlsl_deref *deref, const struct hlsl_type *type)
{
    struct hlsl_ir_node *offset_node = deref->offset.node;
    const struct hlsl_ir_var *var = deref->var;
    struct hlsl_reg ret = {0};
    unsigned int offset = 0;

    /* We should always have generated a cast to UINT. */
    if (offset_node)
        assert(offset_node->data_type->type == HLSL_CLASS_SCALAR
                && offset_node->data_type->base_type == HLSL_TYPE_UINT);

    if (offset_node && offset_node->type != HLSL_IR_CONSTANT)
    {
        FIXME("Dereference with non-constant offset of type %s.\n", hlsl_node_type_to_string(offset_node->type));
        return ret;
    }

    ret = var->reg;

    ret.allocated = var->reg.allocated;
    ret.id = var->reg.id;
    if (offset_node)
        offset = hlsl_ir_constant(offset_node)->value.u[0];
    ret.id += offset / 4;

    if (type_is_single_reg(var->data_type))
    {
        assert(!offset);
        ret.writemask = var->reg.writemask;
    }
    else
    {
        assert(type_is_single_reg(type));
        ret.writemask = ((1 << type->dimx) - 1) << (offset & 3);
    }
    return ret;
}

struct bytecode_buffer
{
    struct hlsl_ctx *ctx;
    uint32_t *data;
    size_t count, size;
    int status;
};

/* Returns the token index. */
static unsigned int put_dword(struct bytecode_buffer *buffer, uint32_t value)
{
    unsigned int index = buffer->count;

    if (buffer->status)
        return index;

    if (!hlsl_array_reserve(buffer->ctx, (void **)&buffer->data, &buffer->size,
            buffer->count + 1, sizeof(*buffer->data)))
    {
        buffer->status = VKD3D_ERROR_OUT_OF_MEMORY;
        return index;
    }
    buffer->data[buffer->count++] = value;

    return index;
}

/* Returns the token index. */
static unsigned int put_float(struct bytecode_buffer *buffer, float value)
{
    union
    {
        float f;
        uint32_t u;
    } u;
    u.f = value;
    return put_dword(buffer, u.u);
}

static void set_dword(struct bytecode_buffer *buffer, unsigned int index, uint32_t value)
{
    if (buffer->status)
        return;

    assert(index < buffer->count);
    buffer->data[index] = value;
}

/* Returns the token index. */
static unsigned int put_string(struct bytecode_buffer *buffer, const char *str)
{
    unsigned int index = buffer->count;
    size_t len = strlen(str) + 1;
    unsigned int token_count = (len + 3) / sizeof(*buffer->data);

    if (buffer->status)
        return index;

    if (!hlsl_array_reserve(buffer->ctx, (void **)&buffer->data, &buffer->size,
            buffer->count + token_count, sizeof(*buffer->data)))
    {
        buffer->status = E_OUTOFMEMORY;
        return index;
    }

    buffer->data[buffer->count + token_count - 1] = 0xabababab;
    memcpy(buffer->data + buffer->count, str, len);
    buffer->count += token_count;
    return index;
}

static uint32_t sm1_version(enum vkd3d_shader_type type, unsigned int major, unsigned int minor)
{
    if (type == VKD3D_SHADER_TYPE_VERTEX)
        return D3DVS_VERSION(major, minor);
    else
        return D3DPS_VERSION(major, minor);
}

static D3DXPARAMETER_CLASS sm1_class(const struct hlsl_type *type)
{
    switch (type->type)
    {
        case HLSL_CLASS_ARRAY:
            return sm1_class(type->e.array.type);
        case HLSL_CLASS_MATRIX:
            assert(type->modifiers & HLSL_MODIFIERS_MAJORITY_MASK);
            if (type->modifiers & HLSL_MODIFIER_COLUMN_MAJOR)
                return D3DXPC_MATRIX_COLUMNS;
            else
                return D3DXPC_MATRIX_ROWS;
        case HLSL_CLASS_OBJECT:
            return D3DXPC_OBJECT;
        case HLSL_CLASS_SCALAR:
            return D3DXPC_SCALAR;
        case HLSL_CLASS_STRUCT:
            return D3DXPC_STRUCT;
        case HLSL_CLASS_VECTOR:
            return D3DXPC_VECTOR;
        default:
            ERR("Invalid class %#x.\n", type->type);
            assert(0);
            return 0;
    }
}

static D3DXPARAMETER_TYPE sm1_base_type(const struct hlsl_type *type)
{
    switch (type->base_type)
    {
        case HLSL_TYPE_BOOL:
            return D3DXPT_BOOL;
        case HLSL_TYPE_FLOAT:
        case HLSL_TYPE_HALF:
            return D3DXPT_FLOAT;
        case HLSL_TYPE_INT:
        case HLSL_TYPE_UINT:
            return D3DXPT_INT;
        case HLSL_TYPE_PIXELSHADER:
            return D3DXPT_PIXELSHADER;
        case HLSL_TYPE_SAMPLER:
            switch (type->sampler_dim)
            {
                case HLSL_SAMPLER_DIM_1D:
                    return D3DXPT_SAMPLER1D;
                case HLSL_SAMPLER_DIM_2D:
                    return D3DXPT_SAMPLER2D;
                case HLSL_SAMPLER_DIM_3D:
                    return D3DXPT_SAMPLER3D;
                case HLSL_SAMPLER_DIM_CUBE:
                    return D3DXPT_SAMPLERCUBE;
                case HLSL_SAMPLER_DIM_GENERIC:
                    return D3DXPT_SAMPLER;
                default:
                    ERR("Invalid dimension %#x.\n", type->sampler_dim);
            }
            break;
        case HLSL_TYPE_STRING:
            return D3DXPT_STRING;
        case HLSL_TYPE_TEXTURE:
            switch (type->sampler_dim)
            {
                case HLSL_SAMPLER_DIM_1D:
                    return D3DXPT_TEXTURE1D;
                case HLSL_SAMPLER_DIM_2D:
                    return D3DXPT_TEXTURE2D;
                case HLSL_SAMPLER_DIM_3D:
                    return D3DXPT_TEXTURE3D;
                case HLSL_SAMPLER_DIM_CUBE:
                    return D3DXPT_TEXTURECUBE;
                case HLSL_SAMPLER_DIM_GENERIC:
                    return D3DXPT_TEXTURE;
                default:
                    ERR("Invalid dimension %#x.\n", type->sampler_dim);
            }
            break;
        case HLSL_TYPE_VERTEXSHADER:
            return D3DXPT_VERTEXSHADER;
        case HLSL_TYPE_VOID:
            return D3DXPT_VOID;
        default:
            assert(0);
    }
    assert(0);
    return 0;
}

static const struct hlsl_type *get_array_type(const struct hlsl_type *type)
{
    if (type->type == HLSL_CLASS_ARRAY)
        return get_array_type(type->e.array.type);
    return type;
}

static unsigned int get_array_size(const struct hlsl_type *type)
{
    if (type->type == HLSL_CLASS_ARRAY)
        return get_array_size(type->e.array.type) * type->e.array.elements_count;
    return 1;
}

static void write_sm1_type(struct bytecode_buffer *buffer, struct hlsl_type *type, unsigned int ctab_start)
{
    const struct hlsl_type *array_type = get_array_type(type);
    unsigned int fields_offset = 0, field_count = 0;
    unsigned int array_size = get_array_size(type);
    struct hlsl_struct_field *field;

    if (type->bytecode_offset)
        return;

    if (array_type->type == HLSL_CLASS_STRUCT)
    {
        LIST_FOR_EACH_ENTRY(field, array_type->e.elements, struct hlsl_struct_field, entry)
        {
            field->name_bytecode_offset = put_string(buffer, field->name);
            write_sm1_type(buffer, field->type, ctab_start);
        }

        fields_offset = (buffer->count - ctab_start) * sizeof(*buffer->data);

        LIST_FOR_EACH_ENTRY(field, array_type->e.elements, struct hlsl_struct_field, entry)
        {
            put_dword(buffer, (field->name_bytecode_offset - ctab_start) * sizeof(*buffer->data));
            put_dword(buffer, (field->type->bytecode_offset - ctab_start) * sizeof(*buffer->data));
            ++field_count;
        }
    }

    type->bytecode_offset = put_dword(buffer, sm1_class(type) | (sm1_base_type(type) << 16));
    put_dword(buffer, type->dimy | (type->dimx << 16));
    put_dword(buffer, array_size | (field_count << 16));
    put_dword(buffer, fields_offset);
}

static void sm1_sort_extern(struct list *sorted, struct hlsl_ir_var *to_sort)
{
    struct hlsl_ir_var *var;

    list_remove(&to_sort->extern_entry);

    LIST_FOR_EACH_ENTRY(var, sorted, struct hlsl_ir_var, extern_entry)
    {
        if (strcmp(to_sort->name, var->name) < 0)
        {
            list_add_before(&var->extern_entry, &to_sort->extern_entry);
            return;
        }
    }

    list_add_tail(sorted, &to_sort->extern_entry);
}

static void sm1_sort_externs(struct hlsl_ctx *ctx)
{
    struct list sorted = LIST_INIT(sorted);
    struct hlsl_ir_var *var, *next;

    LIST_FOR_EACH_ENTRY_SAFE(var, next, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
        sm1_sort_extern(&sorted, var);
    list_move_tail(&ctx->extern_vars, &sorted);
}

static void write_sm1_uniforms(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        struct hlsl_ir_function_decl *entry_func)
{
    unsigned int ctab_start, vars_start, size_offset, creator_offset, offset;
    unsigned int uniform_count = 0;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (!var->semantic.name && var->reg.allocated)
        {
            ++uniform_count;

            if (var->is_param && var->is_uniform)
            {
                struct vkd3d_string_buffer *name;

                if (!(name = hlsl_get_string_buffer(ctx)))
                {
                    buffer->status = VKD3D_ERROR_OUT_OF_MEMORY;
                    return;
                }
                vkd3d_string_buffer_printf(name, "$%s", var->name);
                vkd3d_free((char *)var->name);
                var->name = hlsl_strdup(ctx, name->buffer);
                hlsl_release_string_buffer(ctx, name);
            }
        }
    }

    sm1_sort_externs(ctx);

    size_offset = put_dword(buffer, 0);
    put_dword(buffer, MAKEFOURCC('C','T','A','B'));

    ctab_start = put_dword(buffer, sizeof(D3DXSHADER_CONSTANTTABLE));
    creator_offset = put_dword(buffer, 0);
    put_dword(buffer, sm1_version(ctx->profile->type, ctx->profile->major_version, ctx->profile->minor_version));
    put_dword(buffer, uniform_count);
    put_dword(buffer, sizeof(D3DXSHADER_CONSTANTTABLE)); /* offset of constants */
    put_dword(buffer, 0); /* FIXME: flags */
    put_dword(buffer, 0); /* FIXME: target string */

    vars_start = buffer->count;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (!var->semantic.name && var->reg.allocated)
        {
            put_dword(buffer, 0); /* name */
            put_dword(buffer, D3DXRS_FLOAT4 | (var->reg.id << 16));
            put_dword(buffer, var->data_type->reg_size);
            put_dword(buffer, 0); /* type */
            put_dword(buffer, 0); /* FIXME: default value */
        }
    }

    uniform_count = 0;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (!var->semantic.name && var->reg.allocated)
        {
            set_dword(buffer, vars_start + (uniform_count * 5), (buffer->count - ctab_start) * sizeof(*buffer->data));
            put_string(buffer, var->name);

            write_sm1_type(buffer, var->data_type, ctab_start);
            set_dword(buffer, vars_start + (uniform_count * 5) + 3,
                    (var->data_type->bytecode_offset - ctab_start) * sizeof(*buffer->data));
            ++uniform_count;
        }
    }

    offset = put_string(buffer, vkd3d_shader_get_version(NULL, NULL));
    set_dword(buffer, creator_offset, (offset - ctab_start) * sizeof(*buffer->data));

    set_dword(buffer, size_offset, D3DSIO_COMMENT | ((buffer->count - (ctab_start - 1)) << 16));
}

static uint32_t sm1_encode_register_type(D3DSHADER_PARAM_REGISTER_TYPE type)
{
    return ((type << D3DSP_REGTYPE_SHIFT) & D3DSP_REGTYPE_MASK)
            | ((type << D3DSP_REGTYPE_SHIFT2) & D3DSP_REGTYPE_MASK2);
}

struct sm1_instruction
{
    D3DSHADER_INSTRUCTION_OPCODE_TYPE opcode;

    struct sm1_dst_register
    {
        D3DSHADER_PARAM_REGISTER_TYPE type;
        D3DSHADER_PARAM_DSTMOD_TYPE mod;
        unsigned int writemask;
        uint32_t reg;
    } dst;

    struct sm1_src_register
    {
        D3DSHADER_PARAM_REGISTER_TYPE type;
        D3DSHADER_PARAM_SRCMOD_TYPE mod;
        unsigned int swizzle;
        uint32_t reg;
    } srcs[2];
    unsigned int src_count;

    unsigned int has_dst;
};

static void write_sm1_dst_register(struct bytecode_buffer *buffer, const struct sm1_dst_register *reg)
{
    assert(reg->writemask);
    put_dword(buffer, (1u << 31) | sm1_encode_register_type(reg->type) | reg->mod | (reg->writemask << 16) | reg->reg);
}

static void write_sm1_src_register(struct bytecode_buffer *buffer,
        const struct sm1_src_register *reg, unsigned int dst_writemask)
{
    unsigned int swizzle = map_swizzle(reg->swizzle, dst_writemask);

    put_dword(buffer, (1u << 31) | sm1_encode_register_type(reg->type) | reg->mod | (swizzle << 16) | reg->reg);
}

static void write_sm1_instruction(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        const struct sm1_instruction *instr)
{
    uint32_t token = instr->opcode;
    unsigned int i;

    if (ctx->profile->major_version > 1)
        token |= (instr->has_dst + instr->src_count) << D3DSI_INSTLENGTH_SHIFT;
    put_dword(buffer, token);

    if (instr->has_dst)
        write_sm1_dst_register(buffer, &instr->dst);

    for (i = 0; i < instr->src_count; ++i)
        write_sm1_src_register(buffer, &instr->srcs[i], instr->dst.writemask);
};

static void write_sm1_binary_op(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        D3DSHADER_INSTRUCTION_OPCODE_TYPE opcode, const struct hlsl_reg *dst,
        const struct hlsl_reg *src1, const struct hlsl_reg *src2)
{
    const struct sm1_instruction instr =
    {
        .opcode = opcode,

        .dst.type = D3DSPR_TEMP,
        .dst.writemask = dst->writemask,
        .dst.reg = dst->id,
        .has_dst = 1,

        .srcs[0].type = D3DSPR_TEMP,
        .srcs[0].swizzle = swizzle_from_writemask(src1->writemask),
        .srcs[0].reg = src1->id,
        .srcs[1].type = D3DSPR_TEMP,
        .srcs[1].swizzle = swizzle_from_writemask(src2->writemask),
        .srcs[1].reg = src2->id,
        .src_count = 2,
    };
    write_sm1_instruction(ctx, buffer, &instr);
}

static void write_sm1_unary_op(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        D3DSHADER_INSTRUCTION_OPCODE_TYPE opcode, const struct hlsl_reg *dst,
        const struct hlsl_reg *src, D3DSHADER_PARAM_SRCMOD_TYPE src_mod)
{
    const struct sm1_instruction instr =
    {
        .opcode = opcode,

        .dst.type = D3DSPR_TEMP,
        .dst.writemask = dst->writemask,
        .dst.reg = dst->id,
        .has_dst = 1,

        .srcs[0].type = D3DSPR_TEMP,
        .srcs[0].swizzle = swizzle_from_writemask(src->writemask),
        .srcs[0].reg = src->id,
        .srcs[0].mod = src_mod,
        .src_count = 1,
    };
    write_sm1_instruction(ctx, buffer, &instr);
}

static void write_sm1_constant_defs(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer)
{
    unsigned int i, x;

    for (i = 0; i < ctx->constant_defs.count; ++i)
    {
        uint32_t token = D3DSIO_DEF;
        const struct sm1_dst_register reg =
        {
            .type = D3DSPR_CONST,
            .writemask = VKD3DSP_WRITEMASK_ALL,
            .reg = i,
        };

        if (ctx->profile->major_version > 1)
            token |= 5 << D3DSI_INSTLENGTH_SHIFT;
        put_dword(buffer, token);

        write_sm1_dst_register(buffer, &reg);
        for (x = 0; x < 4; ++x)
            put_float(buffer, ctx->constant_defs.values[i].f[x]);
    }
}

static void write_sm1_semantic_dcl(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        const struct hlsl_ir_var *var, bool output)
{
    struct sm1_dst_register reg = {0};
    uint32_t token, usage_idx;
    D3DDECLUSAGE usage;
    bool ret;

    if (sm1_register_from_semantic(ctx, &var->semantic, output, &reg.type, &reg.reg))
    {
        usage = 0;
        usage_idx = 0;
    }
    else
    {
        ret = sm1_usage_from_semantic(&var->semantic, &usage, &usage_idx);
        assert(ret);
        reg.type = output ? D3DSPR_OUTPUT : D3DSPR_INPUT;
        reg.reg = var->reg.id;
    }

    token = D3DSIO_DCL;
    if (ctx->profile->major_version > 1)
        token |= 2 << D3DSI_INSTLENGTH_SHIFT;
    put_dword(buffer, token);

    token = (1u << 31);
    token |= usage << D3DSP_DCL_USAGE_SHIFT;
    token |= usage_idx << D3DSP_DCL_USAGEINDEX_SHIFT;
    put_dword(buffer, token);

    reg.writemask = (1 << var->data_type->dimx) - 1;
    write_sm1_dst_register(buffer, &reg);
}

static void write_sm1_semantic_dcls(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer)
{
    bool write_in = false, write_out = false;
    struct hlsl_ir_var *var;

    if (ctx->profile->type == VKD3D_SHADER_TYPE_PIXEL)
        write_in = true;
    else if (ctx->profile->type == VKD3D_SHADER_TYPE_VERTEX && ctx->profile->major_version == 3)
        write_in = write_out = true;
    else if (ctx->profile->type == VKD3D_SHADER_TYPE_VERTEX && ctx->profile->major_version < 3)
        write_in = true;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (write_in && var->is_input_semantic)
            write_sm1_semantic_dcl(ctx, buffer, var, false);
        if (write_out && var->is_output_semantic)
            write_sm1_semantic_dcl(ctx, buffer, var, true);
    }
}

static void write_sm1_constant(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer, const struct hlsl_ir_node *instr)
{
    const struct hlsl_ir_constant *constant = hlsl_ir_constant(instr);
    struct sm1_instruction sm1_instr =
    {
        .opcode = D3DSIO_MOV,

        .dst.type = D3DSPR_TEMP,
        .dst.reg = instr->reg.id,
        .dst.writemask = instr->reg.writemask,
        .has_dst = 1,

        .srcs[0].type = D3DSPR_CONST,
        .srcs[0].reg = constant->reg.id,
        .srcs[0].swizzle = swizzle_from_writemask(constant->reg.writemask),
        .src_count = 1,
    };

    assert(instr->reg.allocated);
    assert(constant->reg.allocated);
    write_sm1_instruction(ctx, buffer, &sm1_instr);
}

static void write_sm1_expr(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer, const struct hlsl_ir_node *instr)
{
    struct hlsl_ir_expr *expr = hlsl_ir_expr(instr);
    struct hlsl_ir_node *arg1 = expr->operands[0].node;
    struct hlsl_ir_node *arg2 = expr->operands[1].node;

    assert(instr->reg.allocated);

    if (instr->data_type->base_type != HLSL_TYPE_FLOAT)
    {
        FIXME("Non-float operations need to be lowered.\n");
        return;
    }

    switch (expr->op)
    {
        case HLSL_IR_BINOP_ADD:
            write_sm1_binary_op(ctx, buffer, D3DSIO_ADD, &instr->reg, &arg1->reg, &arg2->reg);
            break;

        case HLSL_IR_BINOP_MUL:
            write_sm1_binary_op(ctx, buffer, D3DSIO_MUL, &instr->reg, &arg1->reg, &arg2->reg);
            break;

        case HLSL_IR_UNOP_NEG:
            write_sm1_unary_op(ctx, buffer, D3DSIO_MOV, &instr->reg, &arg1->reg, D3DSPSM_NEG);
            break;

        default:
            FIXME("Unhandled op %u.\n", expr->op);
            break;
    }
}

static void write_sm1_load(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer, const struct hlsl_ir_node *instr)
{
    const struct hlsl_ir_load *load = hlsl_ir_load(instr);
    const struct hlsl_reg reg = hlsl_reg_from_deref(&load->src, instr->data_type);
    struct sm1_instruction sm1_instr =
    {
        .opcode = D3DSIO_MOV,

        .dst.type = D3DSPR_TEMP,
        .dst.reg = instr->reg.id,
        .dst.writemask = instr->reg.writemask,
        .has_dst = 1,

        .srcs[0].type = D3DSPR_TEMP,
        .srcs[0].reg = reg.id,
        .srcs[0].swizzle = swizzle_from_writemask(reg.writemask),
        .src_count = 1,
    };

    assert(instr->reg.allocated);

    if (load->src.var->is_uniform)
    {
        assert(reg.allocated);
        sm1_instr.srcs[0].type = D3DSPR_CONST;
    }
    else if (load->src.var->is_input_semantic)
    {
        if (!sm1_register_from_semantic(ctx, &load->src.var->semantic,
                false, &sm1_instr.srcs[0].type, &sm1_instr.srcs[0].reg))
        {
            assert(reg.allocated);
            sm1_instr.srcs[0].type = D3DSPR_INPUT;
            sm1_instr.srcs[0].reg = reg.id;
        }
        else
            sm1_instr.srcs[0].swizzle = swizzle_from_writemask((1 << load->src.var->data_type->dimx) - 1);
    }

    write_sm1_instruction(ctx, buffer, &sm1_instr);
}

static void write_sm1_store(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer, const struct hlsl_ir_node *instr)
{
    const struct hlsl_ir_store *store = hlsl_ir_store(instr);
    const struct hlsl_ir_node *rhs = store->rhs.node;
    const struct hlsl_reg reg = hlsl_reg_from_deref(&store->lhs, rhs->data_type);
    struct sm1_instruction sm1_instr =
    {
        .opcode = D3DSIO_MOV,

        .dst.type = D3DSPR_TEMP,
        .dst.reg = reg.id,
        .dst.writemask = combine_writemasks(reg.writemask, store->writemask),
        .has_dst = 1,

        .srcs[0].type = D3DSPR_TEMP,
        .srcs[0].reg = rhs->reg.id,
        .srcs[0].swizzle = swizzle_from_writemask(rhs->reg.writemask),
        .src_count = 1,
    };

    if (store->lhs.var->data_type->type == HLSL_CLASS_MATRIX)
    {
        FIXME("Matrix writemasks need to be lowered.\n");
        return;
    }

    if (store->lhs.var->is_output_semantic)
    {
        if (!sm1_register_from_semantic(ctx, &store->lhs.var->semantic, true, &sm1_instr.dst.type, &sm1_instr.dst.reg))
        {
            assert(reg.allocated);
            sm1_instr.dst.type = D3DSPR_OUTPUT;
            sm1_instr.dst.reg = reg.id;
        }
        else
            sm1_instr.dst.writemask = (1u << store->lhs.var->data_type->dimx) - 1;
    }
    else
        assert(reg.allocated);

    write_sm1_instruction(ctx, buffer, &sm1_instr);
}

static void write_sm1_swizzle(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer, const struct hlsl_ir_node *instr)
{
    const struct hlsl_ir_swizzle *swizzle = hlsl_ir_swizzle(instr);
    const struct hlsl_ir_node *val = swizzle->val.node;
    struct sm1_instruction sm1_instr =
    {
        .opcode = D3DSIO_MOV,

        .dst.type = D3DSPR_TEMP,
        .dst.reg = instr->reg.id,
        .dst.writemask = instr->reg.writemask,
        .has_dst = 1,

        .srcs[0].type = D3DSPR_TEMP,
        .srcs[0].reg = val->reg.id,
        .srcs[0].swizzle = combine_swizzles(swizzle_from_writemask(val->reg.writemask),
                swizzle->swizzle, instr->data_type->dimx),
        .src_count = 1,
    };

    assert(instr->reg.allocated);
    assert(val->reg.allocated);
    write_sm1_instruction(ctx, buffer, &sm1_instr);
}

static void write_sm1_instructions(struct hlsl_ctx *ctx, struct bytecode_buffer *buffer,
        const struct hlsl_ir_function_decl *entry_func)
{
    const struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, entry_func->body, struct hlsl_ir_node, entry)
    {
        if (instr->data_type)
        {
            if (instr->data_type->type == HLSL_CLASS_MATRIX)
            {
                FIXME("Matrix operations need to be lowered.\n");
                break;
            }

            assert(instr->data_type->type == HLSL_CLASS_SCALAR || instr->data_type->type == HLSL_CLASS_VECTOR);
        }

        switch (instr->type)
        {
            case HLSL_IR_CONSTANT:
                write_sm1_constant(ctx, buffer, instr);
                break;

            case HLSL_IR_EXPR:
                write_sm1_expr(ctx, buffer, instr);
                break;

            case HLSL_IR_LOAD:
                write_sm1_load(ctx, buffer, instr);
                break;

            case HLSL_IR_STORE:
                write_sm1_store(ctx, buffer, instr);
                break;

            case HLSL_IR_SWIZZLE:
                write_sm1_swizzle(ctx, buffer, instr);
                break;

            default:
                FIXME("Unhandled instruction type %s.\n", hlsl_node_type_to_string(instr->type));
        }
    }
}

static int write_sm1_shader(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func,
        struct vkd3d_shader_code *out)
{
    struct bytecode_buffer buffer = {.ctx = ctx};
    int ret;

    put_dword(&buffer, sm1_version(ctx->profile->type, ctx->profile->major_version, ctx->profile->minor_version));

    write_sm1_uniforms(ctx, &buffer, entry_func);

    write_sm1_constant_defs(ctx, &buffer);
    write_sm1_semantic_dcls(ctx, &buffer);
    write_sm1_instructions(ctx, &buffer, entry_func);

    put_dword(&buffer, D3DSIO_END);

    if (!(ret = buffer.status))
    {
        out->code = buffer.data;
        out->size = buffer.count * sizeof(*buffer.data);
    }
    return ret;
}

int hlsl_emit_dxbc(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func, struct vkd3d_shader_code *out)
{
    struct hlsl_ir_var *var;

    list_move_head(entry_func->body, &ctx->static_initializers);

    LIST_FOR_EACH_ENTRY(var, &ctx->globals->vars, struct hlsl_ir_var, scope_entry)
    {
        if (var->data_type->type == HLSL_CLASS_OBJECT)
            list_add_tail(&ctx->extern_vars, &var->extern_entry);
        else if (var->modifiers & HLSL_STORAGE_UNIFORM)
            prepend_uniform_copy(ctx, entry_func->body, var);
    }

    LIST_FOR_EACH_ENTRY(var, entry_func->parameters, struct hlsl_ir_var, param_entry)
    {
        if (var->data_type->type == HLSL_CLASS_OBJECT)
        {
            list_add_tail(&ctx->extern_vars, &var->extern_entry);
        }
        else
        {
            if (var->modifiers & HLSL_STORAGE_UNIFORM)
            {
                prepend_uniform_copy(ctx, entry_func->body, var);
            }
            else
            {
                if (var->data_type->type != HLSL_CLASS_STRUCT && !var->semantic.name)
                    hlsl_error(ctx, var->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                            "Parameter \"%s\" is missing a semantic.", var->name);

                if (var->modifiers & HLSL_STORAGE_IN)
                    prepend_input_var_copy(ctx, entry_func->body, var);
                if (var->modifiers & HLSL_STORAGE_OUT)
                    append_output_var_copy(ctx, entry_func->body, var);
            }
        }
    }
    if (entry_func->return_var)
    {
        if (entry_func->return_var->data_type->type != HLSL_CLASS_STRUCT && !entry_func->return_var->semantic.name)
            hlsl_error(ctx, entry_func->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Entry point \"%s\" is missing a return value semantic.", entry_func->func->name);

        append_output_var_copy(ctx, entry_func->body, entry_func->return_var);
    }

    while (transform_ir(ctx, fold_redundant_casts, entry_func->body, NULL));
    while (transform_ir(ctx, split_struct_copies, entry_func->body, NULL));
    while (transform_ir(ctx, fold_constants, entry_func->body, NULL));

    if (ctx->profile->major_version < 4)
        transform_ir(ctx, lower_division, entry_func->body, NULL);

    do
        compute_liveness(ctx, entry_func);
    while (transform_ir(ctx, dce, entry_func->body, NULL));

    compute_liveness(ctx, entry_func);

    if (TRACE_ON())
        rb_for_each_entry(&ctx->functions, dump_function, ctx);

    allocate_temp_registers(ctx, entry_func);
    if (ctx->profile->major_version < 4)
        allocate_const_registers(ctx, entry_func);
    allocate_semantic_registers(ctx);

    if (ctx->result)
        return ctx->result;

    if (ctx->profile->major_version < 4)
        return write_sm1_shader(ctx, entry_func, out);
    else
        return VKD3D_ERROR_NOT_IMPLEMENTED;
}
