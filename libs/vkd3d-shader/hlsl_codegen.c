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
#include "vkd3d_d3d9types.h"

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

    if (!(uniform = hlsl_new_var(temp->name, temp->data_type, temp->loc, NULL, temp->reg_reservation)))
    {
        ctx->failed = true;
        return;
    }
    list_add_before(&temp->scope_entry, &uniform->scope_entry);
    list_add_tail(&ctx->extern_vars, &uniform->extern_entry);
    temp->is_uniform = 0;
    uniform->is_uniform = 1;

    if (!(name = vkd3d_string_buffer_get(&ctx->string_buffers)))
    {
        ctx->failed = true;
        return;
    }
    vkd3d_string_buffer_printf(name, "<temp-%s>", temp->name);
    temp->name = vkd3d_strdup(name->buffer);
    vkd3d_string_buffer_release(&ctx->string_buffers, name);

    if (!(load = hlsl_new_var_load(uniform, temp->loc)))
    {
        ctx->failed = true;
        return;
    }
    list_add_head(instrs, &load->node.entry);

    if (!(store = hlsl_new_simple_store(temp, &load->node)))
    {
        ctx->failed = true;
        return;
    }
    list_add_after(&load->node.entry, &store->node.entry);
}

static void prepend_input_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset, const char *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_var *varying;
    struct hlsl_ir_load *load;

    if (!(name = vkd3d_string_buffer_get(&ctx->string_buffers)))
    {
        ctx->failed = true;
        return;
    }
    vkd3d_string_buffer_printf(name, "<input-%s>", semantic);
    if (!(varying = hlsl_new_var(vkd3d_strdup(name->buffer), type, var->loc, vkd3d_strdup(semantic), NULL)))
    {
        vkd3d_string_buffer_release(&ctx->string_buffers, name);
        ctx->failed = true;
        return;
    }
    vkd3d_string_buffer_release(&ctx->string_buffers, name);
    varying->is_input_varying = 1;
    list_add_before(&var->scope_entry, &varying->scope_entry);
    list_add_tail(&ctx->extern_vars, &varying->extern_entry);

    if (!(load = hlsl_new_var_load(varying, var->loc)))
    {
        ctx->failed = true;
        return;
    }
    list_add_head(instrs, &load->node.entry);

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset * 4, var->loc)))
    {
        ctx->failed = true;
        return;
    }
    list_add_after(&load->node.entry, &offset->node.entry);

    if (!(store = hlsl_new_store(var, &offset->node, &load->node, 0, var->loc)))
    {
        ctx->failed = true;
        return;
    }
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
        else if (field->semantic)
            prepend_input_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset, field->semantic);
        else
            hlsl_error(ctx, field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Field '%s' is missing a semantic.", field->name);
    }
}

/* Split input varyings into two variables representing the varying and temp
 * registers, and copy the former to the latter, so that writes to input
 * varyings work. */
static void prepend_input_var_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var)
{
    if (var->data_type->type == HLSL_CLASS_STRUCT)
        prepend_input_struct_copy(ctx, instrs, var, var->data_type, 0);
    else if (var->semantic)
        prepend_input_copy(ctx, instrs, var, var->data_type, 0, var->semantic);

    var->is_input_varying = 0;
}

static void append_output_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset, const char *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_var *varying;
    struct hlsl_ir_load *load;

    if (!(name = vkd3d_string_buffer_get(&ctx->string_buffers)))
    {
        ctx->failed = true;
        return;
    }
    vkd3d_string_buffer_printf(name, "<output-%s>", semantic);
    if (!(varying = hlsl_new_var(vkd3d_strdup(name->buffer), type, var->loc, vkd3d_strdup(semantic), NULL)))
    {
        vkd3d_string_buffer_release(&ctx->string_buffers, name);
        ctx->failed = true;
        return;
    }
    vkd3d_string_buffer_release(&ctx->string_buffers, name);
    varying->is_output_varying = 1;
    list_add_before(&var->scope_entry, &varying->scope_entry);
    list_add_tail(&ctx->extern_vars, &varying->extern_entry);

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset * 4, var->loc)))
    {
        ctx->failed = true;
        return;
    }
    list_add_tail(instrs, &offset->node.entry);

    if (!(load = hlsl_new_load(var, &offset->node, type, var->loc)))
    {
        ctx->failed = true;
        return;
    }
    list_add_after(&offset->node.entry, &load->node.entry);

    if (!(store = hlsl_new_store(varying, NULL, &load->node, 0, var->loc)))
    {
        ctx->failed = true;
        return;
    }
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
        else if (field->semantic)
            append_output_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset, field->semantic);
        else
            hlsl_error(ctx, field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Field '%s' is missing a semantic.", field->name);
    }
}

/* Split output varyings into two variables representing the temp and varying
 * registers, and copy the former to the latter, so that reads from output
 * varyings work. */
static void append_output_var_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var)
{
    if (var->data_type->type == HLSL_CLASS_STRUCT)
        append_output_struct_copy(ctx, instrs, var, var->data_type, 0);
    else if (var->semantic)
        append_output_copy(ctx, instrs, var, var->data_type, 0, var->semantic);

    var->is_output_varying = 0;
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
        {
            ctx->failed = true;
            return false;
        }
        list_add_before(&instr->entry, &c->node.entry);

        offset = &c->node;
        if (rhs_load->src.offset.node)
        {
            if (!(add = hlsl_new_binary_expr(HLSL_IR_BINOP_ADD, rhs_load->src.offset.node, &c->node)))
            {
                ctx->failed = true;
                return false;
            }
            list_add_before(&instr->entry, &add->entry);
            offset = add;
        }
        if (!(field_load = hlsl_new_load(rhs_load->src.var, offset, field->type, instr->loc)))
        {
            ctx->failed = true;
            return false;
        }
        list_add_before(&instr->entry, &field_load->node.entry);

        offset = &c->node;
        if (store->lhs.offset.node)
        {
            if (!(add = hlsl_new_binary_expr(HLSL_IR_BINOP_ADD, store->lhs.offset.node, &c->node)))
            {
                ctx->failed = true;
                return false;
            }
            list_add_before(&instr->entry, &add->entry);
            offset = add;
        }

        if (!(field_store = hlsl_new_store(store->lhs.var, offset, &field_load->node, 0, instr->loc)))
        {
            ctx->failed = true;
            return false;
        }
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

    if (!(res = vkd3d_calloc(1, sizeof(*res))))
    {
        ctx->failed = true;
        return false;
    }
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

    if (func->body)
        hlsl_dump_function(func);
}

static void dump_function(struct rb_entry *entry, void *context)
{
    struct hlsl_ir_function *func = RB_ENTRY_VALUE(entry, struct hlsl_ir_function, entry);
    rb_for_each_entry(&func->overloads, dump_function_decl, NULL);
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
        if (var->is_uniform || var->is_input_varying)
            var->first_write = 1;
        else if (var->is_output_varying)
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

static bool resize_liveness(struct liveness *liveness, size_t new_count)
{
    size_t old_capacity = liveness->size;

    if (!vkd3d_array_reserve((void **)&liveness->regs, &liveness->size, new_count, sizeof(*liveness->regs)))
        return false;

    if (liveness->size > old_capacity)
        memset(liveness->regs + old_capacity, 0, (liveness->size - old_capacity) * sizeof(*liveness->regs));
    return true;
}

static struct hlsl_reg allocate_register(struct liveness *liveness,
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
        if (!resize_liveness(liveness, component_idx + 4))
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

static struct hlsl_reg allocate_range(struct liveness *liveness,
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
    if (!resize_liveness(liveness, component_idx + component_count))
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

static void allocate_variable_temp_register(struct hlsl_ir_var *var, struct liveness *liveness)
{
    if (var->is_input_varying || var->is_output_varying || var->is_uniform)
        return;

    if (!var->reg.allocated && var->last_read)
    {
        if (var->data_type->reg_size > 1)
            var->reg = allocate_range(liveness, var->first_write,
                    var->last_read, var->data_type->reg_size);
        else
            var->reg = allocate_register(liveness, var->first_write,
                    var->last_read, var->data_type->dimx);
        TRACE("Allocated %s to %s (liveness %u-%u).\n", var->name,
                debug_register('r', var->reg, var->data_type), var->first_write, var->last_read);
    }
}

static void allocate_temp_registers_recurse(struct list *instrs, struct liveness *liveness)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        if (!instr->reg.allocated && instr->last_read)
        {
            if (instr->data_type->reg_size > 1)
                instr->reg = allocate_range(liveness, instr->index,
                        instr->last_read, instr->data_type->reg_size);
            else
                instr->reg = allocate_register(liveness, instr->index,
                        instr->last_read, instr->data_type->dimx);
            TRACE("Allocated anonymous expression @%u to %s (liveness %u-%u).\n", instr->index,
                    debug_register('r', instr->reg, instr->data_type), instr->index, instr->last_read);
        }

        switch (instr->type)
        {
            case HLSL_IR_IF:
            {
                struct hlsl_ir_if *iff = hlsl_ir_if(instr);
                allocate_temp_registers_recurse(&iff->then_instrs, liveness);
                allocate_temp_registers_recurse(&iff->else_instrs, liveness);
                break;
            }

            case HLSL_IR_LOAD:
            {
                struct hlsl_ir_load *load = hlsl_ir_load(instr);
                /* We need to at least allocate a variable for undefs.
                 * FIXME: We should probably find a way to remove them instead. */
                allocate_variable_temp_register(load->src.var, liveness);
                break;
            }

            case HLSL_IR_LOOP:
            {
                struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);
                allocate_temp_registers_recurse(&loop->body, liveness);
                break;
            }

            case HLSL_IR_STORE:
            {
                struct hlsl_ir_store *store = hlsl_ir_store(instr);
                allocate_variable_temp_register(store->lhs.var, liveness);
                break;
            }

            default:
                break;
        }
    }
}

static void allocate_const_registers_recurse(struct list *instrs, struct liveness *liveness)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
            case HLSL_IR_CONSTANT:
            {
                struct hlsl_ir_constant *constant = hlsl_ir_constant(instr);

                if (instr->data_type->reg_size > 1)
                    constant->reg = allocate_range(liveness, 1, UINT_MAX, instr->data_type->reg_size);
                else
                    constant->reg = allocate_register(liveness, 1, UINT_MAX, instr->data_type->dimx);
                TRACE("Allocated constant @%u to %s.\n", instr->index,
                        debug_register('c', constant->reg, instr->data_type));
                break;
            }

            case HLSL_IR_IF:
            {
                struct hlsl_ir_if *iff = hlsl_ir_if(instr);
                allocate_const_registers_recurse(&iff->then_instrs, liveness);
                allocate_const_registers_recurse(&iff->else_instrs, liveness);
                break;
            }

            case HLSL_IR_LOOP:
            {
                struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);
                allocate_const_registers_recurse(&loop->body, liveness);
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

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_uniform && var->last_read)
        {
            if (var->data_type->reg_size > 1)
                var->reg = allocate_range(&liveness, 1, UINT_MAX, var->data_type->reg_size);
            else
            {
                var->reg = allocate_register(&liveness, 1, UINT_MAX, 4);
                var->reg.writemask = (1u << var->data_type->dimx) - 1;
            }
            TRACE("Allocated %s to %s.\n", var->name, debug_register('c', var->reg, var->data_type));
        }
    }

    allocate_const_registers_recurse(entry_func->body, &liveness);
}

/* Simple greedy temporary register allocation pass that just assigns a unique
 * index to all (simultaneously live) variables or intermediate values. Agnostic
 * as to how many registers are actually available for the current backend, and
 * does not handle constants. */
static void allocate_temp_registers(struct hlsl_ir_function_decl *entry_func)
{
    struct liveness liveness = {0};
    allocate_temp_registers_recurse(entry_func->body, &liveness);
}

struct bytecode_buffer
{
    uint32_t *data;
    size_t count, size;
    int status;
};

static void put_dword(struct bytecode_buffer *buffer, uint32_t value)
{
    if (buffer->status)
        return;

    if (!vkd3d_array_reserve((void **)&buffer->data, &buffer->size, buffer->count + 1, sizeof(*buffer->data)))
    {
        buffer->status = VKD3D_ERROR_OUT_OF_MEMORY;
        return;
    }
    buffer->data[buffer->count++] = value;
}

static uint32_t sm1_version(enum vkd3d_shader_type type, unsigned int major, unsigned int minor)
{
    if (type == VKD3D_SHADER_TYPE_VERTEX)
        return D3DVS_VERSION(major, minor);
    else
        return D3DPS_VERSION(major, minor);
}

static int write_sm1_shader(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func,
        struct vkd3d_shader_code *out)
{
    struct bytecode_buffer buffer = {0};
    int ret;

    put_dword(&buffer, sm1_version(ctx->profile->type, ctx->profile->major_version, ctx->profile->minor_version));

    put_dword(&buffer, D3DSIO_END);

    if (!(ret = buffer.status))
    {
        out->code = buffer.data;
        out->size = buffer.count * sizeof(uint32_t);
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
        if (var->is_uniform)
            prepend_uniform_copy(ctx, entry_func->body, var);
    }

    LIST_FOR_EACH_ENTRY(var, entry_func->parameters, struct hlsl_ir_var, param_entry)
    {
        if (var->data_type->type == HLSL_CLASS_OBJECT)
            list_add_tail(&ctx->extern_vars, &var->extern_entry);
        if (var->is_uniform)
            prepend_uniform_copy(ctx, entry_func->body, var);
        if (var->is_input_varying)
            prepend_input_var_copy(ctx, entry_func->body, var);
        if (var->is_output_varying)
            append_output_var_copy(ctx, entry_func->body, var);
    }
    if (entry_func->return_var)
        append_output_var_copy(ctx, entry_func->body, entry_func->return_var);

    while (transform_ir(ctx, fold_redundant_casts, entry_func->body, NULL));
    while (transform_ir(ctx, split_struct_copies, entry_func->body, NULL));
    while (transform_ir(ctx, fold_constants, entry_func->body, NULL));

    do
        compute_liveness(ctx, entry_func);
    while (transform_ir(ctx, dce, entry_func->body, NULL));

    compute_liveness(ctx, entry_func);

    if (TRACE_ON())
        rb_for_each_entry(&ctx->functions, dump_function, NULL);

    allocate_temp_registers(entry_func);
    if (ctx->profile->major_version < 4)
        allocate_const_registers(ctx, entry_func);

    if (ctx->failed)
        return VKD3D_ERROR_INVALID_SHADER;

    if (ctx->profile->major_version < 4)
        return write_sm1_shader(ctx, entry_func, out);
    else
        return VKD3D_ERROR_NOT_IMPLEMENTED;
}
