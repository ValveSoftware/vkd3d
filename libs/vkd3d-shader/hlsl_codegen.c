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
    list_add_before(&old->entry, &new->entry);
    list_remove(&old->entry);
    hlsl_free_instr(old);
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

    replace_node(&expr->node, &res->node);
    return true;
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
        case HLSL_IR_ASSIGNMENT:
        {
            struct hlsl_ir_assignment *assignment = hlsl_ir_assignment(instr);

            var = assignment->lhs.var;
            if (!var->first_write)
                var->first_write = loop_first ? min(instr->index, loop_first) : instr->index;
            assignment->rhs.node->last_read = instr->index;
            if (assignment->lhs.offset.node)
                assignment->lhs.offset.node->last_read = instr->index;
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
            var->last_read = loop_last ? max(instr->index, loop_last) : instr->index;
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
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->globals->vars, struct hlsl_ir_var, scope_entry)
    {
        var->first_write = 1;
    }

    LIST_FOR_EACH_ENTRY(var, entry_func->parameters, struct hlsl_ir_var, param_entry)
    {
        if (var->modifiers & HLSL_STORAGE_IN)
            var->first_write = 1;
        if (var->modifiers & HLSL_STORAGE_OUT)
            var->last_read = UINT_MAX;
    }

    if (entry_func->return_var)
        entry_func->return_var->last_read = UINT_MAX;

    compute_liveness_recurse(entry_func->body, 0, 0);
}

int hlsl_emit_dxbc(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func)
{
    list_move_head(entry_func->body, &ctx->static_initializers);

    while (transform_ir(ctx, fold_constants, entry_func->body, NULL));

    /* Index 0 means unused; index 1 means function entry, so start at 2. */
    index_instructions(entry_func->body, 2);

    if (TRACE_ON())
        rb_for_each_entry(&ctx->functions, dump_function, NULL);

    compute_liveness(ctx, entry_func);

    if (ctx->failed)
        return VKD3D_ERROR_INVALID_SHADER;
    return VKD3D_ERROR_NOT_IMPLEMENTED;
}
