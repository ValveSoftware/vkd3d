/*
 * HLSL constant value operations for constant folding
 *
 * Copyright 2022 Francisco Casas for CodeWeavers
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

bool hlsl_fold_constants(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_constant *arg1, *arg2 = NULL, *res;
    struct hlsl_ir_expr *expr;
    unsigned int i, dimx;

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
    dimx = instr->data_type->dimx;

    if (!(res = hlsl_alloc(ctx, sizeof(*res))))
        return false;
    init_node(&res->node, HLSL_IR_CONSTANT, instr->data_type, instr->loc);

    switch (instr->data_type->base_type)
    {
        case HLSL_TYPE_FLOAT:
        {
            switch (expr->op)
            {
                case HLSL_OP1_CAST:
                    if (instr->data_type->dimx != arg1->node.data_type->dimx
                            || instr->data_type->dimy != arg1->node.data_type->dimy)
                    {
                        FIXME("Cast from %s to %s.\n", debug_hlsl_type(ctx, arg1->node.data_type),
                                debug_hlsl_type(ctx, instr->data_type));
                        vkd3d_free(res);
                        return false;
                    }

                    switch (arg1->node.data_type->base_type)
                    {
                        case HLSL_TYPE_INT:
                            for (i = 0; i < dimx; ++i)
                                res->value[i].f = arg1->value[i].i;
                            break;

                        case HLSL_TYPE_UINT:
                            for (i = 0; i < dimx; ++i)
                                res->value[i].f = arg1->value[i].u;
                            break;

                        default:
                            FIXME("Cast from %s to %s.\n", debug_hlsl_type(ctx, arg1->node.data_type),
                                    debug_hlsl_type(ctx, instr->data_type));
                            vkd3d_free(res);
                            return false;
                    }
                    break;

                default:
                    FIXME("Fold float op %#x.\n", expr->op);
                    vkd3d_free(res);
                    return false;
            }
            break;
        }

        case HLSL_TYPE_UINT:
        {
            switch (expr->op)
            {
                case HLSL_OP1_CAST:
                    if (instr->data_type->dimx != arg1->node.data_type->dimx
                            || instr->data_type->dimy != arg1->node.data_type->dimy)
                    {
                        FIXME("Cast from %s to %s.\n", debug_hlsl_type(ctx, arg1->node.data_type),
                                debug_hlsl_type(ctx, instr->data_type));
                        vkd3d_free(res);
                        return false;
                    }

                    switch (arg1->node.data_type->base_type)
                    {
                        case HLSL_TYPE_INT:
                            for (i = 0; i < dimx; ++i)
                                res->value[i].i = arg1->value[i].u;
                            break;

                        default:
                            FIXME("Cast from %s to %s.\n", debug_hlsl_type(ctx, arg1->node.data_type),
                                    debug_hlsl_type(ctx, instr->data_type));
                            vkd3d_free(res);
                            return false;
                    }
                    break;

                case HLSL_OP1_NEG:
                    for (i = 0; i < instr->data_type->dimx; ++i)
                        res->value[i].u = -arg1->value[i].u;
                    break;

                case HLSL_OP2_ADD:
                    for (i = 0; i < instr->data_type->dimx; ++i)
                        res->value[i].u = arg1->value[i].u + arg2->value[i].u;
                    break;

                case HLSL_OP2_MUL:
                    for (i = 0; i < instr->data_type->dimx; ++i)
                        res->value[i].u = arg1->value[i].u * arg2->value[i].u;
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
    hlsl_replace_node(&expr->node, &res->node);
    return true;
}
