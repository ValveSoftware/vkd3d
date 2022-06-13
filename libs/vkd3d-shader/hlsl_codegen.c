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

static unsigned int minor_size(const struct hlsl_type *type)
{
    if (type->modifiers & HLSL_MODIFIER_ROW_MAJOR)
        return type->dimx;
    else
        return type->dimy;
}

static unsigned int major_size(const struct hlsl_type *type)
{
    if (type->modifiers & HLSL_MODIFIER_ROW_MAJOR)
        return type->dimy;
    else
        return type->dimx;
}

static bool shader_is_sm_5_1(const struct hlsl_ctx *ctx)
{
    return ctx->profile->major_version == 5 && ctx->profile->minor_version >= 1;
}

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

    if (!(uniform = hlsl_new_var(ctx, temp->name, temp->data_type,
            temp->loc, NULL, temp->modifiers, &temp->reg_reservation)))
        return;
    list_add_before(&temp->scope_entry, &uniform->scope_entry);
    list_add_tail(&ctx->extern_vars, &uniform->extern_entry);
    uniform->is_uniform = 1;
    uniform->is_param = temp->is_param;
    uniform->buffer = temp->buffer;

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
        struct hlsl_type *type, unsigned int field_offset, unsigned int modifiers, const struct hlsl_semantic *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_semantic new_semantic;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_load *load;
    struct hlsl_ir_var *input;

    if (type->type == HLSL_CLASS_MATRIX)
    {
        struct hlsl_type *vector_type = hlsl_get_vector_type(ctx, type->base_type, minor_size(type));
        struct hlsl_semantic vector_semantic = *semantic;
        unsigned int i;

        for (i = 0; i < major_size(type); ++i)
        {
            prepend_input_copy(ctx, instrs, var, vector_type, 4 * i, modifiers, &vector_semantic);
            ++vector_semantic.index;
        }

        return;
    }

    if (!(name = hlsl_get_string_buffer(ctx)))
        return;
    vkd3d_string_buffer_printf(name, "<input-%s%u>", semantic->name, semantic->index);
    if (!(new_semantic.name = hlsl_strdup(ctx, semantic->name)))
    {
        hlsl_release_string_buffer(ctx, name);
        return;
    }
    new_semantic.index = semantic->index;
    if (!(input = hlsl_new_var(ctx, hlsl_strdup(ctx, name->buffer),
            type, var->loc, &new_semantic, modifiers, NULL)))
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

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset, &var->loc)))
        return;
    list_add_after(&load->node.entry, &offset->node.entry);

    if (!(store = hlsl_new_store(ctx, var, &offset->node, &load->node, 0, var->loc)))
        return;
    list_add_after(&offset->node.entry, &store->node.entry);
}

static void prepend_input_struct_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset)
{
    size_t i;

    for (i = 0; i < type->e.record.field_count; ++i)
    {
        const struct hlsl_struct_field *field = &type->e.record.fields[i];

        if (field->type->type == HLSL_CLASS_STRUCT)
            prepend_input_struct_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset);
        else if (field->semantic.name)
            prepend_input_copy(ctx, instrs, var, field->type,
                    field_offset + field->reg_offset, field->modifiers, &field->semantic);
        else
            hlsl_error(ctx, &field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
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
        prepend_input_copy(ctx, instrs, var, var->data_type, 0, var->modifiers, &var->semantic);
}

static void append_output_copy(struct hlsl_ctx *ctx, struct list *instrs, struct hlsl_ir_var *var,
        struct hlsl_type *type, unsigned int field_offset, unsigned int modifiers, const struct hlsl_semantic *semantic)
{
    struct vkd3d_string_buffer *name;
    struct hlsl_semantic new_semantic;
    struct hlsl_ir_constant *offset;
    struct hlsl_ir_store *store;
    struct hlsl_ir_var *output;
    struct hlsl_ir_load *load;

    if (type->type == HLSL_CLASS_MATRIX)
    {
        struct hlsl_type *vector_type = hlsl_get_vector_type(ctx, type->base_type, minor_size(type));
        struct hlsl_semantic vector_semantic = *semantic;
        unsigned int i;

        for (i = 0; i < major_size(type); ++i)
        {
            append_output_copy(ctx, instrs, var, vector_type, 4 * i, modifiers, &vector_semantic);
            ++vector_semantic.index;
        }

        return;
    }

    if (!(name = hlsl_get_string_buffer(ctx)))
        return;
    vkd3d_string_buffer_printf(name, "<output-%s%u>", semantic->name, semantic->index);
    if (!(new_semantic.name = hlsl_strdup(ctx, semantic->name)))
    {
        hlsl_release_string_buffer(ctx, name);
        return;
    }
    new_semantic.index = semantic->index;
    if (!(output = hlsl_new_var(ctx, hlsl_strdup(ctx, name->buffer),
            type, var->loc, &new_semantic, modifiers, NULL)))
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

    if (!(offset = hlsl_new_uint_constant(ctx, field_offset, &var->loc)))
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
    size_t i;

    for (i = 0; i < type->e.record.field_count; ++i)
    {
        const struct hlsl_struct_field *field = &type->e.record.fields[i];

        if (field->type->type == HLSL_CLASS_STRUCT)
            append_output_struct_copy(ctx, instrs, var, field->type, field_offset + field->reg_offset);
        else if (field->semantic.name)
            append_output_copy(ctx, instrs, var, field->type,
                    field_offset + field->reg_offset, field->modifiers, &field->semantic);
        else
            hlsl_error(ctx, &field->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
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
        append_output_copy(ctx, instrs, var, var->data_type, 0, var->modifiers, &var->semantic);
}

static bool transform_ir(struct hlsl_ctx *ctx, bool (*func)(struct hlsl_ctx *ctx, struct hlsl_ir_node *, void *),
        struct hlsl_block *block, void *context)
{
    struct hlsl_ir_node *instr, *next;
    bool progress = false;

    LIST_FOR_EACH_ENTRY_SAFE(instr, next, &block->instrs, struct hlsl_ir_node, entry)
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

struct recursive_call_ctx
{
    const struct hlsl_ir_function_decl **backtrace;
    size_t count, capacity;
};

static bool find_recursive_calls(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct recursive_call_ctx *call_ctx = context;
    struct hlsl_ir_function_decl *decl;
    const struct hlsl_ir_call *call;
    size_t i;

    if (instr->type != HLSL_IR_CALL)
        return false;
    call = hlsl_ir_call(instr);
    decl = call->decl;

    for (i = 0; i < call_ctx->count; ++i)
    {
        if (call_ctx->backtrace[i] == decl)
        {
            hlsl_error(ctx, &call->node.loc, VKD3D_SHADER_ERROR_HLSL_RECURSIVE_CALL,
                    "Recursive call to \"%s\".", decl->func->name);
            return false;
        }
    }

    if (!hlsl_array_reserve(ctx, (void **)&call_ctx->backtrace, &call_ctx->capacity,
            call_ctx->count + 1, sizeof(*call_ctx->backtrace)))
        return false;
    call_ctx->backtrace[call_ctx->count++] = decl;

    transform_ir(ctx, find_recursive_calls, &decl->body, call_ctx);

    --call_ctx->count;

    return false;
}

static void insert_early_return_break(struct hlsl_ctx *ctx,
        struct hlsl_ir_function_decl *func, struct hlsl_ir_node *cf_instr)
{
    struct hlsl_ir_jump *jump;
    struct hlsl_ir_load *load;
    struct hlsl_ir_if *iff;

    if (!(load = hlsl_new_var_load(ctx, func->early_return_var, cf_instr->loc)))
        return;
    list_add_after(&cf_instr->entry, &load->node.entry);

    if (!(iff = hlsl_new_if(ctx, &load->node, cf_instr->loc)))
        return;
    list_add_after(&load->node.entry, &iff->node.entry);

    if (!(jump = hlsl_new_jump(ctx, HLSL_IR_JUMP_BREAK, cf_instr->loc)))
        return;
    list_add_tail(&iff->then_instrs.instrs, &jump->node.entry);
}

/* Remove HLSL_IR_JUMP_RETURN calls by altering subsequent control flow. */
static bool lower_return(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *func,
        struct hlsl_block *block, bool in_loop)
{
    struct hlsl_ir_node *return_instr = NULL, *cf_instr = NULL;
    struct hlsl_ir_node *instr, *next;
    bool has_early_return = false;

    /* SM1 has no function calls. SM4 does, but native d3dcompiler inlines
     * everything anyway. We are safest following suit.
     *
     * The basic idea is to keep track of whether the function has executed an
     * early return in a synthesized boolean variable (func->early_return_var)
     * and guard all code after the return on that variable being false. In the
     * case of loops we also replace the return with a break.
     *
     * The following algorithm loops over instructions until it hits either a
     * return statement, or a CF block which might contain a return statement.
     *
     * In the former case, we remove everything after the return statement in
     * this block. We have to stop and do this in a separate loop, because we
     * have to remove statements in reverse.
     *
     * In the latter case, we stop, pull out everything after the CF
     * instruction, shove it into an if block, and then call lower_return() on
     * that if block. We have to do it this way to make the following case work:
     *
     *     if (...)
     *         return;
     *     foo();
     *     if (...)
     *         return;
     *     bar();
     *
     * Both foo() and bar() need to be separately guarded out, and we can't
     * really do that without breaking out of this loop.
     *
     * (We could return a "did we make progress" like transform_ir(), but we
     * already know the only block that still needs addressing, so there's not
     * much point.)
     *
     * If we're inside of a loop, on the other hand, we do things a little
     * differently. "break" offers similar enough semantics that we can use it
     * instead of "return" and just keep going. (Moreover we kind of have to
     * break; if we've returned we don't want to execute *any* of the loop.)
     */

    LIST_FOR_EACH_ENTRY_SAFE(instr, next, &block->instrs, struct hlsl_ir_node, entry)
    {
        if (instr->type == HLSL_IR_CALL)
        {
            struct hlsl_ir_call *call = hlsl_ir_call(instr);

            lower_return(ctx, call->decl, &call->decl->body, false);
        }
        else if (instr->type == HLSL_IR_IF)
        {
            struct hlsl_ir_if *iff = hlsl_ir_if(instr);

            has_early_return |= lower_return(ctx, func, &iff->then_instrs, in_loop);
            has_early_return |= lower_return(ctx, func, &iff->else_instrs, in_loop);

            if (has_early_return)
            {
                /* If we're in a loop, we actually don't need to emit a break
                 * instruction. The return itself will be translated into a
                 * break, and we'll emit a break after any loop instructions
                 * containing it. */
                if (!in_loop)
                {
                    cf_instr = instr;
                    break;
                }
            }
        }
        else if (instr->type == HLSL_IR_LOOP)
        {
            if (lower_return(ctx, func, &hlsl_ir_loop(instr)->body, true))
            {
                has_early_return = true;
                if (in_loop)
                {
                    insert_early_return_break(ctx, func, instr);
                }
                else
                {
                    cf_instr = instr;
                    break;
                }
            }
        }
        else if (instr->type == HLSL_IR_JUMP)
        {
            struct hlsl_ir_jump *jump = hlsl_ir_jump(instr);
            struct hlsl_ir_constant *constant;
            struct hlsl_ir_store *store;

            if (jump->type == HLSL_IR_JUMP_RETURN)
            {
                if (!func->early_return_var)
                {
                    struct vkd3d_string_buffer *string;

                    if (!(string = hlsl_get_string_buffer(ctx)))
                        return false;
                    vkd3d_string_buffer_printf(string, "<early_return-%s>", func->func->name);
                    if (!(func->early_return_var = hlsl_new_synthetic_var(ctx, string->buffer,
                            hlsl_get_scalar_type(ctx, HLSL_TYPE_BOOL), jump->node.loc)))
                        return false;

                    if (!(constant = hlsl_new_bool_constant(ctx, false, &jump->node.loc)))
                        return false;
                    list_add_head(&func->body.instrs, &constant->node.entry);

                    if (!(store = hlsl_new_simple_store(ctx, func->early_return_var, &constant->node)))
                        return false;
                    list_add_after(&constant->node.entry, &store->node.entry);
                }
                if (!(constant = hlsl_new_bool_constant(ctx, true, &jump->node.loc)))
                    return false;
                list_add_before(&jump->node.entry, &constant->node.entry);

                if (!(store = hlsl_new_simple_store(ctx, func->early_return_var, &constant->node)))
                    return false;
                list_add_after(&constant->node.entry, &store->node.entry);

                has_early_return = true;
                if (in_loop)
                {
                    jump->type = HLSL_IR_JUMP_BREAK;
                }
                else
                {
                    return_instr = instr;
                    break;
                }
            }
        }
    }

    if (return_instr)
    {
        /* If we're in a loop, we should have used "break" instead. */
        assert(!in_loop);

        /* Iterate in reverse, to avoid use-after-free when unlinking sources from
         * the "uses" list. */
        LIST_FOR_EACH_ENTRY_SAFE_REV(instr, next, &block->instrs, struct hlsl_ir_node, entry)
        {
            list_remove(&instr->entry);
            hlsl_free_instr(instr);

            /* Yes, we just freed it, but we're comparing pointers. */
            if (instr == return_instr)
                break;
        }
    }
    else if (cf_instr)
    {
        struct list *tail = list_tail(&block->instrs);
        struct hlsl_ir_load *load;
        struct hlsl_ir_node *not;
        struct hlsl_ir_if *iff;

        /* If we're in a loop, we should have used "break" instead. */
        assert(!in_loop);

        if (tail == &cf_instr->entry)
            return has_early_return;

        if (!(load = hlsl_new_var_load(ctx, func->early_return_var, cf_instr->loc)))
            return false;
        list_add_tail(&block->instrs, &load->node.entry);

        if (!(not = hlsl_new_unary_expr(ctx, HLSL_OP1_LOGIC_NOT, &load->node, cf_instr->loc)))
            return false;
        list_add_tail(&block->instrs, &not->entry);

        if (!(iff = hlsl_new_if(ctx, not, cf_instr->loc)))
            return false;
        list_add_tail(&block->instrs, &iff->node.entry);

        list_move_slice_tail(&iff->then_instrs.instrs, list_next(&block->instrs, &cf_instr->entry), tail);

        lower_return(ctx, func, &iff->then_instrs, in_loop);
    }

    return has_early_return;
}

/* Remove HLSL_IR_CALL instructions by inlining them. */
static bool lower_calls(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_ir_function_decl *decl;
    struct hlsl_ir_call *call;
    struct hlsl_block block;

    if (instr->type != HLSL_IR_CALL)
        return false;
    call = hlsl_ir_call(instr);
    decl = call->decl;

    if (!decl->has_body)
        hlsl_error(ctx, &call->node.loc, VKD3D_SHADER_ERROR_HLSL_NOT_DEFINED,
                "Function \"%s\" is not defined.", decl->func->name);

    list_init(&block.instrs);
    if (!hlsl_clone_block(ctx, &block, &decl->body))
        return false;
    list_move_before(&call->node.entry, &block.instrs);

    list_remove(&call->node.entry);
    hlsl_free_instr(&call->node);
    return true;
}

/* Lower casts from vec1 to vecN to swizzles. */
static bool lower_broadcasts(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_type *src_type, *dst_type;
    struct hlsl_type *dst_scalar_type;
    struct hlsl_ir_expr *cast;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    cast = hlsl_ir_expr(instr);
    if (cast->op != HLSL_OP1_CAST)
        return false;
    src_type = cast->operands[0].node->data_type;
    dst_type = cast->node.data_type;

    if (src_type->type <= HLSL_CLASS_VECTOR && dst_type->type <= HLSL_CLASS_VECTOR && src_type->dimx == 1)
    {
        struct hlsl_ir_node *replacement;
        struct hlsl_ir_swizzle *swizzle;
        struct hlsl_ir_expr *new_cast;

        dst_scalar_type = hlsl_get_scalar_type(ctx, dst_type->base_type);
        /* We need to preserve the cast since it might be doing more than just
         * turning the scalar into a vector. */
        if (!(new_cast = hlsl_new_cast(ctx, cast->operands[0].node, dst_scalar_type, &cast->node.loc)))
            return false;
        list_add_after(&cast->node.entry, &new_cast->node.entry);
        replacement = &new_cast->node;

        if (dst_type->dimx != 1)
        {
            if (!(swizzle = hlsl_new_swizzle(ctx, HLSL_SWIZZLE(X, X, X, X), dst_type->dimx, replacement, &cast->node.loc)))
                return false;
            list_add_after(&new_cast->node.entry, &swizzle->node.entry);
            replacement = &swizzle->node;
        }

        hlsl_replace_node(&cast->node, replacement);
        return true;
    }

    return false;
}

enum copy_propagation_value_state
{
    VALUE_STATE_NOT_WRITTEN = 0,
    VALUE_STATE_STATICALLY_WRITTEN,
    VALUE_STATE_DYNAMICALLY_WRITTEN,
};

struct copy_propagation_value
{
    enum copy_propagation_value_state state;
    struct hlsl_ir_node *node;
    unsigned int component;
};

struct copy_propagation_var_def
{
    struct rb_entry entry;
    struct hlsl_ir_var *var;
    struct copy_propagation_value values[];
};

struct copy_propagation_state
{
    struct rb_tree var_defs;
    struct copy_propagation_state *parent;
};

static int copy_propagation_var_def_compare(const void *key, const struct rb_entry *entry)
{
    struct copy_propagation_var_def *var_def = RB_ENTRY_VALUE(entry, struct copy_propagation_var_def, entry);
    uintptr_t key_int = (uintptr_t)key, entry_int = (uintptr_t)var_def->var;

    return (key_int > entry_int) - (key_int < entry_int);
}

static void copy_propagation_var_def_destroy(struct rb_entry *entry, void *context)
{
    struct copy_propagation_var_def *var_def = RB_ENTRY_VALUE(entry, struct copy_propagation_var_def, entry);

    vkd3d_free(var_def);
}

static struct copy_propagation_value *copy_propagation_get_value(const struct copy_propagation_state *state,
        const struct hlsl_ir_var *var, unsigned component)
{
    for (; state; state = state->parent)
    {
        struct rb_entry *entry = rb_get(&state->var_defs, var);
        if (entry)
        {
            struct copy_propagation_var_def *var_def = RB_ENTRY_VALUE(entry, struct copy_propagation_var_def, entry);
            enum copy_propagation_value_state state = var_def->values[component].state;

            assert(component < var_def->var->data_type->reg_size);

            switch (state)
            {
                case VALUE_STATE_STATICALLY_WRITTEN:
                    return &var_def->values[component];
                case VALUE_STATE_DYNAMICALLY_WRITTEN:
                    return NULL;
                case VALUE_STATE_NOT_WRITTEN:
                    break;
            }
        }
    }

    return NULL;
}

static struct copy_propagation_var_def *copy_propagation_create_var_def(struct hlsl_ctx *ctx,
        struct copy_propagation_state *state, struct hlsl_ir_var *var)
{
    struct rb_entry *entry = rb_get(&state->var_defs, var);
    struct copy_propagation_var_def *var_def;
    int res;

    if (entry)
        return RB_ENTRY_VALUE(entry, struct copy_propagation_var_def, entry);

    if (!(var_def = hlsl_alloc(ctx, offsetof(struct copy_propagation_var_def, values[var->data_type->reg_size]))))
        return NULL;

    var_def->var = var;

    res = rb_put(&state->var_defs, var, &var_def->entry);
    assert(!res);

    return var_def;
}

static void copy_propagation_invalidate_variable(struct copy_propagation_var_def *var_def,
        unsigned offset, unsigned char writemask)
{
    unsigned i;

    TRACE("Invalidate variable %s[%u]%s.\n", var_def->var->name, offset, debug_hlsl_writemask(writemask));

    for (i = 0; i < 4; ++i)
    {
        if (writemask & (1u << i))
            var_def->values[offset + i].state = VALUE_STATE_DYNAMICALLY_WRITTEN;
    }
}

static void copy_propagation_invalidate_whole_variable(struct copy_propagation_var_def *var_def)
{
    unsigned i;

    TRACE("Invalidate variable %s.\n", var_def->var->name);

    for (i = 0; i < var_def->var->data_type->reg_size; ++i)
        var_def->values[i].state = VALUE_STATE_DYNAMICALLY_WRITTEN;
}

static void copy_propagation_set_value(struct copy_propagation_var_def *var_def, unsigned int offset,
        unsigned char writemask, struct hlsl_ir_node *node)
{
    unsigned int i, j = 0;

    for (i = 0; i < 4; ++i)
    {
        if (writemask & (1u << i))
        {
            TRACE("Variable %s[%u] is written by instruction %p%s.\n",
                    var_def->var->name, offset + i, node, debug_hlsl_writemask(1u << i));
            var_def->values[offset + i].state = VALUE_STATE_STATICALLY_WRITTEN;
            var_def->values[offset + i].node = node;
            var_def->values[offset + i].component = j++;
        }
    }
}

static struct hlsl_ir_node *copy_propagation_compute_constant(struct hlsl_ctx *ctx, struct copy_propagation_state *state,
        const struct hlsl_deref *deref, unsigned int count, const struct vkd3d_shader_location *loc)
{
    const struct hlsl_ir_var *var = deref->var;
    enum hlsl_base_type type = HLSL_TYPE_VOID;
    struct hlsl_ir_constant *constant = NULL;
    struct hlsl_ir_node *store_nodes[4];
    unsigned store_components[4];
    unsigned int i, offset;

    if (!hlsl_offset_from_deref(ctx, deref, &offset))
        return NULL;

    if (var->data_type->type != HLSL_CLASS_OBJECT)
        assert(offset + count <= var->data_type->reg_size);

    for (i = 0; i < count; ++i)
    {
        struct copy_propagation_value *value = copy_propagation_get_value(state, var, offset + i);
        struct hlsl_ir_node *store_node;
        enum hlsl_base_type store_type;

        if (!value)
            return NULL;

        store_node = value->node;
        if (!store_node || store_node->type != HLSL_IR_CONSTANT)
            return NULL;

        store_type = store_node->data_type->base_type;

        if (type == HLSL_TYPE_VOID)
            type = store_type;
        else if (type != store_type)
            return NULL;

        store_nodes[i] = store_node;
        store_components[i] = value->component;
    }

    if (!(constant = hlsl_new_uint_constant(ctx, 0, loc)))
        return NULL;
    constant->node.data_type = hlsl_get_vector_type(ctx, type, count);

    for (i = 0; i < count; ++i)
        constant->value[i] = hlsl_ir_constant(store_nodes[i])->value[store_components[i]];

    TRACE("Load from %s[%d-%d] reconstructed as constant value.\n",
            var->name, offset, offset + count);

    return &constant->node;
}

static struct hlsl_ir_node *copy_propagation_compute_replacement(struct hlsl_ctx *ctx,
        const struct copy_propagation_state *state, const struct hlsl_deref *deref,
        unsigned int count, unsigned int *swizzle)
{
    const struct hlsl_ir_var *var = deref->var;
    struct hlsl_ir_node *node = NULL;
    unsigned int offset, i;

    if (!hlsl_offset_from_deref(ctx, deref, &offset))
        return NULL;

    if (var->data_type->type != HLSL_CLASS_OBJECT)
        assert(offset + count <= var->data_type->reg_size);

    *swizzle = 0;

    for (i = 0; i < count; ++i)
    {
        struct copy_propagation_value *value = copy_propagation_get_value(state, var, offset + i);

        if (!value)
            return NULL;

        if (!node)
        {
            node = value->node;
        }
        else if (node != value->node)
        {
            TRACE("No single source for propagating load from %s[%u-%u].\n", var->name, offset, offset + count);
            return NULL;
        }
        *swizzle |= value->component << i * 2;
    }

    TRACE("Load from %s[%u-%u] propagated as instruction %p%s.\n",
            var->name, offset, offset + count, node, debug_hlsl_swizzle(*swizzle, count));
    return node;
}

static bool copy_propagation_transform_load(struct hlsl_ctx *ctx,
        struct hlsl_ir_load *load, struct copy_propagation_state *state)
{
    struct hlsl_ir_node *node = &load->node, *new_node;
    struct hlsl_type *type = node->data_type;
    struct hlsl_ir_swizzle *swizzle_node;
    unsigned int dimx = 0;
    unsigned int swizzle;

    switch (type->type)
    {
        case HLSL_CLASS_SCALAR:
        case HLSL_CLASS_VECTOR:
            dimx = type->dimx;
            break;

        case HLSL_CLASS_OBJECT:
            dimx = 1;
            break;

        case HLSL_CLASS_MATRIX:
        case HLSL_CLASS_ARRAY:
        case HLSL_CLASS_STRUCT:
            /* FIXME: Actually we shouldn't even get here, but we don't split
             * matrices yet. */
            return false;
    }

    if ((new_node = copy_propagation_compute_constant(ctx, state, &load->src, dimx, &node->loc)))
    {
        list_add_before(&node->entry, &new_node->entry);
        hlsl_replace_node(node, new_node);
        return true;
    }

    if ((new_node = copy_propagation_compute_replacement(ctx, state, &load->src, dimx, &swizzle)))
    {
        if (type->type != HLSL_CLASS_OBJECT)
        {
            if (!(swizzle_node = hlsl_new_swizzle(ctx, swizzle, dimx, new_node, &node->loc)))
                return false;
            list_add_before(&node->entry, &swizzle_node->node.entry);
            new_node = &swizzle_node->node;
        }
        hlsl_replace_node(node, new_node);
        return true;
    }

    return false;
}

static bool copy_propagation_transform_object_load(struct hlsl_ctx *ctx,
        struct hlsl_deref *deref, struct copy_propagation_state *state)
{
    struct hlsl_ir_load *load;
    struct hlsl_ir_node *node;
    unsigned int swizzle;

    if (!(node = copy_propagation_compute_replacement(ctx, state, deref, 1, &swizzle)))
        return false;

    /* Only HLSL_IR_LOAD can produce an object. */
    load = hlsl_ir_load(node);
    deref->var = load->src.var;
    hlsl_src_remove(&deref->offset);
    hlsl_src_from_node(&deref->offset, load->src.offset.node);
    return true;
}

static bool copy_propagation_transform_resource_load(struct hlsl_ctx *ctx,
        struct hlsl_ir_resource_load *load, struct copy_propagation_state *state)
{
    bool progress = false;

    progress |= copy_propagation_transform_object_load(ctx, &load->resource, state);
    if (load->sampler.var)
        progress |= copy_propagation_transform_object_load(ctx, &load->sampler, state);
    return progress;
}

static bool copy_propagation_transform_resource_store(struct hlsl_ctx *ctx,
        struct hlsl_ir_resource_store *store, struct copy_propagation_state *state)
{
    bool progress = false;

    progress |= copy_propagation_transform_object_load(ctx, &store->resource, state);
    return progress;
}

static void copy_propagation_record_store(struct hlsl_ctx *ctx, struct hlsl_ir_store *store,
        struct copy_propagation_state *state)
{
    struct copy_propagation_var_def *var_def;
    struct hlsl_deref *lhs = &store->lhs;
    struct hlsl_ir_var *var = lhs->var;
    unsigned int offset;

    if (!(var_def = copy_propagation_create_var_def(ctx, state, var)))
        return;

    if (hlsl_offset_from_deref(ctx, lhs, &offset))
    {
        unsigned int writemask = store->writemask;

        if (store->rhs.node->data_type->type == HLSL_CLASS_OBJECT)
            writemask = VKD3DSP_WRITEMASK_0;
        copy_propagation_set_value(var_def, offset, writemask, store->rhs.node);
    }
    else
    {
        copy_propagation_invalidate_whole_variable(var_def);
    }
}

static void copy_propagation_state_init(struct hlsl_ctx *ctx, struct copy_propagation_state *state,
        struct copy_propagation_state *parent)
{
    rb_init(&state->var_defs, copy_propagation_var_def_compare);
    state->parent = parent;
}

static void copy_propagation_state_destroy(struct copy_propagation_state *state)
{
    rb_destroy(&state->var_defs, copy_propagation_var_def_destroy, NULL);
}

static void copy_propagation_invalidate_from_block(struct hlsl_ctx *ctx, struct copy_propagation_state *state,
        struct hlsl_block *block)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, &block->instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
            case HLSL_IR_STORE:
            {
                struct hlsl_ir_store *store = hlsl_ir_store(instr);
                struct copy_propagation_var_def *var_def;
                struct hlsl_deref *lhs = &store->lhs;
                struct hlsl_ir_var *var = lhs->var;
                unsigned int offset;

                if (!(var_def = copy_propagation_create_var_def(ctx, state, var)))
                    continue;

                if (hlsl_offset_from_deref(ctx, lhs, &offset))
                    copy_propagation_invalidate_variable(var_def, offset, store->writemask);
                else
                    copy_propagation_invalidate_whole_variable(var_def);

                break;
            }

            case HLSL_IR_IF:
            {
                struct hlsl_ir_if *iff = hlsl_ir_if(instr);

                copy_propagation_invalidate_from_block(ctx, state, &iff->then_instrs);
                copy_propagation_invalidate_from_block(ctx, state, &iff->else_instrs);

                break;
            }

            case HLSL_IR_LOOP:
            {
                struct hlsl_ir_loop *loop = hlsl_ir_loop(instr);

                copy_propagation_invalidate_from_block(ctx, state, &loop->body);

                break;
            }

            default:
                break;
        }
    }
}

static bool copy_propagation_transform_block(struct hlsl_ctx *ctx, struct hlsl_block *block,
        struct copy_propagation_state *state);

static bool copy_propagation_process_if(struct hlsl_ctx *ctx, struct hlsl_ir_if *iff,
        struct copy_propagation_state *state)
{
    struct copy_propagation_state inner_state;
    bool progress = false;

    copy_propagation_state_init(ctx, &inner_state, state);
    progress |= copy_propagation_transform_block(ctx, &iff->then_instrs, &inner_state);
    copy_propagation_state_destroy(&inner_state);

    copy_propagation_state_init(ctx, &inner_state, state);
    progress |= copy_propagation_transform_block(ctx, &iff->else_instrs, &inner_state);
    copy_propagation_state_destroy(&inner_state);

    /* Ideally we'd invalidate the outer state looking at what was
     * touched in the two inner states, but this doesn't work for
     * loops (because we need to know what is invalidated in advance),
     * so we need copy_propagation_invalidate_from_block() anyway. */
    copy_propagation_invalidate_from_block(ctx, state, &iff->then_instrs);
    copy_propagation_invalidate_from_block(ctx, state, &iff->else_instrs);

    return progress;
}

static bool copy_propagation_process_loop(struct hlsl_ctx *ctx, struct hlsl_ir_loop *loop,
        struct copy_propagation_state *state)
{
    struct copy_propagation_state inner_state;
    bool progress = false;

    copy_propagation_invalidate_from_block(ctx, state, &loop->body);

    copy_propagation_state_init(ctx, &inner_state, state);
    progress |= copy_propagation_transform_block(ctx, &loop->body, &inner_state);
    copy_propagation_state_destroy(&inner_state);

    return progress;
}

static bool copy_propagation_transform_block(struct hlsl_ctx *ctx, struct hlsl_block *block,
        struct copy_propagation_state *state)
{
    struct hlsl_ir_node *instr, *next;
    bool progress = false;

    LIST_FOR_EACH_ENTRY_SAFE(instr, next, &block->instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
            case HLSL_IR_LOAD:
                progress |= copy_propagation_transform_load(ctx, hlsl_ir_load(instr), state);
                break;

            case HLSL_IR_RESOURCE_LOAD:
                progress |= copy_propagation_transform_resource_load(ctx, hlsl_ir_resource_load(instr), state);
                break;

            case HLSL_IR_RESOURCE_STORE:
                progress |= copy_propagation_transform_resource_store(ctx, hlsl_ir_resource_store(instr), state);
                break;

            case HLSL_IR_STORE:
                copy_propagation_record_store(ctx, hlsl_ir_store(instr), state);
                break;

            case HLSL_IR_IF:
                progress |= copy_propagation_process_if(ctx, hlsl_ir_if(instr), state);
                break;

            case HLSL_IR_LOOP:
                progress |= copy_propagation_process_loop(ctx, hlsl_ir_loop(instr), state);
                break;

            default:
                break;
        }
    }

    return progress;
}

static bool copy_propagation_execute(struct hlsl_ctx *ctx, struct hlsl_block *block)
{
    struct copy_propagation_state state;
    bool progress;

    copy_propagation_state_init(ctx, &state, NULL);

    progress = copy_propagation_transform_block(ctx, block, &state);

    copy_propagation_state_destroy(&state);

    return progress;
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
        const struct hlsl_type *dst_type = expr->node.data_type;
        const struct hlsl_type *src_type;

        if (expr->op != HLSL_OP1_CAST)
            return false;

        src_type = expr->operands[0].node->data_type;

        if (hlsl_types_are_equal(src_type, dst_type)
                || (src_type->base_type == dst_type->base_type && is_vec1(src_type) && is_vec1(dst_type)))
        {
            hlsl_replace_node(&expr->node, expr->operands[0].node);
            return true;
        }
    }

    return false;
}

/* Create a new load from src_load, with the given type and extra offset.
 * Inserts new instructions right after "src_load", and returns a pointer to
 * the resulting load instruction. */
static struct hlsl_ir_load *split_load(struct hlsl_ctx *ctx,
        struct hlsl_ir_load *src_load, const unsigned int offset, struct hlsl_type *type)
{
    struct hlsl_ir_node *offset_instr, *add;
    struct hlsl_ir_load *dst_load;
    struct hlsl_ir_constant *c;

    if (!(c = hlsl_new_uint_constant(ctx, offset, &src_load->node.loc)))
        return NULL;
    list_add_after(&src_load->node.entry, &c->node.entry);

    offset_instr = &c->node;
    if (src_load->src.offset.node)
    {
        if (!(add = hlsl_new_binary_expr(ctx, HLSL_OP2_ADD, src_load->src.offset.node, &c->node)))
            return NULL;
        list_add_after(&c->node.entry, &add->entry);
        offset_instr = add;
    }
    if (!(dst_load = hlsl_new_load(ctx, src_load->src.var, offset_instr, type, src_load->node.loc)))
        return NULL;
    list_add_after(&offset_instr->entry, &dst_load->node.entry);
    return dst_load;
}

/* Copy an element of a complex variable. Helper for
 * split_array_copies(), split_struct_copies() and
 * split_matrix_copies(). Inserts new instructions right before
 * "store". */
static bool split_copy(struct hlsl_ctx *ctx, struct hlsl_ir_store *src_store,
        struct hlsl_ir_load *src_load, const unsigned int offset, struct hlsl_type *type)
{
    struct hlsl_ir_node *offset_instr, *add;
    struct hlsl_ir_store *dst_store;
    struct hlsl_ir_load *dst_load;
    struct hlsl_ir_constant *c;

    if (!(dst_load = split_load(ctx, src_load, offset, type)))
        return false;

    if (!(c = hlsl_new_uint_constant(ctx, offset, &src_store->node.loc)))
        return NULL;
    list_add_before(&src_store->node.entry, &c->node.entry);

    offset_instr = &c->node;
    if (src_store->lhs.offset.node)
    {
        if (!(add = hlsl_new_binary_expr(ctx, HLSL_OP2_ADD, src_store->lhs.offset.node, &c->node)))
            return false;
        list_add_before(&src_store->node.entry, &add->entry);
        offset_instr = add;
    }

    if (!(dst_store = hlsl_new_store(ctx, src_store->lhs.var, offset_instr, &dst_load->node, 0, src_store->node.loc)))
        return false;
    list_add_before(&src_store->node.entry, &dst_store->node.entry);
    return true;
}

static bool split_array_copies(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_ir_node *rhs;
    struct hlsl_type *element_type;
    const struct hlsl_type *type;
    unsigned int element_size, i;
    struct hlsl_ir_store *store;

    if (instr->type != HLSL_IR_STORE)
        return false;

    store = hlsl_ir_store(instr);
    rhs = store->rhs.node;
    type = rhs->data_type;
    if (type->type != HLSL_CLASS_ARRAY)
        return false;
    element_type = type->e.array.type;
    element_size = hlsl_type_get_array_element_reg_size(element_type);

    if (rhs->type != HLSL_IR_LOAD)
    {
        hlsl_fixme(ctx, &instr->loc, "Array store rhs is not HLSL_IR_LOAD. Broadcast may be missing.");
        return false;
    }

    for (i = 0; i < type->e.array.elements_count; ++i)
    {
        if (!split_copy(ctx, store, hlsl_ir_load(rhs), i * element_size, element_type))
            return false;
    }

    /* Remove the store instruction, so that we can split structs which contain
     * other structs. Although assignments produce a value, we don't allow
     * HLSL_IR_STORE to be used as a source. */
    list_remove(&store->node.entry);
    hlsl_free_instr(&store->node);
    return true;
}

static bool split_struct_copies(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_ir_node *rhs;
    const struct hlsl_type *type;
    struct hlsl_ir_store *store;
    size_t i;

    if (instr->type != HLSL_IR_STORE)
        return false;

    store = hlsl_ir_store(instr);
    rhs = store->rhs.node;
    type = rhs->data_type;
    if (type->type != HLSL_CLASS_STRUCT)
        return false;

    if (rhs->type != HLSL_IR_LOAD)
    {
        hlsl_fixme(ctx, &instr->loc, "Struct store rhs is not HLSL_IR_LOAD. Broadcast may be missing.");
        return false;
    }

    for (i = 0; i < type->e.record.field_count; ++i)
    {
        const struct hlsl_struct_field *field = &type->e.record.fields[i];

        if (!split_copy(ctx, store, hlsl_ir_load(rhs), field->reg_offset, field->type))
            return false;
    }

    /* Remove the store instruction, so that we can split structs which contain
     * other structs. Although assignments produce a value, we don't allow
     * HLSL_IR_STORE to be used as a source. */
    list_remove(&store->node.entry);
    hlsl_free_instr(&store->node);
    return true;
}

static bool split_matrix_copies(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_ir_node *rhs;
    struct hlsl_type *element_type;
    const struct hlsl_type *type;
    unsigned int i;
    struct hlsl_ir_store *store;

    if (instr->type != HLSL_IR_STORE)
        return false;

    store = hlsl_ir_store(instr);
    rhs = store->rhs.node;
    type = rhs->data_type;
    if (type->type != HLSL_CLASS_MATRIX)
        return false;
    element_type = hlsl_get_vector_type(ctx, type->base_type, minor_size(type));

    if (rhs->type != HLSL_IR_LOAD)
    {
        hlsl_fixme(ctx, &instr->loc, "Copying from unsupported node type.\n");
        return false;
    }

    for (i = 0; i < major_size(type); ++i)
    {
        if (!split_copy(ctx, store, hlsl_ir_load(rhs), 4 * i, element_type))
            return false;
    }

    list_remove(&store->node.entry);
    hlsl_free_instr(&store->node);
    return true;
}

static bool lower_narrowing_casts(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    const struct hlsl_type *src_type, *dst_type;
    struct hlsl_type *dst_vector_type;
    struct hlsl_ir_expr *cast;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    cast = hlsl_ir_expr(instr);
    if (cast->op != HLSL_OP1_CAST)
        return false;
    src_type = cast->operands[0].node->data_type;
    dst_type = cast->node.data_type;

    if (src_type->type <= HLSL_CLASS_VECTOR && dst_type->type <= HLSL_CLASS_VECTOR && dst_type->dimx < src_type->dimx)
    {
        struct hlsl_ir_swizzle *swizzle;
        struct hlsl_ir_expr *new_cast;

        dst_vector_type = hlsl_get_vector_type(ctx, dst_type->base_type, src_type->dimx);
        /* We need to preserve the cast since it might be doing more than just
         * narrowing the vector. */
        if (!(new_cast = hlsl_new_cast(ctx, cast->operands[0].node, dst_vector_type, &cast->node.loc)))
            return false;
        list_add_after(&cast->node.entry, &new_cast->node.entry);
        if (!(swizzle = hlsl_new_swizzle(ctx, HLSL_SWIZZLE(X, Y, Z, W), dst_type->dimx, &new_cast->node, &cast->node.loc)))
            return false;
        list_add_after(&new_cast->node.entry, &swizzle->node.entry);

        hlsl_replace_node(&cast->node, &swizzle->node);
        return true;
    }

    return false;
}

/* Lower samples from separate texture and sampler variables to samples from
 * synthesized combined samplers. That is, translate SM4-style samples in the
 * source to SM1-style samples in the bytecode. */
static bool lower_separate_samples(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_resource_load *sample;
    struct vkd3d_string_buffer *name;
    struct hlsl_ir_var *var;

    if (instr->type != HLSL_IR_RESOURCE_LOAD)
        return false;
    sample = hlsl_ir_resource_load(instr);
    if (sample->load_type != HLSL_RESOURCE_SAMPLE || !sample->sampler.var)
        return false;

    if (!(name = hlsl_get_string_buffer(ctx)))
        return false;
    vkd3d_string_buffer_printf(name, "%s+%s", sample->sampler.var->name, sample->resource.var->name);

    TRACE("Lowering to combined sampler %s.\n", debugstr_a(name->buffer));

    if (!(var = hlsl_get_var(ctx->globals, name->buffer)))
    {
        struct hlsl_type *data_type = hlsl_new_texture_type(ctx, sample->resource.var->data_type->sampler_dim,
                ctx->builtin_types.vector[HLSL_TYPE_FLOAT][4 - 1]);

        if (!(var = hlsl_new_synthetic_var(ctx, name->buffer, data_type, instr->loc)))
        {
            hlsl_release_string_buffer(ctx, name);
            return false;
        }
        list_add_tail(&ctx->globals->vars, &var->scope_entry);
    }
    hlsl_release_string_buffer(ctx, name);

    sample->resource.var = var;
    hlsl_src_remove(&sample->resource.offset);
    sample->sampler.var = NULL;
    hlsl_src_remove(&sample->sampler.offset);
    return true;
}

/* Lower samples from separate texture and sampler variables to samples from
 * synthesized combined samplers. That is, translate SM4-style samples in the
 * source to SM1-style samples in the bytecode. */
static bool lower_combined_samples(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_resource_load *sample;
    struct vkd3d_string_buffer *name;
    struct hlsl_ir_var *var;

    if (instr->type != HLSL_IR_RESOURCE_LOAD)
        return false;
    sample = hlsl_ir_resource_load(instr);
    if (sample->load_type != HLSL_RESOURCE_SAMPLE || sample->sampler.var)
        return false;

    if (!(name = hlsl_get_string_buffer(ctx)))
        return false;
    vkd3d_string_buffer_printf(name, "<resource>%s", sample->resource.var->name);

    TRACE("Lowering to separate resource %s.\n", debugstr_a(name->buffer));

    if (!(var = hlsl_get_var(ctx->globals, name->buffer)))
    {
        struct hlsl_type *data_type = hlsl_new_texture_type(ctx, sample->resource.var->data_type->sampler_dim,
                ctx->builtin_types.vector[HLSL_TYPE_FLOAT][4 - 1]);

        if (!(var = hlsl_new_synthetic_var(ctx, name->buffer, data_type, instr->loc)))
        {
            hlsl_release_string_buffer(ctx, name);
            return false;
        }
    }
    hlsl_release_string_buffer(ctx, name);

    sample->sampler.var = sample->resource.var;
    hlsl_src_from_node(&sample->sampler.offset, sample->resource.offset.node);
    sample->resource.var = var;
    hlsl_src_remove(&sample->resource.offset);
    return true;
}

/* Given a sequence like:
 *
 *     2: float4 | load(x)
 *     4: float3 | @2.zyz
 *
 * where the swizzle of @3 is narrower than @2, convert this to a new sequence
 *
 *     2: float4 | load(x)
 *     3: float2 | load(x[1])
 *     4: float3 | @3.yxy
 *
 * (keeping around the old load for other instructions that might refer to it.)
 *
 * If the resulting swizzle is identity, we can of course then drop it, which
 * saves us a mov instruction. (We should probably be lowering such swizzles
 * at the assembly level anyway, though.)
 *
 * A secondary benefit is that we can potentially copy-prop the swizzled portion
 * even when we can't copy-prop the whole variable. This pattern notably appears
 * in the pattern for UAV stores—we add an implicit 0 miplevel on the assumption
 * that an instruction is an array load, and then immediately remove it when we
 * find it on the LHS. We could potentially generate better code there, but this
 * is easier, and potentially useful elsewhere.
 */
static bool split_swizzled_load(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_type *src_load_type, *dst_load_type, *swizzle_type;
    struct hlsl_ir_swizzle *swizzle, *new_swizzle;
    unsigned int min_component = HLSL_SWIZZLE_W;
    unsigned int max_component = HLSL_SWIZZLE_X;
    struct hlsl_ir_load *src_load, *dst_load;
    unsigned int i, s;

    if (instr->type != HLSL_IR_SWIZZLE)
        return false;
    swizzle = hlsl_ir_swizzle(instr);
    swizzle_type = swizzle->node.data_type;

    if (swizzle->val.node->type != HLSL_IR_LOAD)
        return false;
    src_load = hlsl_ir_load(swizzle->val.node);
    src_load_type = src_load->node.data_type;

    for (i = 0; i < swizzle_type->dimx; ++i)
    {
        unsigned int component = hlsl_swizzle_get_component(swizzle->swizzle, i);

        min_component = min(min_component, component);
        max_component = max(max_component, component);
    }
    assert(max_component >= min_component);

    if (min_component == 0 && max_component == src_load_type->dimx - 1)
        return false;

    dst_load_type = hlsl_get_vector_type(ctx, src_load_type->base_type, max_component - min_component + 1);

    if (!(dst_load = split_load(ctx, src_load, min_component, dst_load_type)))
        return false;

    s = 0;
    for (i = 0; i < swizzle_type->dimx; ++i)
        s |= (hlsl_swizzle_get_component(swizzle->swizzle, i) - min_component) << HLSL_SWIZZLE_SHIFT(i);

    if (!(new_swizzle = hlsl_new_swizzle(ctx, s, swizzle_type->dimx, &dst_load->node, &swizzle->node.loc)))
        return false;
    list_add_after(&swizzle->node.entry, &new_swizzle->node.entry);

    hlsl_replace_node(&swizzle->node, &new_swizzle->node);
    return true;
}

static bool remove_trivial_swizzles(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_ir_swizzle *swizzle;
    unsigned int i;

    if (instr->type != HLSL_IR_SWIZZLE)
        return false;
    swizzle = hlsl_ir_swizzle(instr);

    if (instr->data_type->dimx != swizzle->val.node->data_type->dimx)
        return false;

    for (i = 0; i < instr->data_type->dimx; ++i)
        if (((swizzle->swizzle >> (2 * i)) & 3) != i)
            return false;

    hlsl_replace_node(instr, swizzle->val.node);

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
    if (expr->op != HLSL_OP2_DIV)
        return false;

    if (!(rcp = hlsl_new_unary_expr(ctx, HLSL_OP1_RCP, expr->operands[1].node, instr->loc)))
        return false;
    list_add_before(&expr->node.entry, &rcp->entry);
    expr->op = HLSL_OP2_MUL;
    hlsl_src_remove(&expr->operands[1]);
    hlsl_src_from_node(&expr->operands[1], rcp);
    return true;
}

static bool lower_casts_to_bool(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_type *type = instr->data_type, *arg_type;
    struct hlsl_ir_constant *zero;
    struct hlsl_ir_expr *expr;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    expr = hlsl_ir_expr(instr);
    if (expr->op != HLSL_OP1_CAST)
        return false;
    arg_type = expr->operands[0].node->data_type;
    if (type->type > HLSL_CLASS_VECTOR || arg_type->type > HLSL_CLASS_VECTOR)
        return false;
    if (type->base_type != HLSL_TYPE_BOOL)
        return false;

    /* Narrowing casts should have already been lowered. */
    assert(type->dimx == arg_type->dimx);

    zero = hlsl_new_constant(ctx, arg_type, &instr->loc);
    if (!zero)
        return false;
    list_add_before(&instr->entry, &zero->node.entry);

    expr->op = HLSL_OP2_NEQUAL;
    hlsl_src_from_node(&expr->operands[1], &zero->node);

    return true;
}

static bool lower_int_abs(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    struct hlsl_type *type = instr->data_type;
    struct hlsl_ir_node *arg, *neg;
    struct hlsl_ir_expr *expr;

    if (instr->type != HLSL_IR_EXPR)
        return false;
    expr = hlsl_ir_expr(instr);

    if (expr->op != HLSL_OP1_ABS)
        return false;
    if (type->type != HLSL_CLASS_SCALAR && type->type != HLSL_CLASS_VECTOR)
        return false;
    if (type->base_type != HLSL_TYPE_INT)
        return false;

    arg = expr->operands[0].node;

    if (!(neg = hlsl_new_unary_expr(ctx, HLSL_OP1_NEG, arg, instr->loc)))
        return false;
    list_add_before(&instr->entry, &neg->entry);

    expr->op = HLSL_OP2_MAX;
    hlsl_src_from_node(&expr->operands[1], neg);

    return true;
}

static bool dce(struct hlsl_ctx *ctx, struct hlsl_ir_node *instr, void *context)
{
    switch (instr->type)
    {
        case HLSL_IR_CONSTANT:
        case HLSL_IR_EXPR:
        case HLSL_IR_LOAD:
        case HLSL_IR_RESOURCE_LOAD:
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

        case HLSL_IR_CALL:
        case HLSL_IR_IF:
        case HLSL_IR_JUMP:
        case HLSL_IR_LOOP:
        case HLSL_IR_RESOURCE_STORE:
            break;
    }

    return false;
}

/* Allocate a unique, ordered index to each instruction, which will be used for
 * computing liveness ranges. */
static unsigned int index_instructions(struct hlsl_block *block, unsigned int index)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, &block->instrs, struct hlsl_ir_node, entry)
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

    if (func->has_body)
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
static void compute_liveness_recurse(struct hlsl_block *block, unsigned int loop_first, unsigned int loop_last)
{
    struct hlsl_ir_node *instr;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(instr, &block->instrs, struct hlsl_ir_node, entry)
    {
        const unsigned int var_last_read = loop_last ? max(instr->index, loop_last) : instr->index;

        switch (instr->type)
        {
        case HLSL_IR_CALL:
            /* We should have inlined all calls before computing liveness. */
            assert(0);
            break;

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
            var->last_read = max(var->last_read, var_last_read);
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
        case HLSL_IR_RESOURCE_LOAD:
        {
            struct hlsl_ir_resource_load *load = hlsl_ir_resource_load(instr);

            var = load->resource.var;
            var->last_read = max(var->last_read, var_last_read);
            if (load->resource.offset.node)
                load->resource.offset.node->last_read = instr->index;

            if ((var = load->sampler.var))
            {
                var->last_read = max(var->last_read, var_last_read);
                if (load->sampler.offset.node)
                    load->sampler.offset.node->last_read = instr->index;
            }

            load->coords.node->last_read = instr->index;
            if (load->texel_offset.node)
                load->texel_offset.node->last_read = instr->index;
            if (load->lod.node)
                load->lod.node->last_read = instr->index;
            break;
        }
        case HLSL_IR_RESOURCE_STORE:
        {
            struct hlsl_ir_resource_store *store = hlsl_ir_resource_store(instr);

            var = store->resource.var;
            var->last_read = max(var->last_read, var_last_read);
            if (store->resource.offset.node)
                store->resource.offset.node->last_read = instr->index;
            store->coords.node->last_read = instr->index;
            store->value.node->last_read = instr->index;
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
    index_instructions(&entry_func->body, 2);

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

    compute_liveness_recurse(&entry_func->body, 0, 0);
}

struct liveness
{
    size_t size;
    uint32_t reg_count;
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
    liveness->reg_count = max(liveness->reg_count, ret.id + 1);
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
        unsigned int first_write, unsigned int last_read, unsigned int component_count)
{
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
    liveness->reg_count = max(liveness->reg_count, ret.id + align(component_count, 4));
    return ret;
}

static const char *debug_register(char class, struct hlsl_reg reg, const struct hlsl_type *type)
{
    static const char writemask_offset[] = {'w','x','y','z'};

    if (type->reg_size > 4)
    {
        if (type->reg_size & 3)
            return vkd3d_dbg_sprintf("%c%u-%c%u.%c", class, reg.id, class,
                    reg.id + (type->reg_size / 4), writemask_offset[type->reg_size & 3]);

        return vkd3d_dbg_sprintf("%c%u-%c%u", class, reg.id, class,
                reg.id + (type->reg_size / 4) - 1);
    }
    return vkd3d_dbg_sprintf("%c%u%s", class, reg.id, debug_hlsl_writemask(reg.writemask));
}

static void allocate_variable_temp_register(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, struct liveness *liveness)
{
    if (var->is_input_semantic || var->is_output_semantic || var->is_uniform)
        return;

    if (!var->reg.allocated && var->last_read)
    {
        if (var->data_type->reg_size > 4)
            var->reg = allocate_range(ctx, liveness, var->first_write,
                    var->last_read, var->data_type->reg_size);
        else
            var->reg = allocate_register(ctx, liveness, var->first_write,
                    var->last_read, hlsl_type_component_count(var->data_type));
        TRACE("Allocated %s to %s (liveness %u-%u).\n", var->name,
                debug_register('r', var->reg, var->data_type), var->first_write, var->last_read);
    }
}

static void allocate_temp_registers_recurse(struct hlsl_ctx *ctx, struct hlsl_block *block, struct liveness *liveness)
{
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, &block->instrs, struct hlsl_ir_node, entry)
    {
        if (!instr->reg.allocated && instr->last_read)
        {
            if (instr->data_type->reg_size > 4)
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

static void allocate_const_registers_recurse(struct hlsl_ctx *ctx, struct hlsl_block *block, struct liveness *liveness)
{
    struct hlsl_constant_defs *defs = &ctx->constant_defs;
    struct hlsl_ir_node *instr;

    LIST_FOR_EACH_ENTRY(instr, &block->instrs, struct hlsl_ir_node, entry)
    {
        switch (instr->type)
        {
            case HLSL_IR_CONSTANT:
            {
                struct hlsl_ir_constant *constant = hlsl_ir_constant(instr);
                const struct hlsl_type *type = instr->data_type;
                unsigned int x, y, i, writemask, end_reg;
                unsigned int reg_size = type->reg_size;

                if (reg_size > 4)
                    constant->reg = allocate_range(ctx, liveness, 1, UINT_MAX, reg_size);
                else
                    constant->reg = allocate_register(ctx, liveness, 1, UINT_MAX, type->dimx);
                TRACE("Allocated constant @%u to %s.\n", instr->index, debug_register('c', constant->reg, type));

                if (!hlsl_array_reserve(ctx, (void **)&defs->values, &defs->size,
                        constant->reg.id + reg_size / 4, sizeof(*defs->values)))
                    return;
                end_reg = constant->reg.id + reg_size / 4;
                if (end_reg > defs->count)
                {
                    memset(&defs->values[defs->count], 0, sizeof(*defs->values) * (end_reg - defs->count));
                    defs->count = end_reg;
                }

                assert(type->type <= HLSL_CLASS_LAST_NUMERIC);

                if (!(writemask = constant->reg.writemask))
                    writemask = (1u << type->dimx) - 1;

                for (y = 0; y < type->dimy; ++y)
                {
                    for (x = 0, i = 0; x < 4; ++x)
                    {
                        const union hlsl_constant_value *value;
                        float f;

                        if (!(writemask & (1u << x)))
                            continue;
                        value = &constant->value[i++];

                        switch (type->base_type)
                        {
                            case HLSL_TYPE_BOOL:
                                f = !!value->u;
                                break;

                            case HLSL_TYPE_FLOAT:
                            case HLSL_TYPE_HALF:
                                f = value->f;
                                break;

                            case HLSL_TYPE_INT:
                                f = value->i;
                                break;

                            case HLSL_TYPE_UINT:
                                f = value->u;
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

    allocate_const_registers_recurse(ctx, &entry_func->body, &liveness);

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_uniform && var->last_read)
        {
            if (var->data_type->reg_size > 4)
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
    allocate_temp_registers_recurse(ctx, &entry_func->body, &liveness);
    ctx->temp_count = liveness.reg_count;
    vkd3d_free(liveness.regs);
}

static void allocate_semantic_register(struct hlsl_ctx *ctx, struct hlsl_ir_var *var, unsigned int *counter, bool output)
{
    static const char *shader_names[] =
    {
        [VKD3D_SHADER_TYPE_PIXEL] = "Pixel",
        [VKD3D_SHADER_TYPE_VERTEX] = "Vertex",
        [VKD3D_SHADER_TYPE_GEOMETRY] = "Geometry",
        [VKD3D_SHADER_TYPE_HULL] = "Hull",
        [VKD3D_SHADER_TYPE_DOMAIN] = "Domain",
        [VKD3D_SHADER_TYPE_COMPUTE] = "Compute",
    };

    unsigned int type;
    uint32_t reg;
    bool builtin;

    assert(var->semantic.name);

    if (ctx->profile->major_version < 4)
    {
        D3DDECLUSAGE usage;
        uint32_t usage_idx;

        if (!hlsl_sm1_usage_from_semantic(&var->semantic, &usage, &usage_idx))
        {
            hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SEMANTIC,
                    "Invalid semantic '%s'.", var->semantic.name);
            return;
        }

        if ((!output && !var->last_read) || (output && !var->first_write))
            return;

        builtin = hlsl_sm1_register_from_semantic(ctx, &var->semantic, output, &type, &reg);
    }
    else
    {
        D3D_NAME usage;
        bool has_idx;

        if (!hlsl_sm4_usage_from_semantic(ctx, &var->semantic, output, &usage))
        {
            hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_SEMANTIC,
                    "Invalid semantic '%s'.", var->semantic.name);
            return;
        }
        if ((builtin = hlsl_sm4_register_from_semantic(ctx, &var->semantic, output, &type, NULL, &has_idx)))
            reg = has_idx ? var->semantic.index : 0;
    }

    if (builtin)
    {
        TRACE("%s %s semantic %s[%u] matches predefined register %#x[%u].\n", shader_names[ctx->profile->type],
                output ? "output" : "input", var->semantic.name, var->semantic.index, type, reg);
    }
    else
    {
        var->reg.allocated = true;
        var->reg.id = (*counter)++;
        var->reg.writemask = (1 << var->data_type->dimx) - 1;
        TRACE("Allocated %s to %s.\n", var->name, debug_register(output ? 'o' : 'v', var->reg, var->data_type));
    }
}

static void allocate_semantic_registers(struct hlsl_ctx *ctx)
{
    unsigned int input_counter = 0, output_counter = 0;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_input_semantic)
            allocate_semantic_register(ctx, var, &input_counter, false);
        if (var->is_output_semantic)
            allocate_semantic_register(ctx, var, &output_counter, true);
    }
}

static const struct hlsl_buffer *get_reserved_buffer(struct hlsl_ctx *ctx, uint32_t space, uint32_t index)
{
    const struct hlsl_buffer *buffer;

    LIST_FOR_EACH_ENTRY(buffer, &ctx->buffers, const struct hlsl_buffer, entry)
    {
        if (buffer->used_size && buffer->reservation.type == 'b'
                && buffer->reservation.space == space && buffer->reservation.index == index)
            return buffer;
    }
    return NULL;
}

static void calculate_buffer_offset(struct hlsl_ir_var *var)
{
    struct hlsl_buffer *buffer = var->buffer;

    buffer->size = hlsl_type_get_sm4_offset(var->data_type, buffer->size);

    var->buffer_offset = buffer->size;
    TRACE("Allocated buffer offset %u to %s.\n", var->buffer_offset, var->name);
    buffer->size += var->data_type->reg_size;
    if (var->last_read)
        buffer->used_size = buffer->size;
}

static void allocate_buffers(struct hlsl_ctx *ctx)
{
    struct hlsl_buffer *buffer;
    uint32_t index = 0, id = 0;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        if (var->is_uniform && var->data_type->type != HLSL_CLASS_OBJECT)
        {
            if (var->is_param)
                var->buffer = ctx->params_buffer;

            calculate_buffer_offset(var);
        }
    }

    LIST_FOR_EACH_ENTRY(buffer, &ctx->buffers, struct hlsl_buffer, entry)
    {
        if (!buffer->used_size)
            continue;

        if (buffer->type == HLSL_BUFFER_CONSTANT)
        {
            const struct hlsl_reg_reservation *reservation = &buffer->reservation;

            if (reservation->type == 'b')
            {
                const struct hlsl_buffer *reserved_buffer = get_reserved_buffer(
                        ctx, reservation->space, reservation->index);

                if (reserved_buffer && reserved_buffer != buffer)
                {
                    hlsl_error(ctx, &buffer->loc, VKD3D_SHADER_ERROR_HLSL_OVERLAPPING_RESERVATIONS,
                            "Multiple buffers bound to space %u, index %u.", reservation->space, reservation->index);
                    hlsl_note(ctx, &reserved_buffer->loc, VKD3D_SHADER_LOG_ERROR,
                            "Buffer %s is already bound to space %u, index %u.",
                            reserved_buffer->name, reservation->space, reservation->index);
                }

                buffer->reg.space = reservation->space;
                buffer->reg.index = reservation->index;
                if (shader_is_sm_5_1(ctx))
                    buffer->reg.id = id++;
                else
                    buffer->reg.id = reservation->index;
                buffer->reg.allocated = true;
                TRACE("Allocated reserved %s to space%u, cb%u.\n",
                        buffer->name, reservation->space, reservation->index);
            }
            else if (!reservation->type)
            {
                while (get_reserved_buffer(ctx, 0, index))
                    ++index;

                buffer->reg.space = 0;
                buffer->reg.index = index;
                if (shader_is_sm_5_1(ctx))
                    buffer->reg.id = id++;
                else
                    buffer->reg.id = index;
                buffer->reg.allocated = true;
                TRACE("Allocated %s to space0, cb%u.\n", buffer->name, index);
                ++index;
            }
            else
            {
                hlsl_error(ctx, &buffer->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_RESERVATION,
                        "Constant buffers must be allocated to register type 'b'.");
            }
        }
        else
        {
            FIXME("Allocate registers for texture buffers.\n");
        }
    }
}

static const struct hlsl_ir_var *get_reserved_object(struct hlsl_ctx *ctx, char type, uint32_t space, uint32_t index)
{
    const struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, const struct hlsl_ir_var, extern_entry)
    {
        if (var->last_read && var->reg_reservation.type == type
                && var->reg_reservation.space == space && var->reg_reservation.index == index)
            return var;
    }
    return NULL;
}

static const struct object_type_info
{
    enum hlsl_base_type type;
    char reg_name;
}
object_types[] =
{
    { HLSL_TYPE_SAMPLER, 's' },
    { HLSL_TYPE_TEXTURE, 't' },
};

static const struct object_type_info *get_object_type_info(enum hlsl_base_type type)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(object_types); ++i)
        if (type == object_types[i].type)
            return &object_types[i];

    WARN("No type info for object type %u.\n", type);
    return NULL;
}

static void allocate_objects(struct hlsl_ctx *ctx, enum hlsl_base_type type)
{
    const struct object_type_info *type_info = get_object_type_info(type);
    uint32_t index = 0, id = 0;
    struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        const struct hlsl_reg_reservation *reservation = &var->reg_reservation;

        if (!var->last_read || var->data_type->type != HLSL_CLASS_OBJECT
                || var->data_type->base_type != type)
            continue;

        if (reservation->type == type_info->reg_name)
        {
            const struct hlsl_ir_var *reserved_object = get_reserved_object(ctx,
                    type_info->reg_name, reservation->space, reservation->index);

            if (reserved_object && reserved_object != var)
            {
                hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_OVERLAPPING_RESERVATIONS,
                        "Multiple objects bound to %c%u, space %u.", type_info->reg_name,
                        reservation->index, reservation->space);
                hlsl_note(ctx, &reserved_object->loc, VKD3D_SHADER_LOG_ERROR,
                        "Object '%s' is already bound to %c%u, space %u.", reserved_object->name,
                        type_info->reg_name, reservation->index, reservation->space);
            }

            var->reg.space = reservation->space;
            var->reg.index = reservation->index;
            if (shader_is_sm_5_1(ctx))
                var->reg.id = id++;
            else
                var->reg.id = reservation->index;
            var->reg.allocated = true;
            TRACE("Allocated reserved %s to %c%u, space %u, id %u.\n", var->name,
                    type_info->reg_name, var->reg.index, var->reg.space, var->reg.id);
        }
        else if (!var->reg_reservation.type)
        {
            while (get_reserved_object(ctx, type_info->reg_name, 0, index))
                ++index;

            var->reg.space = 0;
            var->reg.index = index;
            if (shader_is_sm_5_1(ctx))
                var->reg.id = id++;
            else
                var->reg.id = index;
            var->reg.allocated = true;
            TRACE("Allocated %s to %c%u, space 0, id %u.\n", var->name,
                    type_info->reg_name, var->reg.index, var->reg.id);
            ++index;
        }
        else
        {
            struct vkd3d_string_buffer *type_string;

            type_string = hlsl_type_to_string(ctx, var->data_type);
            hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_RESERVATION,
                    "Object of type '%s' must be bound to register type '%c'.",
                    type_string->buffer, type_info->reg_name);
            hlsl_release_string_buffer(ctx, type_string);
        }
    }
}

static const struct hlsl_ir_var *get_reserved_uav(struct hlsl_ctx *ctx, uint32_t space, uint32_t index)
{
    const struct hlsl_ir_var *var;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, const struct hlsl_ir_var, extern_entry)
    {
        if (var->last_read && var->reg_reservation.type == 'u'
                && var->reg_reservation.space == space && var->reg_reservation.index == index)
            return var;
    }
    return NULL;
}

static void allocate_uavs(struct hlsl_ctx *ctx)
{
    uint32_t rtv_count = 0, index, id = 0;
    struct hlsl_ir_var *var;

    /* UAVs in pixel shaders occupy the same namespace as RTVs before shader
     * model 5.1. */
    if (ctx->profile->type == VKD3D_SHADER_TYPE_PIXEL
            && ctx->profile->major_version >= 4 && !shader_is_sm_5_1(ctx))
    {
        LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
        {
            D3D_NAME usage;

            if (!var->is_output_semantic)
                continue;

            if (hlsl_sm4_usage_from_semantic(ctx, &var->semantic, true, &usage) && usage == D3D_NAME_TARGET)
                rtv_count = max(rtv_count, var->semantic.index + 1);
        }
    }
    index = rtv_count;

    LIST_FOR_EACH_ENTRY(var, &ctx->extern_vars, struct hlsl_ir_var, extern_entry)
    {
        const struct hlsl_reg_reservation *reservation = &var->reg_reservation;

        if (!var->last_read || var->data_type->type != HLSL_CLASS_OBJECT
                || var->data_type->base_type != HLSL_TYPE_UAV)
            continue;

        if (var->reg_reservation.type == 'u')
        {
            const struct hlsl_ir_var *reserved_uav = get_reserved_uav(ctx, reservation->space, reservation->index);

            if (reserved_uav && reserved_uav != var)
            {
                hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_OVERLAPPING_RESERVATIONS,
                        "Multiple UAVs bound to u%u, space %u.", reservation->index, reservation->space);
                hlsl_note(ctx, &reserved_uav->loc, VKD3D_SHADER_LOG_ERROR,
                        "UAV '%s' is already bound to u%u, space %u.",
                        reserved_uav->name, reservation->index, reservation->space);
            }

            if (reservation->index < rtv_count)
                hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_OVERLAPPING_RESERVATIONS,
                        "UAV '%s' is bound to u%u, but %u RTVs are currently in use.",
                        var->name, var->reg_reservation.index, rtv_count);

            var->reg.space = reservation->space;
            var->reg.index = reservation->index;
            if (shader_is_sm_5_1(ctx))
                var->reg.id = id++;
            else
                var->reg.id = var->reg.index;
            var->reg.allocated = true;
            TRACE("Allocated reserved %s to u%u, space %u, id %u.\n", var->name,
                    var->reg.index, var->reg.space, var->reg.id);
        }
        else if (!reservation->type)
        {
            while (get_reserved_uav(ctx, 0, index))
                ++index;

            var->reg.space = 0;
            var->reg.index = index;
            if (shader_is_sm_5_1(ctx))
                var->reg.id = id++;
            else
                var->reg.id = var->reg.index;
            var->reg.allocated = true;
            TRACE("Allocated %s to u%u, space 0, id %u.\n", var->name, var->reg.index, var->reg.id);
            ++index;
        }
        else
        {
            hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_RESERVATION,
                    "UAVs must be bound to register type 'u'.");
        }
    }
}

bool hlsl_offset_from_deref(struct hlsl_ctx *ctx, const struct hlsl_deref *deref, unsigned int *offset)
{
    struct hlsl_ir_node *offset_node = deref->offset.node;

    if (!offset_node)
    {
        *offset = 0;
        return true;
    }

    /* We should always have generated a cast to UINT. */
    assert(offset_node->data_type->type == HLSL_CLASS_SCALAR
            && offset_node->data_type->base_type == HLSL_TYPE_UINT);

    if (offset_node->type != HLSL_IR_CONSTANT)
        return false;

    *offset = hlsl_ir_constant(offset_node)->value[0].u;

    if (*offset >= deref->var->data_type->reg_size)
    {
        hlsl_error(ctx, &deref->offset.node->loc, VKD3D_SHADER_ERROR_HLSL_OFFSET_OUT_OF_BOUNDS,
                "Dereference is out of bounds. %u/%u", *offset, deref->var->data_type->reg_size);
        return false;
    }

    return true;
}

unsigned int hlsl_offset_from_deref_safe(struct hlsl_ctx *ctx, const struct hlsl_deref *deref)
{
    unsigned int offset;

    if (hlsl_offset_from_deref(ctx, deref, &offset))
        return offset;

    hlsl_fixme(ctx, &deref->offset.node->loc, "Dereference with non-constant offset of type %s.",
            hlsl_node_type_to_string(deref->offset.node->type));

    return 0;
}

struct hlsl_reg hlsl_reg_from_deref(struct hlsl_ctx *ctx, const struct hlsl_deref *deref)
{
    const struct hlsl_ir_var *var = deref->var;
    struct hlsl_reg ret = var->reg;
    unsigned int offset = hlsl_offset_from_deref_safe(ctx, deref);

    ret.id += offset / 4;

    ret.writemask = 0xf & (0xf << (offset % 4));
    if (var->reg.writemask)
        ret.writemask = hlsl_combine_writemasks(var->reg.writemask, ret.writemask);

    return ret;
}

static void parse_numthreads_attribute(struct hlsl_ctx *ctx, const struct hlsl_attribute *attr)
{
    unsigned int i;

    ctx->found_numthreads = 1;

    if (attr->args_count != 3)
    {
        hlsl_error(ctx, &attr->loc, VKD3D_SHADER_ERROR_HLSL_WRONG_PARAMETER_COUNT,
                "Expected 3 parameters for [numthreads] attribute, but got %u.", attr->args_count);
        return;
    }

    for (i = 0; i < attr->args_count; ++i)
    {
        const struct hlsl_ir_node *instr = attr->args[i].node;
        const struct hlsl_type *type = instr->data_type;

        if (type->type != HLSL_CLASS_SCALAR
                || (type->base_type != HLSL_TYPE_INT && type->base_type != HLSL_TYPE_UINT))
        {
            struct vkd3d_string_buffer *string;

            if ((string = hlsl_type_to_string(ctx, type)))
                hlsl_error(ctx, &instr->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_TYPE,
                        "Wrong type for argument %u of [numthreads]: expected int or uint, but got %s.",
                        i, string->buffer);
            hlsl_release_string_buffer(ctx, string);
            break;
        }

        if (instr->type != HLSL_IR_CONSTANT)
        {
            hlsl_fixme(ctx, &instr->loc, "Non-constant expression in [numthreads] initializer.");
            break;
        }

        if (!(ctx->thread_count[i] = hlsl_ir_constant(instr)->value[0].u))
            hlsl_error(ctx, &instr->loc, VKD3D_SHADER_ERROR_HLSL_INVALID_THREAD_COUNT,
                    "Thread count must be a positive integer.");
    }
}

int hlsl_emit_bytecode(struct hlsl_ctx *ctx, struct hlsl_ir_function_decl *entry_func,
        enum vkd3d_shader_target_type target_type, struct vkd3d_shader_code *out)
{
    const struct hlsl_profile_info *profile = ctx->profile;
    struct hlsl_block *const body = &entry_func->body;
    struct recursive_call_ctx recursive_call_ctx;
    struct hlsl_ir_var *var;
    unsigned int i;
    bool progress;

    list_move_head(&body->instrs, &ctx->static_initializers);

    memset(&recursive_call_ctx, 0, sizeof(recursive_call_ctx));
    transform_ir(ctx, find_recursive_calls, body, &recursive_call_ctx);
    vkd3d_free(recursive_call_ctx.backtrace);

    /* Avoid going into an infinite loop when expanding them. */
    if (ctx->result)
        return ctx->result;

    lower_return(ctx, entry_func, body, false);

    while (transform_ir(ctx, lower_calls, body, NULL));

    if (profile->major_version < 4)
        transform_ir(ctx, lower_separate_samples, body, NULL);
    else
        transform_ir(ctx, lower_combined_samples, body, NULL);

    LIST_FOR_EACH_ENTRY(var, &ctx->globals->vars, struct hlsl_ir_var, scope_entry)
    {
        if (var->modifiers & HLSL_STORAGE_UNIFORM)
            prepend_uniform_copy(ctx, &body->instrs, var);
    }

    LIST_FOR_EACH_ENTRY(var, entry_func->parameters, struct hlsl_ir_var, param_entry)
    {
        if (var->data_type->type == HLSL_CLASS_OBJECT || (var->modifiers & HLSL_STORAGE_UNIFORM))
        {
            prepend_uniform_copy(ctx, &body->instrs, var);
        }
        else
        {
            if (var->data_type->type != HLSL_CLASS_STRUCT && !var->semantic.name)
                hlsl_error(ctx, &var->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                        "Parameter \"%s\" is missing a semantic.", var->name);

            if (var->modifiers & HLSL_STORAGE_IN)
                prepend_input_var_copy(ctx, &body->instrs, var);
            if (var->modifiers & HLSL_STORAGE_OUT)
                append_output_var_copy(ctx, &body->instrs, var);
        }
    }
    if (entry_func->return_var)
    {
        if (entry_func->return_var->data_type->type != HLSL_CLASS_STRUCT && !entry_func->return_var->semantic.name)
            hlsl_error(ctx, &entry_func->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_SEMANTIC,
                    "Entry point \"%s\" is missing a return value semantic.", entry_func->func->name);

        append_output_var_copy(ctx, &body->instrs, entry_func->return_var);
    }

    for (i = 0; i < entry_func->attr_count; ++i)
    {
        const struct hlsl_attribute *attr = entry_func->attrs[i];

        if (!strcmp(attr->name, "numthreads") && profile->type == VKD3D_SHADER_TYPE_COMPUTE)
            parse_numthreads_attribute(ctx, attr);
        else
            hlsl_warning(ctx, &entry_func->attrs[i]->loc, VKD3D_SHADER_WARNING_HLSL_UNKNOWN_ATTRIBUTE,
                    "Ignoring unknown attribute \"%s\".", entry_func->attrs[i]->name);
    }

    if (profile->type == VKD3D_SHADER_TYPE_COMPUTE && !ctx->found_numthreads)
        hlsl_error(ctx, &entry_func->loc, VKD3D_SHADER_ERROR_HLSL_MISSING_ATTRIBUTE,
                "Entry point \"%s\" is missing a [numthreads] attribute.", entry_func->func->name);

    transform_ir(ctx, lower_broadcasts, body, NULL);
    while (transform_ir(ctx, fold_redundant_casts, body, NULL));
    do
    {
        progress = transform_ir(ctx, split_array_copies, body, NULL);
        progress |= transform_ir(ctx, split_struct_copies, body, NULL);
    }
    while (progress);
    transform_ir(ctx, split_matrix_copies, body, NULL);
    transform_ir(ctx, lower_narrowing_casts, body, NULL);
    transform_ir(ctx, lower_casts_to_bool, body, NULL);
    transform_ir(ctx, lower_int_abs, body, NULL);
    do
    {
        progress = transform_ir(ctx, hlsl_fold_constant_exprs, body, NULL);
        progress |= transform_ir(ctx, hlsl_fold_constant_swizzles, body, NULL);
        progress |= copy_propagation_execute(ctx, body);
        progress |= transform_ir(ctx, split_swizzled_load, body, NULL);
        progress |= transform_ir(ctx, remove_trivial_swizzles, body, NULL);
    }
    while (progress);

    if (profile->major_version < 4)
        transform_ir(ctx, lower_division, body, NULL);

    do
        compute_liveness(ctx, entry_func);
    while (transform_ir(ctx, dce, body, NULL));

    compute_liveness(ctx, entry_func);

    if (TRACE_ON())
        rb_for_each_entry(&ctx->functions, dump_function, ctx);

    allocate_temp_registers(ctx, entry_func);
    allocate_semantic_registers(ctx);
    if (profile->major_version < 4)
    {
        allocate_const_registers(ctx, entry_func);
    }
    else
    {
        allocate_buffers(ctx);
        allocate_objects(ctx, HLSL_TYPE_TEXTURE);
        allocate_uavs(ctx);
    }
    allocate_semantic_registers(ctx);
    allocate_objects(ctx, HLSL_TYPE_SAMPLER);

    if (ctx->result)
        return ctx->result;

    switch (target_type)
    {
        case VKD3D_SHADER_TARGET_D3D_BYTECODE:
            return hlsl_sm1_write(ctx, entry_func, out);

        case VKD3D_SHADER_TARGET_DXBC_TPF:
            return hlsl_sm4_write(ctx, entry_func, out);

        default:
            ERR("Unsupported shader target type %#x.\n", target_type);
            return VKD3D_ERROR_INVALID_ARGUMENT;
    }
}
