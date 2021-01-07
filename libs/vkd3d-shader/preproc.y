/*
 * HLSL preprocessor
 *
 * Copyright 2020 Zebediah Figura for CodeWeavers
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

#include "vkd3d_shader_private.h"
#include "preproc.h"

#define PREPROC_YYLTYPE struct vkd3d_shader_location

}

%code provides
{

int preproc_yylex(PREPROC_YYSTYPE *yylval_param, PREPROC_YYLTYPE *yylloc_param, void *scanner);

}

%code
{

#define YYLLOC_DEFAULT(cur, rhs, n) (cur) = YYRHSLOC(rhs, !!n)

static void preproc_error(struct preproc_ctx *ctx, const struct vkd3d_shader_location *loc,
        enum vkd3d_shader_error error, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    vkd3d_shader_verror(ctx->message_context, loc, error, format, args);
    va_end(args);
    ctx->error = true;
}

void preproc_warning(struct preproc_ctx *ctx, const struct vkd3d_shader_location *loc,
        enum vkd3d_shader_error error, const char *format, ...)
{
    va_list args;

    va_start(args, format);
    vkd3d_shader_vwarning(ctx->message_context, loc, error, format, args);
    va_end(args);
}

static void yyerror(const YYLTYPE *loc, void *scanner, struct preproc_ctx *ctx, const char *string)
{
    preproc_error(ctx, loc, VKD3D_SHADER_ERROR_PP_INVALID_SYNTAX, "%s", string);
}

static struct preproc_macro *preproc_find_macro(struct preproc_ctx *ctx, const char *name)
{
    struct rb_entry *entry;

    if ((entry = rb_get(&ctx->macros, name)))
        return RB_ENTRY_VALUE(entry, struct preproc_macro, entry);
    return NULL;
}

static bool preproc_add_macro(struct preproc_ctx *ctx, const struct vkd3d_shader_location *loc, char *name)
{
    struct preproc_macro *macro;
    int ret;

    if ((macro = preproc_find_macro(ctx, name)))
    {
        preproc_warning(ctx, loc, VKD3D_SHADER_WARNING_PP_ALREADY_DEFINED, "Redefinition of %s.", name);
        rb_remove(&ctx->macros, &macro->entry);
        preproc_free_macro(macro);
    }

    TRACE("Defining new macro %s.\n", debugstr_a(name));

    if (!(macro = vkd3d_malloc(sizeof(*macro))))
        return false;
    macro->name = name;
    ret = rb_put(&ctx->macros, name, &macro->entry);
    assert(!ret);
    return true;
}

void preproc_free_macro(struct preproc_macro *macro)
{
    vkd3d_free(macro->name);
    vkd3d_free(macro);
}

static bool preproc_was_writing(struct preproc_ctx *ctx)
{
    if (ctx->if_count < 2)
        return true;
    return ctx->if_stack[ctx->if_count - 2].current_true;
}

static bool preproc_push_if(struct preproc_ctx *ctx, bool condition)
{
    struct preproc_if_state *state;

    if (!vkd3d_array_reserve((void **)&ctx->if_stack, &ctx->if_stack_size, ctx->if_count + 1, sizeof(*ctx->if_stack)))
        return false;
    state = &ctx->if_stack[ctx->if_count++];
    state->current_true = condition && preproc_was_writing(ctx);
    state->seen_true = condition;
    state->seen_else = false;
    return true;
}

static int char_to_int(char c)
{
    if ('0' <= c && c <= '9')
        return c - '0';
    if ('A' <= c && c <= 'F')
        return c - 'A' + 10;
    if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    return -1;
}

static uint32_t preproc_parse_integer(const char *s)
{
    uint32_t base = 10, ret = 0;
    int digit;

    if (*s == '0')
    {
        base = 8;
        ++s;
        if (*s == 'x' || *s == 'X')
        {
            base = 16;
            ++s;
        }
    }

    while ((digit = char_to_int(*s++)) >= 0)
        ret = ret * base + (uint32_t)digit;
    return ret;
}

}

%define api.prefix {preproc_yy}
%define api.pure full
%define parse.error verbose
%expect 0
%locations
%lex-param {yyscan_t scanner}
%parse-param {void *scanner}
%parse-param {struct preproc_ctx *ctx}

%union
{
    char *string;
    uint32_t integer;
}

%token <string> T_IDENTIFIER
%token <string> T_INTEGER
%token <string> T_TEXT

%token T_NEWLINE

%token T_DEFINE "#define"
%token T_ELIF "#elif"
%token T_ELSE "#else"
%token T_ENDIF "#endif"
%token T_IF "#if"

%type <integer> expr
%type <string> body_token

%%

shader_text
    : %empty
    | shader_text directive
        {
            vkd3d_string_buffer_printf(&ctx->buffer, "\n");
        }

body_text
    : %empty
    | body_text body_token
        {
            vkd3d_free($2);
        }

body_token
    : T_IDENTIFIER
    | T_INTEGER
    | T_TEXT

directive
    : T_DEFINE T_IDENTIFIER body_text T_NEWLINE
        {
            if (!preproc_add_macro(ctx, &@$, $2))
                YYABORT;
        }
    | T_IF expr T_NEWLINE
        {
            if (!preproc_push_if(ctx, !!$2))
                YYABORT;
        }
    | T_ELIF expr T_NEWLINE
        {
            if (ctx->if_count)
            {
                struct preproc_if_state *state = &ctx->if_stack[ctx->if_count - 1];

                if (state->seen_else)
                {
                    preproc_warning(ctx, &@$, VKD3D_SHADER_WARNING_PP_INVALID_DIRECTIVE, "Ignoring #elif after #else.");
                }
                else
                {
                    state->current_true = $2 && !state->seen_true && preproc_was_writing(ctx);
                    state->seen_true = $2 || state->seen_true;
                }
            }
            else
            {
                preproc_warning(ctx, &@$, VKD3D_SHADER_WARNING_PP_INVALID_DIRECTIVE,
                        "Ignoring #elif without prior #if.");
            }
        }
    | T_ELSE T_NEWLINE
        {
            if (ctx->if_count)
            {
                struct preproc_if_state *state = &ctx->if_stack[ctx->if_count - 1];

                if (state->seen_else)
                {
                    preproc_warning(ctx, &@$, VKD3D_SHADER_WARNING_PP_INVALID_DIRECTIVE, "Ignoring #else after #else.");
                }
                else
                {
                    state->current_true = !state->seen_true && preproc_was_writing(ctx);
                    state->seen_else = true;
                }
            }
            else
            {
                preproc_warning(ctx, &@$, VKD3D_SHADER_WARNING_PP_INVALID_DIRECTIVE,
                        "Ignoring #else without prior #if.");
            }
        }
    | T_ENDIF T_NEWLINE
        {
            if (ctx->if_count)
                --ctx->if_count;
            else
                preproc_warning(ctx, &@$, VKD3D_SHADER_WARNING_PP_INVALID_DIRECTIVE,
                        "Ignoring #endif without prior #if.");
        }

expr
    : T_INTEGER
        {
            $$ = preproc_parse_integer($1);
            vkd3d_free($1);
        }
