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

%{

#include "preproc.tab.h"

#define YYSTYPE PREPROC_YYSTYPE
#define YYLTYPE PREPROC_YYLTYPE

#define YY_DECL static int preproc_lexer_lex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t yyscanner)

static void update_location(struct preproc_ctx *ctx);

#define YY_USER_ACTION update_location(yyget_extra(yyscanner));

%}

%option 8bit
%option bison-bridge
%option bison-locations
%option extra-type="struct preproc_ctx *"
%option never-interactive
%option noinput
%option nounput
%option noyy_top_state
%option noyywrap
%option prefix="preproc_yy"
%option reentrant
%option stack

    /* Because these can both be terminated by EOF, we need states for them. */
%s C_COMMENT
%s CXX_COMMENT

%s INCLUDE

NEWLINE         \r?\n
WS              [ \t]
IDENTIFIER      [A-Za-z_][A-Za-z0-9_]*

%%

<INITIAL>"//"                       {yy_push_state(CXX_COMMENT, yyscanner);}
<INITIAL>"/*"                       {yy_push_state(C_COMMENT, yyscanner);}
<CXX_COMMENT>\\{NEWLINE}            {}
<CXX_COMMENT>\n                     {
        yy_pop_state(yyscanner);
        BEGIN(INITIAL);
        return T_NEWLINE;
    }
<C_COMMENT>"*/"                     {yy_pop_state(yyscanner);}
<C_COMMENT,CXX_COMMENT><<EOF>>      {yy_pop_state(yyscanner);}
<C_COMMENT,CXX_COMMENT>.            {}

<INITIAL>{IDENTIFIER}               {return T_IDENTIFIER;}

    /* We have no use for floats, but shouldn't parse them as integers. */

<INITIAL>[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[hHfF]?    {return T_TEXT;}
<INITIAL>[0-9]+\.([eE][+-]?[0-9]+)?[hHfF]?          {return T_TEXT;}
<INITIAL>[0-9]+([eE][+-]?[0-9]+)?[hHfF]             {return T_TEXT;}
<INITIAL>[0-9]+[eE][+-]?[0-9]+                      {return T_TEXT;}
<INITIAL>0[xX][0-9a-fA-f]+[ul]{0,2}                 {return T_INTEGER;}
<INITIAL>0[0-7]*[ul]{0,2}                           {return T_INTEGER;}
<INITIAL>[1-9][0-9]*[ul]{0,2}                       {return T_INTEGER;}

<INITIAL>"&&"                       {return T_TEXT;}
<INITIAL>"||"                       {return T_TEXT;}
<INITIAL>"++"                       {return T_TEXT;}
<INITIAL>"--"                       {return T_TEXT;}
<INITIAL>"<<"=?                     {return T_TEXT;}
<INITIAL>">>"=?                     {return T_TEXT;}
<INITIAL>[-+*/%&|^=><!]=            {return T_TEXT;}

<INCLUDE>\"[^"]*\"                  {return T_STRING;}
<INCLUDE>\<[^>]*\>                  {return T_STRING;}

    /* C strings (including escaped quotes). */
<INITIAL>\"([^"\\]|\\.)*\"          {return T_TEXT;}

<INITIAL>#{WS}*{IDENTIFIER}         {
        struct preproc_ctx *ctx = yyget_extra(yyscanner);
        const char *p;

        if (!ctx->last_was_newline)
            return T_TEXT;

        for (p = yytext + 1; strchr(" \t", *p); ++p)
            ;

        if (!strcmp(p, "include"))
        {
            BEGIN(INCLUDE);
            return T_INCLUDE;
        }

        if (!strcmp(p, "define"))
            return T_DEFINE;
        if (!strcmp(p, "elif"))
            return T_ELIF;
        if (!strcmp(p, "else"))
            return T_ELSE;
        if (!strcmp(p, "endif"))
            return T_ENDIF;
        if (!strcmp(p, "if"))
            return T_IF;
        if (!strcmp(p, "ifdef"))
            return T_IFDEF;
        if (!strcmp(p, "ifndef"))
            return T_IFNDEF;

        preproc_warning(ctx, yyget_lloc(yyscanner), VKD3D_SHADER_WARNING_PP_UNKNOWN_DIRECTIVE,
                "Ignoring unknown directive \"%s\".", yytext);
        return T_TEXT;
    }

<INITIAL,INCLUDE>\\{NEWLINE}        {}
<INITIAL,INCLUDE>{NEWLINE}          {
        BEGIN(INITIAL);
        return T_NEWLINE;
    }

<INITIAL>{WS}+                      {}
<INITIAL>.                          {return T_TEXT;}

%%

static void update_location(struct preproc_ctx *ctx)
{
    struct preproc_buffer *buffer = &preproc_get_top_file(ctx)->buffer;
    unsigned int i, leng = yyget_leng(ctx->scanner);
    const char *text = yyget_text(ctx->scanner);

    /* We want to do this here, rather than before calling yylex(), because
     * some tokens are skipped by the lexer. */

    *yyget_lloc(ctx->scanner) = buffer->location;

    for (i = 0; i < leng; ++i)
    {
        ++buffer->location.column;
        if (text[i] == '\n')
        {
            buffer->location.column = 1;
            ++buffer->location.line;
        }
    }
}

static void preproc_pop_buffer(struct preproc_ctx *ctx)
{
    struct preproc_file *file = preproc_get_top_file(ctx);

    if (ctx->file_count > 1)
        preproc_close_include(ctx, &file->code);

    if (file->if_count)
    {
        const struct vkd3d_shader_location loc = {.source_name = file->filename};

        preproc_warning(ctx, &loc, VKD3D_SHADER_WARNING_PP_UNTERMINATED_IF, "Unterminated #if block.");
    }
    vkd3d_free(file->if_stack);

    vkd3d_free(file->filename);

    yy_delete_buffer(file->buffer.lexer_buffer, ctx->scanner);

    --ctx->file_count;
    TRACE("File stack size is now %zu.\n", ctx->file_count);

    if (ctx->file_count)
        yy_switch_to_buffer(ctx->file_stack[ctx->file_count - 1].buffer.lexer_buffer, ctx->scanner);
}

static bool preproc_is_writing(struct preproc_ctx *ctx)
{
    const struct preproc_file *file = preproc_get_top_file(ctx);

    if (!file->if_count)
        return true;
    return file->if_stack[file->if_count - 1].current_true;
}

static int return_token(int token, YYSTYPE *lval, const char *text)
{
    switch (token)
    {
        case T_IDENTIFIER:
        case T_INTEGER:
        case T_STRING:
        case T_TEXT:
            if (!(lval->string = vkd3d_strdup(text)))
                return 0;
            break;
    }

    return token;
}

int yylex(YYSTYPE *lval, YYLTYPE *lloc, yyscan_t scanner)
{
    struct preproc_ctx *ctx = yyget_extra(scanner);

    for (;;)
    {
        const char *text;
        int token;

        if (ctx->last_was_eof)
        {
            preproc_pop_buffer(ctx);
            if (!ctx->file_count)
                return 0;
        }
        ctx->last_was_eof = false;

        assert(ctx->file_count);
        if (!(token = preproc_lexer_lex(lval, lloc, scanner)))
        {
            ctx->last_was_eof = true;
            token = T_NEWLINE;
            text = "\n";
        }
        else
        {
            text = yyget_text(scanner);
        }

        if (ctx->last_was_newline)
        {
            switch (token)
            {
                case T_DEFINE:
                case T_ELIF:
                case T_ELSE:
                case T_ENDIF:
                case T_IF:
                case T_IFDEF:
                case T_IFNDEF:
                case T_INCLUDE:
                    ctx->current_directive = token;
                    break;

                default:
                    ctx->current_directive = 0;
            }
        }

        ctx->last_was_newline = (token == T_NEWLINE);

        TRACE("Parsing token %d, line %d, in directive %d, string %s.\n",
                token, lloc->line, ctx->current_directive, debugstr_a(text));

        switch (ctx->current_directive)
        {
            case T_ELIF:
            case T_ELSE:
            case T_ENDIF:
            case T_IF:
            case T_IFDEF:
            case T_IFNDEF:
                break;

            default:
                if (!preproc_is_writing(ctx))
                    continue;
        }

        if (ctx->current_directive)
            return return_token(token, lval, text);

        vkd3d_string_buffer_printf(&ctx->buffer, "%s ", text);
    }
}

bool preproc_push_include(struct preproc_ctx *ctx, char *filename, const struct vkd3d_shader_code *code)
{
    struct preproc_file *file;

    if (!vkd3d_array_reserve((void **)&ctx->file_stack, &ctx->file_stack_size,
            ctx->file_count + 1, sizeof(*ctx->file_stack)))
        return false;
    file = &ctx->file_stack[ctx->file_count++];
    memset(file, 0, sizeof(*file));
    file->code = *code;
    file->filename = filename;
    file->buffer.lexer_buffer = yy_scan_bytes(code->code, code->size, ctx->scanner);
    file->buffer.location.source_name = file->filename;
    file->buffer.location.line = 1;
    file->buffer.location.column = 1;
    TRACE("File stack size is now %zu.\n", ctx->file_count);
    ctx->last_was_newline = true;
    return true;
}

static int preproc_macro_compare(const void *key, const struct rb_entry *entry)
{
    const struct preproc_macro *macro = RB_ENTRY_VALUE(entry, struct preproc_macro, entry);
    const char *name = key;

    return strcmp(name, macro->name);
}

static void preproc_macro_rb_free(struct rb_entry *entry, void *ctx)
{
    preproc_free_macro(RB_ENTRY_VALUE(entry, struct preproc_macro, entry));
}

int preproc_lexer_parse(const struct vkd3d_shader_compile_info *compile_info,
        struct vkd3d_shader_code *out, struct vkd3d_shader_message_context *message_context)
{
    static const struct vkd3d_shader_preprocess_info default_preprocess_info = {0};
    struct preproc_ctx ctx = {0};
    char *source_name;
    void *output_code;

    vkd3d_string_buffer_init(&ctx.buffer);
    rb_init(&ctx.macros, preproc_macro_compare);
    if (!(ctx.preprocess_info = vkd3d_find_struct(compile_info->next, PREPROCESS_INFO)))
        ctx.preprocess_info = &default_preprocess_info;
    ctx.message_context = message_context;

    if (!(source_name = vkd3d_strdup(compile_info->source_name ? compile_info->source_name : "<anonymous>")))
    {
        vkd3d_string_buffer_cleanup(&ctx.buffer);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    yylex_init_extra(&ctx, &ctx.scanner);
    if (!preproc_push_include(&ctx, source_name, &compile_info->source))
    {
        yylex_destroy(ctx.scanner);
        vkd3d_free(source_name);
        vkd3d_string_buffer_cleanup(&ctx.buffer);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    preproc_yyparse(ctx.scanner, &ctx);

    while (ctx.file_count)
        preproc_pop_buffer(&ctx);
    yylex_destroy(ctx.scanner);

    rb_destroy(&ctx.macros, preproc_macro_rb_free, NULL);
    vkd3d_free(ctx.file_stack);

    if (ctx.error)
    {
        WARN("Failed to preprocess.\n");
        vkd3d_string_buffer_cleanup(&ctx.buffer);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    if (!(output_code = vkd3d_malloc(ctx.buffer.content_size)))
    {
        vkd3d_string_buffer_cleanup(&ctx.buffer);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }
    memcpy(output_code, ctx.buffer.buffer, ctx.buffer.content_size);
    out->size = ctx.buffer.content_size;
    out->code = output_code;
    vkd3d_string_buffer_trace(&ctx.buffer);
    vkd3d_string_buffer_cleanup(&ctx.buffer);
    return VKD3D_OK;
}
