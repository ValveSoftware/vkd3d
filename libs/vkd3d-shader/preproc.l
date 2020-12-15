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

WS              [ \t]
IDENTIFIER      [A-Za-z_][A-Za-z0-9_]*

%%

<INITIAL>"//"                       {yy_push_state(CXX_COMMENT, yyscanner);}
<INITIAL>"/*"                       {yy_push_state(C_COMMENT, yyscanner);}
<CXX_COMMENT>\\\r?\n                {}
<CXX_COMMENT>\n                     {
        yy_pop_state(yyscanner);
        return T_TEXT;
    }
<C_COMMENT>"*/"                     {yy_pop_state(yyscanner);}
<C_COMMENT,CXX_COMMENT><<EOF>>      {yy_pop_state(yyscanner);}
<C_COMMENT,CXX_COMMENT>.            {}

<INITIAL>{IDENTIFIER}               {return T_TEXT;}

<INITIAL>[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[hHfF]?    {return T_TEXT;}
<INITIAL>[0-9]+\.([eE][+-]?[0-9]+)?[hHfF]?          {return T_TEXT;}
<INITIAL>[0-9]+([eE][+-]?[0-9]+)?[hHfF]             {return T_TEXT;}
<INITIAL>[0-9]+[eE][+-]?[0-9]+                      {return T_TEXT;}
<INITIAL>0[xX][0-9a-fA-f]+[ul]{0,2}                 {return T_TEXT;}
<INITIAL>0[0-7]*[ul]{0,2}                           {return T_TEXT;}
<INITIAL>[1-9][0-9]*[ul]{0,2}                       {return T_TEXT;}

<INITIAL>"&&"                       {return T_TEXT;}
<INITIAL>"||"                       {return T_TEXT;}
<INITIAL>"++"                       {return T_TEXT;}
<INITIAL>"--"                       {return T_TEXT;}
<INITIAL>"<<"=?                     {return T_TEXT;}
<INITIAL>">>"=?                     {return T_TEXT;}
<INITIAL>[-+*/%&|^=><!]=            {return T_TEXT;}

    /* C strings (including escaped quotes). */
<INITIAL>\"([^"\\]|\\.)*\"          {return T_TEXT;}

<INITIAL>{WS}+                      {}
<INITIAL>.                          {return T_TEXT;}

%%

static void update_location(struct preproc_ctx *ctx)
{
    unsigned int i, leng = yyget_leng(ctx->scanner);
    const char *text = yyget_text(ctx->scanner);

    /* We want to do this here, rather than before calling yylex(), because
     * some tokens are skipped by the lexer. */

    *yyget_lloc(ctx->scanner) = ctx->location;

    for (i = 0; i < leng; ++i)
    {
        ++ctx->location.column;
        if (text[i] == '\n')
        {
            ctx->location.column = 1;
            ++ctx->location.line;
        }
    }
}

int yylex(YYSTYPE *lval, YYLTYPE *lloc, yyscan_t scanner)
{
    struct preproc_ctx *ctx = yyget_extra(scanner);

    for (;;)
    {
        const char *text;
        int token;

        if (!(token = preproc_lexer_lex(lval, lloc, scanner)))
            return 0;
        text = yyget_text(scanner);

        TRACE("Parsing token %d, line %d, string %s.\n", token, lloc->line, debugstr_a(text));

        vkd3d_string_buffer_printf(&ctx->buffer, "%s ", text);
    }
}

int preproc_lexer_parse(const struct vkd3d_shader_compile_info *compile_info,
        struct vkd3d_shader_code *out, struct vkd3d_shader_message_context *message_context)
{
    struct preproc_ctx ctx = {0};
    YY_BUFFER_STATE top_buffer;
    void *output_code;

    vkd3d_string_buffer_init(&ctx.buffer);
    ctx.message_context = message_context;
    ctx.location.source_name = compile_info->source_name;
    ctx.location.line = 1;
    ctx.location.column = 1;

    yylex_init_extra(&ctx, &ctx.scanner);
    top_buffer = yy_scan_bytes(compile_info->source.code, compile_info->source.size, ctx.scanner);

    preproc_yyparse(ctx.scanner, &ctx);

    yy_delete_buffer(top_buffer, ctx.scanner);
    yylex_destroy(ctx.scanner);

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
