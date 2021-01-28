/*
 * Copyright 2017 Józef Kucia for CodeWeavers
 * Copyright 2020 Henri Verbeet for CodeWeavers
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <getopt.h>

#include "vkd3d_common.h"
#include "vkd3d_shader.h"
#ifdef HAVE_NCURSES
#include <term.h>
#endif

#define MAX_COMPILE_OPTIONS 3

enum
{
    OPTION_HELP = CHAR_MAX + 1,
    OPTION_BUFFER_UAV,
    OPTION_ENTRY,
    OPTION_OUTPUT,
    OPTION_PRINT_SOURCE_TYPES,
    OPTION_PRINT_TARGET_TYPES,
    OPTION_PROFILE,
    OPTION_STRIP_DEBUG,
    OPTION_VERSION,
    OPTION_TEXT_FORMATTING,
};

static const struct source_type_info
{
    enum vkd3d_shader_source_type type;
    const char *name;
    const char *description;
    bool is_binary;
}
source_type_info[] =
{
    {VKD3D_SHADER_SOURCE_DXBC_TPF,
        "dxbc-tpf",     "A 'Tokenized Program Format' shader embedded in a DXBC container.\n"
        "                This is the format used for Direct3D shader model 4 and 5 shaders.\n",
        true},
    {VKD3D_SHADER_SOURCE_HLSL,
        "hlsl",         "High Level Shader Language source code.\n",
        false},
};

static const struct target_type_info
{
    enum vkd3d_shader_target_type type;
    const char *name;
    const char *description;
    bool is_binary;
}
target_type_info[] =
{
    {VKD3D_SHADER_TARGET_SPIRV_BINARY,
        "spirv-binary", "A SPIR-V shader in binary form.\n"
        "                This is the format used for Vulkan shaders.\n",
        true},
    {VKD3D_SHADER_TARGET_SPIRV_TEXT,
        "spirv-text", "A SPIR-V shader in text form.\n",
        false},
    {VKD3D_SHADER_TARGET_D3D_ASM,
        "d3d-asm", "A shader in Direct3D assembly form.\n",
        false},
    {VKD3D_SHADER_TARGET_DXBC_TPF,
        "dxbc-tpf",     "A 'Tokenized Program Format' shader embedded in a DXBC container.\n"
        "                This is the format used for Direct3D shader model 4 and 5 shaders.\n",
        true},
};

static bool read_shader(struct vkd3d_shader_code *shader, FILE *f)
{
    size_t size = 4096;
    struct stat st;
    size_t pos = 0;
    uint8_t *data;
    size_t ret;

    memset(shader, 0, sizeof(*shader));

    if (fstat(fileno(f), &st) == -1)
    {
        fprintf(stderr, "Could not stat input.\n");
        return false;
    }

    if (S_ISREG(st.st_mode))
        size = st.st_size;

    if (!(data = malloc(size)))
    {
        fprintf(stderr, "Out of memory.\n");
        return false;
    }

    for (;;)
    {
        if (pos >= size)
        {
            if (size > SIZE_MAX / 2 || !(data = realloc(data, size * 2)))
            {
                fprintf(stderr, "Out of memory.\n");
                free(data);
                return false;
            }
            size *= 2;
        }

        if (!(ret = fread(&data[pos], 1, size - pos, f)))
            break;
        pos += ret;
    }

    if (!feof(f))
    {
        free(data);
        return false;
    }

    shader->code = data;
    shader->size = pos;

    return true;
}

static bool write_shader(const struct vkd3d_shader_code *shader, FILE *f)
{
    return fwrite(shader->code, 1, shader->size, f) == shader->size;
}

static void print_usage(const char *program_name)
{
    static const char usage[] =
        "[options...] [file]\n"
        "Options:\n"
        "  -h, --help            Display this information and exit.\n"
        "  -b <type>             Specify the target type.\n"
        "  --buffer-uav=<type>   Specify the buffer type to use for buffer UAV bindings.\n"
        "                        Valid values are 'buffer-texture' (default) and\n"
        "                        'storage-buffer'.\n"
        "  -e, --entry=<name>    Use <name> as the entry point (default is \"main\").\n"
        "  -E                    Preprocess the source code instead of compiling it.\n"
        "  -o, --output=<file>   Write the output to <file>. If <file> is '-' or no\n"
        "                        output file is specified, output will be written to\n"
        "                        standard output.\n"
        "  --formatting=<flags>  Specify the formatting options for text output.\n"
        "                        <flags> is a comma separated list of formatting flags,\n"
        "                        optionally prefixed by '+' or '-'. Valid flags are\n"
        "                        'colour', 'indent', 'offsets', 'header', and 'raw-ids'.\n"
        "                        The 'indent' and 'header' flags are enabled by default.\n"
        "  --print-source-types  Display the supported source types and exit.\n"
        "  --print-target-types  Display the supported target types for the specified\n"
        "                        source type and exit.\n"
        "  -p, --profile=<name>  Specify the target shader profile for HLSL shaders.\n"
        "  --strip-debug         Strip debug information from the output.\n"
        "  -V, --version         Display version information and exit.\n"
        "  -x <type>             Specify the type of the source. Use --print-source-types\n"
        "                        to list valid source types. The default source type is\n"
        "                        'dxbc-tpf' for compile operations, and 'hlsl' for\n"
        "                        preprocess operations.\n"
        "  --                    Stop option processing. Any subsequent argument is\n"
        "                        interpreted as a filename.\n"
        "\n"
        "If the input file is '-' or not specified, input will be read from standard\n"
        "input.\n";

    fprintf(stderr, "Usage: %s %s", program_name, usage);
}

struct options
{
    const char *filename;
    const char *output_filename;
    const char *entry_point;
    const char *profile;
    enum vkd3d_shader_source_type source_type;
    enum vkd3d_shader_target_type target_type;
    bool preprocess_only;
    bool print_version;
    bool print_source_types;
    bool print_target_types;
    uint32_t formatting;
    bool explicit_colour;

    struct vkd3d_shader_compile_option compile_options[MAX_COMPILE_OPTIONS];
    unsigned int compile_option_count;
};

static void add_compile_option(struct options *options,
        enum vkd3d_shader_compile_option_name name, unsigned int value)
{
    struct vkd3d_shader_compile_option *o;
    unsigned int i;

    for (i = 0; i < options->compile_option_count; ++i)
    {
        o = &options->compile_options[i];

        if (o->name == name)
        {
            o->value = value;
            return;
        }
    }

    if (options->compile_option_count >= ARRAY_SIZE(options->compile_options))
    {
        fprintf(stderr, "Ignoring option.\n");
        return;
    }

    o = &options->compile_options[options->compile_option_count++];
    o->name = name;
    o->value = value;
}

static bool parse_buffer_uav(enum vkd3d_shader_compile_option_buffer_uav *buffer_uav, const char *arg)
{
    if (!strcmp(arg, "buffer-texture"))
    {
        *buffer_uav = VKD3D_SHADER_COMPILE_OPTION_BUFFER_UAV_STORAGE_TEXEL_BUFFER;
        return true;
    }

    if (!strcmp(arg, "storage-buffer"))
    {
        *buffer_uav = VKD3D_SHADER_COMPILE_OPTION_BUFFER_UAV_STORAGE_BUFFER;
        return true;
    }

    return false;
}

static bool parse_formatting(uint32_t *formatting, bool *colour, char *arg)
{
    static const struct formatting_option
    {
        char *name;
        enum vkd3d_shader_compile_option_formatting_flags value;
    }
    opts[] =
    {
        {"colour",  VKD3D_SHADER_COMPILE_OPTION_FORMATTING_COLOUR},
        {"indent",  VKD3D_SHADER_COMPILE_OPTION_FORMATTING_INDENT},
        {"offsets", VKD3D_SHADER_COMPILE_OPTION_FORMATTING_OFFSETS},
        {"header",  VKD3D_SHADER_COMPILE_OPTION_FORMATTING_HEADER},
        {"raw-ids", VKD3D_SHADER_COMPILE_OPTION_FORMATTING_RAW_IDS},
    };
    char *tok;

    for (tok = strtok(arg, ","); tok; tok = strtok(NULL, ","))
    {
        bool set = true;
        unsigned int i;

        if (*tok == '-')
        {
            set = false;
            ++tok;
        }
        else if (*tok == '+')
        {
            ++tok;
        }

        for (i = 0; i < ARRAY_SIZE(opts); ++i)
        {
            if (!strcmp(tok, opts[i].name))
            {
                if (set)
                    *formatting |= opts[i].value;
                else
                    *formatting &= ~opts[i].value;
                if (opts[i].value == VKD3D_SHADER_COMPILE_OPTION_FORMATTING_COLOUR)
                    *colour = true;
                break;
            }
        }
        if (i == ARRAY_SIZE(opts))
        {
            fprintf(stderr, "Invalid formatting '%s' specified.\n", tok);
            return false;
        }
    }

    return true;
}

static enum vkd3d_shader_source_type parse_source_type(const char *source)
{
    unsigned int i;

    if (!strcmp(source, "none"))
        return VKD3D_SHADER_SOURCE_DXBC_TPF;

    for (i = 0; i < ARRAY_SIZE(source_type_info); ++i)
    {
        if (!strcmp(source, source_type_info[i].name))
            return source_type_info[i].type;
    }

    return VKD3D_SHADER_SOURCE_NONE;
}

static enum vkd3d_shader_target_type parse_target_type(const char *target)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(target_type_info); ++i)
    {
        if (!strcmp(target, target_type_info[i].name))
            return target_type_info[i].type;
    }

    return VKD3D_SHADER_TARGET_NONE;
}

static const struct source_type_info *get_source_type_info(enum vkd3d_shader_source_type type)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(source_type_info); ++i)
        if (type == source_type_info[i].type)
            return &source_type_info[i];

    return NULL;
}

static const struct target_type_info *get_target_type_info(enum vkd3d_shader_target_type type)
{
    unsigned int i;

    for (i = 0; i < ARRAY_SIZE(target_type_info); ++i)
        if (type == target_type_info[i].type)
            return &target_type_info[i];

    return NULL;
}

static bool validate_target_type(
        enum vkd3d_shader_source_type source_type,
        enum vkd3d_shader_target_type target_type)
{
    unsigned int i, count;
    const enum vkd3d_shader_target_type *supported_types =
        vkd3d_shader_get_supported_target_types(source_type, &count);

    for (i = 0; i < count; ++i)
    {
        if (target_type == supported_types[i])
            return true;
    }

    return false;
}

static bool parse_command_line(int argc, char **argv, struct options *options)
{
    enum vkd3d_shader_compile_option_buffer_uav buffer_uav;
    int option;

    static struct option long_options[] =
    {
        {"help",               no_argument,       NULL, OPTION_HELP},
        {"buffer-uav",         required_argument, NULL, OPTION_BUFFER_UAV},
        {"entry",              required_argument, NULL, OPTION_ENTRY},
        {"output",             required_argument, NULL, OPTION_OUTPUT},
        {"formatting",         required_argument, NULL, OPTION_TEXT_FORMATTING},
        {"print-source-types", no_argument,       NULL, OPTION_PRINT_SOURCE_TYPES},
        {"print-target-types", no_argument,       NULL, OPTION_PRINT_TARGET_TYPES},
        {"profile",            required_argument, NULL, OPTION_PROFILE},
        {"strip-debug",        no_argument,       NULL, OPTION_STRIP_DEBUG},
        {"version",            no_argument,       NULL, OPTION_VERSION},
        {NULL,                 0,                 NULL, 0},
    };

    memset(options, 0, sizeof(*options));
    options->source_type = VKD3D_SHADER_SOURCE_NONE;
    options->target_type = VKD3D_SHADER_TARGET_NONE;
    options->formatting = VKD3D_SHADER_COMPILE_OPTION_FORMATTING_INDENT
            | VKD3D_SHADER_COMPILE_OPTION_FORMATTING_HEADER;

    for (;;)
    {
        if ((option = getopt_long(argc, argv, "b:e:Eho:p:Vx:", long_options, NULL)) == -1)
            break;

        switch (option)
        {
            case 'b':
                if ((options->target_type = parse_target_type(optarg)) == VKD3D_SHADER_TARGET_NONE)
                {
                    fprintf(stderr, "Invalid target type '%s' specified.\n", optarg);
                    return false;
                }
                break;

            case OPTION_BUFFER_UAV:
                if (!parse_buffer_uav(&buffer_uav, optarg))
                {
                    fprintf(stderr, "Invalid buffer UAV type '%s' specified.\n", optarg);
                    return false;
                }
                add_compile_option(options, VKD3D_SHADER_COMPILE_OPTION_BUFFER_UAV, buffer_uav);
                break;

            case OPTION_ENTRY:
            case 'e':
                options->entry_point = optarg;
                break;

            case 'E':
                options->preprocess_only = true;
                break;

            case OPTION_OUTPUT:
            case 'o':
                options->output_filename = optarg;
                break;

            case OPTION_PROFILE:
            case 'p':
                options->profile = optarg;
                break;

            case OPTION_TEXT_FORMATTING:
                if (!parse_formatting(&options->formatting, &options->explicit_colour, optarg))
                    return false;
                break;

            case OPTION_PRINT_SOURCE_TYPES:
                options->print_source_types = true;
                return true;

            case OPTION_PRINT_TARGET_TYPES:
                options->print_target_types = true;
                break;

            case OPTION_STRIP_DEBUG:
                add_compile_option(options, VKD3D_SHADER_COMPILE_OPTION_STRIP_DEBUG, 1);
                break;

            case OPTION_VERSION:
            case 'V':
                options->print_version = true;
                return true;

            case 'x':
                if ((options->source_type = parse_source_type(optarg)) == VKD3D_SHADER_SOURCE_NONE)
                {
                    fprintf(stderr, "Invalid source type '%s' specified.\n", optarg);
                    return false;
                }
                break;

            default:
                return false;
        }
    }

    if (options->source_type == VKD3D_SHADER_SOURCE_NONE)
        options->source_type = options->preprocess_only ? VKD3D_SHADER_SOURCE_HLSL : VKD3D_SHADER_SOURCE_DXBC_TPF;
    if (options->target_type == VKD3D_SHADER_TARGET_NONE && !options->preprocess_only)
        options->target_type = VKD3D_SHADER_TARGET_SPIRV_BINARY;

    if (options->print_target_types)
        return true;

    if (!options->preprocess_only && !validate_target_type(options->source_type, options->target_type))
    {
        fprintf(stderr, "Target type '%s' is invalid for source type '%s'.\n",
                get_target_type_info(options->target_type)->name,
                get_source_type_info(options->source_type)->name);
        return false;
    }

    if (!options->preprocess_only && options->source_type == VKD3D_SHADER_SOURCE_HLSL && !options->profile)
    {
        fprintf(stderr, "You need to specify a profile when compiling from HLSL source.\n");
        return false;
    }

    if (optind < argc)
        options->filename = argv[argc - 1];

    return true;
}

static void print_source_types(void)
{
    const enum vkd3d_shader_source_type *source_types;
    unsigned int count, i;

    source_types = vkd3d_shader_get_supported_source_types(&count);
    fputs("Supported source types:\n", stdout);
    for (i = 0; i < count; ++i)
    {
        const struct source_type_info *type = get_source_type_info(source_types[i]);
        if (type)
            fprintf(stdout, "  %-12s  %s", type->name, type->description);
    }

}

static void print_target_types(enum vkd3d_shader_source_type source_type)
{
    const enum vkd3d_shader_target_type *target_types;
    const char *source_type_name = get_source_type_info(source_type)->name;
    unsigned int count, i;

    target_types = vkd3d_shader_get_supported_target_types(source_type, &count);
    fprintf(stdout, "Supported target types for source type '%s':\n", source_type_name);
    for (i = 0; i < count; ++i)
    {
        const struct target_type_info *type = get_target_type_info(target_types[i]);
        if (type)
            fprintf(stdout, "  %-12s  %s", type->name, type->description);
    }
}

static FILE *open_input(const char *filename, bool *close)
{
    FILE *f;

    *close = false;

    if (!filename || !strcmp(filename, "-"))
        return stdin;

    if (!(f = fopen(filename, "rb")))
    {
        fprintf(stderr, "Unable to open '%s' for reading.\n", filename);
        return NULL;
    }

    *close = true;
    return f;
}

static FILE *open_output(const char *filename, bool *close)
{
    FILE *f;

    *close = false;

    if (!filename || !strcmp(filename, "-"))
        return stdout;

    if (!(f = fopen(filename, "wb")))
    {
        fprintf(stderr, "Unable to open '%s' for writing.\n", filename);
        return NULL;
    }

    *close = true;
    return f;
}

static bool has_colour(FILE *f)
{
#ifdef HAVE_NCURSES
    bool supported;
    int ret;

    if (!isatty(fileno(f)))
        return false;
    setupterm(NULL, fileno(f), &ret);
    if (ret != 1)
        return false;
    supported = !!tigetstr("setaf");
    del_curterm(cur_term);

    return supported;
#else
    return false;
#endif
}

int main(int argc, char **argv)
{
    struct vkd3d_shader_spirv_target_info spirv_target_info = {0};
    struct vkd3d_shader_hlsl_source_info hlsl_source_info = {0};
    bool close_input = false, close_output = false;
    struct vkd3d_shader_compile_info info;
    struct vkd3d_shader_code output_code;
    struct options options;
    FILE *input, *output;
    char *messages;
    int fail = 1;
    int ret;

    if (!parse_command_line(argc, argv, &options))
    {
        print_usage(argv[0]);
        return 1;
    }

    if (options.print_version)
    {
        const char *version = vkd3d_shader_get_version(NULL, NULL);

        fprintf(stdout, "vkd3d shader compiler version " PACKAGE_VERSION " using %s\n", version);
        return 0;
    }

    if (options.print_source_types)
    {
        print_source_types();
        return 0;
    }

    if (options.print_target_types)
    {
        print_target_types(options.source_type);
        return 0;
    }

    if (!(input = open_input(options.filename, &close_input)))
        goto done;

    if (!options.filename && get_source_type_info(options.source_type)->is_binary && isatty(fileno(input)))
    {
        fprintf(stderr, "Input is a tty and input format is binary, exiting.\n"
                "If this is really what you intended, specify the input explicitly.\n");
        goto done;
    }

    if (!(output = open_output(options.output_filename, &close_output)))
        goto done;

    if (!options.output_filename && isatty(fileno(output)))
    {
        bool is_binary;

        if (options.preprocess_only)
            is_binary = get_source_type_info(options.source_type)->is_binary;
        else
            is_binary = get_target_type_info(options.target_type)->is_binary;

        if (is_binary)
        {
            fprintf(stderr, "Output is a tty and output format is binary, exiting.\n"
                    "If this is really what you intended, specify the output explicitly.\n");
            goto done;
        }
    }

    if (!options.explicit_colour && has_colour(output))
        options.formatting |= VKD3D_SHADER_COMPILE_OPTION_FORMATTING_COLOUR;
    add_compile_option(&options, VKD3D_SHADER_COMPILE_OPTION_FORMATTING, options.formatting);

    info.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO;
    info.next = &hlsl_source_info;
    info.source_type = options.source_type;
    info.target_type = options.target_type;
    info.options = options.compile_options;
    info.option_count = options.compile_option_count;
    info.log_level = VKD3D_SHADER_LOG_INFO;
    info.source_name = options.filename;

    hlsl_source_info.type = VKD3D_SHADER_STRUCTURE_TYPE_HLSL_SOURCE_INFO;
    hlsl_source_info.next = &spirv_target_info;
    hlsl_source_info.profile = options.profile;
    hlsl_source_info.entry_point = options.entry_point;

    spirv_target_info.type = VKD3D_SHADER_STRUCTURE_TYPE_SPIRV_TARGET_INFO;
    spirv_target_info.entry_point = options.entry_point;
    spirv_target_info.environment = VKD3D_SHADER_SPIRV_ENVIRONMENT_VULKAN_1_0;

    if (!read_shader(&info.source, input))
    {
        fprintf(stderr, "Failed to read input shader.\n");
        goto done;
    }

    if (options.preprocess_only)
        ret = vkd3d_shader_preprocess(&info, &output_code, &messages);
    else
        ret = vkd3d_shader_compile(&info, &output_code, &messages);

    if (messages)
        fputs(messages, stderr);
    vkd3d_shader_free_messages(messages);
    vkd3d_shader_free_shader_code(&info.source);
    if (ret < 0)
    {
        fprintf(stderr, "Failed to compile shader, ret %d.\n", ret);
        goto done;
    }

    if (!write_shader(&output_code, output))
    {
        fprintf(stderr, "Failed to write output shader.\n");
        vkd3d_shader_free_shader_code(&output_code);
        goto done;
    }

    fail = 0;
    vkd3d_shader_free_shader_code(&output_code);
done:
    if (close_output)
        fclose(output);
    if (close_input)
        fclose(input);
    return fail;
}
