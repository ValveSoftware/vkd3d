/*
 * Copyright 2023 Henri Verbeet for CodeWeavers
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
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/stat.h>

#include "vkd3d_common.h"
#include "vkd3d_shader.h"
#ifdef HAVE_NCURSES
#include <term.h>
#endif

static bool array_reserve(void **elements, size_t *capacity, size_t element_count, size_t element_size)
{
    size_t new_capacity, max_capacity;
    void *new_elements;

    if (element_count <= *capacity)
        return true;

    max_capacity = ~(size_t)0 / element_size;
    if (max_capacity < element_count)
        return false;

    new_capacity = max(*capacity, 4);
    while (new_capacity < element_count && new_capacity <= max_capacity / 2)
        new_capacity *= 2;

    if (new_capacity < element_count)
        new_capacity = element_count;

    if (!(new_elements = realloc(*elements, new_capacity * element_size)))
        return false;

    *elements = new_elements;
    *capacity = new_capacity;

    return true;
}

enum
{
    OPTION_COLOUR = CHAR_MAX + 1,
    OPTION_EMIT,
    OPTION_HELP,
    OPTION_IGNORE_CHECKSUM,
    OPTION_LIST,
    OPTION_LIST_DATA,
    OPTION_NO_COLOUR,
    OPTION_OUTPUT,
    OPTION_VERSION,
};

struct action
{
    enum action_type
    {
        ACTION_TYPE_EMIT,
    } type;
    union
    {
        const char *output_filename;
    } u;
};

struct options
{
    const char *input_filename;
    const char *output_filename;
    bool print_help;
    bool list;
    bool list_data;
    bool print_version;
    bool ignore_checksum;

    struct action *actions;
    size_t action_count, action_capacity;

    struct colours
    {
        const char *reset;
        const char *index;
        const char *offset;
        const char *label;
    } colours;
};

static struct action *action_push(struct options *options, enum action_type type)
{
    struct action *action;

    if (!array_reserve((void **)&options->actions, &options->action_capacity,
            options->action_count + 1, sizeof(*options->actions)))
    {
        fprintf(stderr, "Out of memory.\n");
        exit(1);
    }

    action = &options->actions[options->action_count++];
    action->type = type;

    return action;
}

static bool has_colour(void)
{
#ifdef HAVE_NCURSES
    bool supported;
    int ret;

    if (!isatty(fileno(stdout)))
        return false;
    setupterm(NULL, fileno(stdout), &ret);
    if (ret != 1)
        return false;
    supported = !!tigetstr((char *)"setaf");
    del_curterm(cur_term);

    return supported;
#else
    return false;
#endif
}

static bool parse_command_line(int argc, char **argv, struct options *options)
{
    struct action *action;
    int option;

    static struct option long_options[] =
    {
        {"colour",          no_argument,       NULL, OPTION_COLOUR},
        {"emit",            no_argument,       NULL, OPTION_EMIT},
        {"help",            no_argument,       NULL, OPTION_HELP},
        {"ignore-checksum", no_argument,       NULL, OPTION_IGNORE_CHECKSUM},
        {"list",            no_argument,       NULL, OPTION_LIST},
        {"list-data",       no_argument,       NULL, OPTION_LIST_DATA},
        {"no-colour",       no_argument,       NULL, OPTION_NO_COLOUR},
        {"output",          required_argument, NULL, OPTION_OUTPUT},
        {"version",         no_argument,       NULL, OPTION_VERSION},
        {NULL,              0,                 NULL, 0},
    };

    static const struct colours colours =
    {
        .reset = "\x1b[m",
        .index = "\x1b[92m",
        .offset = "\x1b[36m",
        .label = "\x1b[93m",
    };

    static const struct colours no_colours =
    {
        .reset = "",
        .index = "",
        .offset = "",
        .label = "",
    };

    memset(options, 0, sizeof(*options));
    if (!getenv("NO_COLOUR") && !getenv("NO_COLOR") && has_colour())
        options->colours = colours;
    else
        options->colours = no_colours;

    for (;;)
    {
        if ((option = getopt_long(argc, argv, "ehtVo:", long_options, NULL)) == -1)
            break;

        switch (option)
        {
            case OPTION_COLOUR:
                options->colours = colours;
                break;

            case 'e':
            case OPTION_EMIT:
                action = action_push(options, ACTION_TYPE_EMIT);
                if (!options->output_filename && isatty(fileno(stdout)))
                {
                    fprintf(stderr, "Output is a tty and output format is binary, exiting.\n"
                            "If this is really what you intended, specify the output explicitly.\n");
                    return false;
                }
                action->u.output_filename = options->output_filename;
                break;

            case 'h':
            case OPTION_HELP:
                options->print_help = true;
                return true;

            case OPTION_IGNORE_CHECKSUM:
                options->ignore_checksum = true;
                break;

            case 't':
            case OPTION_LIST:
                options->list = true;
                break;

            case OPTION_LIST_DATA:
                options->list_data = true;
                break;

            case OPTION_NO_COLOUR:
                options->colours = no_colours;
                break;

            case OPTION_OUTPUT:
            case 'o':
                options->output_filename = optarg;
                break;

            case 'V':
            case OPTION_VERSION:
                options->print_version = true;
                return true;

            default:
                return false;
        }
    }

    if (optind < argc)
        options->input_filename = argv[argc - 1];

    return true;
}

static void print_usage(const char *program_name)
{
    static const char usage[] =
        "[options...] [file]\n"
        "Options:\n"
        "  --colour                 Enable colour, even when not supported by the output.\n"
        "  -e, --emit               Emit the content of the DXBC blob.\n"
        "  -h, --help               Display this information and exit.\n"
        "  -t, --list               List the contents of the DXBC blob.\n"
        "  --list-data              List the data contained in the DXBC sections.\n"
        "  --no-colour              Disable colour, even when supported by the output.\n"
        "  -V, --version            Display version information and exit.\n"
        "  --ignore-checksum        Do not validate the checksum when parsing the DXBC blob.\n"
        "  -o, --output=<file>      Set the output filename for --emit.\n"
        "  --                       Stop option processing. Any subsequent argument is\n"
        "                           interpreted as a filename.\n"
        "\n"
        "If the input file is '-' or not specified, input will be read from standard\n"
        "input. Similarly, if the output file is '-' or not specified at the point\n"
        "--emit is found, output will be written to the standard output.\n"
        "\n"
        "Currently this tool can only re-emit the same DXBC blob it loaded. However, it\n"
        "will recompute the checksum while doing so, so --emit can be useful\n"
        "together with --ignore-checksum to fix the checksum for a blob.\n"
        "\n"
        "Options --emit and --output can be specified more than once. The DXBC blob\n"
        "will be emitted once for each --emit occurrence, each time using the closest\n"
        "previous --output occurrence as output filename.\n";

    fprintf(stderr, "Usage: %s %s", program_name, usage);
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

static bool read_input(FILE *f, struct vkd3d_shader_code *dxbc)
{
    size_t size = 4096;
    struct stat st;
    size_t pos = 0;
    uint8_t *data;
    size_t ret;

    memset(dxbc, 0, sizeof(*dxbc));

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

    dxbc->code = data;
    dxbc->size = pos;

    return true;
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

static bool write_output(FILE *f, const struct vkd3d_shader_code *dxbc)
{
    const char *code = dxbc->code;
    size_t pos = 0, ret;

    while (pos != dxbc->size)
    {
        if (!(ret = fwrite(&code[pos], 1, dxbc->size - pos, f)))
            return false;
        pos += ret;
    }

    return true;
}

static const char *dump_tag(char *out, uint32_t tag)
{
    unsigned int i;

    memcpy(out, &tag, sizeof(tag));
    for (i = 0; i < sizeof(tag); ++i)
    {
        if (!isprint(out[i]))
            out[i] = '.';
    }

    return out;
}

static void dump_line(const struct colours *colours, const char *prefix,
        const uint8_t *data, size_t offset, size_t count)
{
    const uint8_t *ptr = &data[offset];
    size_t i;

    if (!count)
        return;

    printf("%s%s%08zx%s  ", prefix, colours->offset, offset, colours->reset);
    for (i = 0; i < count; ++i)
    {
        printf("%02x ", ptr[i]);
        if (i == 7)
            printf(" ");
    }

    while (i < 16)
    {
        printf("   ");
        if (i == 7)
            printf(" ");
        ++i;
    }
    printf(" %s|%s", colours->offset, colours->reset);

    for (i = 0; i < count; ++i)
    {
        printf("%c", isprint(ptr[i]) ? ptr[i] : '.');
    }
    printf("%s|%s\n", colours->offset, colours->reset);
}

static void dump_data(const struct colours *colours, const char *prefix, const struct vkd3d_shader_code *data)
{
    size_t line_count, remainder, i;
    const uint8_t *ptr;

    ptr = data->code;
    line_count = data->size / 16;
    remainder = data->size % 16;

    for (i = 0; i < line_count; ++i, ptr += 16)
    {
        dump_line(colours, prefix, data->code, i * 16, 16);
    }
    if (remainder)
    {
        dump_line(colours, prefix, data->code, i * 16, remainder);
    }
    printf("%s%s%08zx%s\n", prefix, colours->offset, data->size, colours->reset);
}

static void dump_dxbc(const struct vkd3d_shader_code *dxbc, const struct vkd3d_shader_dxbc_desc *dxbc_desc,
        const struct options *options)
{
    const struct colours *colours = &options->colours;
    struct vkd3d_shader_dxbc_section_desc *section;
    char tag[4];
    size_t i;

    printf("         %stag%s: %08x (%.4s)\n",
            colours->label, colours->reset,
            dxbc_desc->tag, dump_tag(tag, dxbc_desc->tag));
    printf("    %schecksum%s: %08x %08x %08x %08x\n",
            colours->label, colours->reset,
            dxbc_desc->checksum[0], dxbc_desc->checksum[1],
            dxbc_desc->checksum[2], dxbc_desc->checksum[3]);
    printf("     %sversion%s: %u\n", colours->label, colours->reset, dxbc_desc->version);
    printf("        %ssize%s: %#zx (%zu) bytes\n", colours->label, colours->reset, dxbc_desc->size, dxbc_desc->size);
    printf("    %ssections%s: %zu\n\n", colours->label, colours->reset, dxbc_desc->section_count);

    printf(" %s#%s  %stag%s              %ssize (bytes)%s             %soffset (bytes)%s\n",
            colours->label, colours->reset,
            colours->label, colours->reset,
            colours->label, colours->reset,
            colours->label, colours->reset);
    for (i = 0; i < dxbc_desc->section_count; ++i)
    {
        char dec_size[32];
        size_t offset;

        section = &dxbc_desc->sections[i];
        sprintf(dec_size, "(%zu)", section->data.size);
        offset = (char *)section->data.code - (char *)dxbc->code;
        printf("%s%2zu%s  %08x (%.4s)  0x%08zx %-13s 0x%08zx (%zu)\n",
                colours->index, i, colours->reset,
                section->tag, dump_tag(tag, section->tag),
                section->data.size, dec_size,
                offset, offset);

        if (!options->list_data)
            continue;
        printf("\n");
        dump_data(colours, "  ", &section->data);
        printf("\n");
    }
}

int main(int argc, char **argv)
{
    bool close_input = false, close_output = false;
    struct vkd3d_shader_code dxbc, serialized;
    struct vkd3d_shader_dxbc_desc dxbc_desc;
    struct options options;
    FILE *input, *output;
    char *messages;
    uint32_t flags;
    bool success;
    int fail = 1;
    size_t i;
    int ret;

    if (!parse_command_line(argc, argv, &options))
    {
        print_usage(argv[0]);
        goto done;
    }

    if (options.print_help || (argc < 2 && isatty(fileno(stdin))))
    {
        print_usage(argv[0]);
        fail = 0;
        goto done;
    }

    if (options.print_version)
    {
        const char *version = vkd3d_shader_get_version(NULL, NULL);

        fprintf(stdout, "vkd3d-dxbc version " PACKAGE_VERSION " using %s\n", version);
        fail = 0;
        goto done;
    }

    if (!(input = open_input(options.input_filename, &close_input)))
        goto done;

    if (!read_input(input, &dxbc))
    {
        fprintf(stderr, "Failed to read input blob.\n");
        goto done;
    }

    flags = 0;
    if (options.ignore_checksum)
        flags |= VKD3D_SHADER_PARSE_DXBC_IGNORE_CHECKSUM;

    ret = vkd3d_shader_parse_dxbc(&dxbc, flags, &dxbc_desc, &messages);
    if (messages)
        fputs(messages, stderr);
    vkd3d_shader_free_messages(messages);
    if (ret < 0)
        goto done;

    if (options.list || options.list_data)
        dump_dxbc(&dxbc, &dxbc_desc, &options);

    for (i = 0; i < options.action_count; ++i)
    {
        struct action *action = &options.actions[i];

        switch (action->type)
        {
            case ACTION_TYPE_EMIT:
                ret = vkd3d_shader_serialize_dxbc(dxbc_desc.section_count, dxbc_desc.sections, &serialized, &messages);
                if (messages)
                    fputs(messages, stderr);
                vkd3d_shader_free_messages(messages);
                if (ret < 0)
                    goto done;

                if (!(output = open_output(action->u.output_filename, &close_output)))
                {
                    vkd3d_shader_free_shader_code(&serialized);
                    goto done;
                }

                if (!(success = write_output(output, &serialized)))
                    fprintf(stderr, "Failed to write output blob.\n");

                if (close_output)
                    fclose(output);
                vkd3d_shader_free_shader_code(&serialized);
                if (!success)
                    goto done;
                break;
        }
    }

    vkd3d_shader_free_dxbc(&dxbc_desc);
    vkd3d_shader_free_shader_code(&dxbc);
    fail = 0;
done:
    free(options.actions);
    if (close_input)
        fclose(input);
    return fail;
}
