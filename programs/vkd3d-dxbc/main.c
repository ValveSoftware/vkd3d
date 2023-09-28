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

enum
{
    OPTION_COLOUR = CHAR_MAX + 1,
    OPTION_HELP,
    OPTION_LIST,
    OPTION_LIST_DATA,
    OPTION_NO_COLOUR,
    OPTION_VERSION,
};

struct options
{
    const char *input_filename;
    bool print_help;
    bool list;
    bool list_data;
    bool print_version;

    struct colours
    {
        const char *reset;
        const char *index;
        const char *offset;
        const char *label;
    } colours;
};

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
    supported = !!tigetstr("setaf");
    del_curterm(cur_term);

    return supported;
#else
    return false;
#endif
}

static bool parse_command_line(int argc, char **argv, struct options *options)
{
    int option;

    static struct option long_options[] =
    {
        {"colour",    no_argument,       NULL, OPTION_COLOUR},
        {"help",      no_argument,       NULL, OPTION_HELP},
        {"list",      no_argument,       NULL, OPTION_LIST},
        {"list-data", no_argument,       NULL, OPTION_LIST_DATA},
        {"no-colour", no_argument,       NULL, OPTION_NO_COLOUR},
        {"version",   no_argument,       NULL, OPTION_VERSION},
        {NULL,        0,                 NULL, 0},
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
        if ((option = getopt_long(argc, argv, "htV", long_options, NULL)) == -1)
            break;

        switch (option)
        {
            case OPTION_COLOUR:
                options->colours = colours;
                break;

            case 'h':
            case OPTION_HELP:
                options->print_help = true;
                return true;

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
        "  -h, --help               Display this information and exit.\n"
        "  -t, --list               List the contents of the DXBC blob.\n"
        "  --list-data              List the data contained in the DXBC sections.\n"
        "  --no-colour              Disable colour, even when supported by the output.\n"
        "  -V, --version            Display version information and exit.\n"
        "  --                       Stop option processing. Any subsequent argument is\n"
        "                           interpreted as a filename.\n"
        "\n"
        "If the input file is '-' or not specified, input will be read from standard\n"
        "input.\n";

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
    struct vkd3d_shader_dxbc_desc dxbc_desc;
    struct vkd3d_shader_code dxbc;
    struct options options;
    bool close_input;
    char *messages;
    int fail = 1;
    FILE *input;
    int ret;

    if (!parse_command_line(argc, argv, &options))
    {
        print_usage(argv[0]);
        return 1;
    }

    if (options.print_help || (argc < 2 && isatty(fileno(stdin))))
    {
        print_usage(argv[0]);
        return 0;
    }

    if (options.print_version)
    {
        const char *version = vkd3d_shader_get_version(NULL, NULL);

        fprintf(stdout, "vkd3d-dxbc version " PACKAGE_VERSION " using %s\n", version);
        return 0;
    }

    if (!(input = open_input(options.input_filename, &close_input)))
        goto done;

    if (!read_input(input, &dxbc))
    {
        fprintf(stderr, "Failed to read input blob.\n");
        goto done;
    }

    ret = vkd3d_shader_parse_dxbc(&dxbc, 0, &dxbc_desc, &messages);
    if (messages)
        fputs(messages, stderr);
    vkd3d_shader_free_messages(messages);
    if (ret < 0)
        goto done;

    if (options.list || options.list_data)
        dump_dxbc(&dxbc, &dxbc_desc, &options);

    vkd3d_shader_free_dxbc(&dxbc_desc);
    vkd3d_shader_free_shader_code(&dxbc);
    fail = 0;
done:
    if (close_input)
        fclose(input);
    return fail;
}
