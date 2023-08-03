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

#include "vkd3d_common.h"
#include "vkd3d_shader.h"

enum
{
    OPTION_HELP = CHAR_MAX + 1,
    OPTION_VERSION,
};

struct options
{
    bool print_help;
    bool print_version;
};

static bool parse_command_line(int argc, char **argv, struct options *options)
{
    int option;

    static struct option long_options[] =
    {
        {"help",      no_argument,       NULL, OPTION_HELP},
        {"version",   no_argument,       NULL, OPTION_VERSION},
        {NULL,        0,                 NULL, 0},
    };

    memset(options, 0, sizeof(*options));

    for (;;)
    {
        if ((option = getopt_long(argc, argv, "hV", long_options, NULL)) == -1)
            break;

        switch (option)
        {
            case 'h':
            case OPTION_HELP:
                options->print_help = true;
                return true;

            case 'V':
            case OPTION_VERSION:
                options->print_version = true;
                return true;

            default:
                return false;
        }
    }

    return true;
}

static void print_usage(const char *program_name)
{
    static const char usage[] =
        "[options...]\n"
        "Options:\n"
        "  -h, --help               Display this information and exit.\n"
        "  -V, --version            Display version information and exit.\n"
        "  --                       Stop option processing. Any subsequent argument is\n"
        "                           interpreted as a filename.\n";

    fprintf(stderr, "Usage: %s %s", program_name, usage);
}

int main(int argc, char **argv)
{
    struct options options;

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

    return 0;
}
