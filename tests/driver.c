/*
 * Copyright 2023 Giovanni Mascellani for CodeWeavers
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

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <windows.h>
#include <shlobj.h>

#define TIMEOUT_MS (10 * 1000)
#define MAX_TIMEOUT_COUNT 3

enum program_result
{
    PROGRAM_RESULT_SUCCESS,
    PROGRAM_RESULT_TIMEOUT,
    PROGRAM_RESULT_FAILURE,
};

static enum program_result run_program(const char *cmdline, const char *log_filename)
{
    char cmdline2[1024], log_dirname[1024], *file_part;
    enum program_result ret = PROGRAM_RESULT_SUCCESS;
    HANDLE log = INVALID_HANDLE_VALUE;
    SECURITY_ATTRIBUTES attrs = {0};
    PROCESS_INFORMATION info = {0};
    DWORD exit_code, wait_result;
    STARTUPINFOA startup = {0};
    int res;

    strcpy(cmdline2, cmdline);

    if (GetFullPathNameA(log_filename, sizeof(log_dirname), log_dirname, &file_part) == 0)
    {
        fprintf(stderr, "Cannot extract the directory name for path %s, last error %ld.\n", log_filename, GetLastError());
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }
    *file_part = '\0';

    res = SHCreateDirectoryExA(NULL, log_dirname, NULL);
    if (res != ERROR_SUCCESS && res != ERROR_ALREADY_EXISTS)
    {
        fprintf(stderr, "Cannot create log directory %s, error %d.\n", log_dirname, res);
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }

    attrs.nLength = sizeof(attrs);
    attrs.bInheritHandle = TRUE;

    log = CreateFileA(log_filename, GENERIC_WRITE, 0, &attrs, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (log == INVALID_HANDLE_VALUE)
    {
        fprintf(stderr, "Cannot create log file %s, last error %ld.\n", log_filename, GetLastError());
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }

    startup.cb = sizeof(startup);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = INVALID_HANDLE_VALUE;
    startup.hStdOutput = log;
    startup.hStdError = log;

    if (!CreateProcessA(NULL, cmdline2, NULL, NULL, TRUE, 0, NULL, NULL, &startup, &info))
    {
        fprintf(stderr, "Cannot create process %s, last error %ld.\n", cmdline2, GetLastError());
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }

    wait_result = WaitForSingleObject(info.hProcess, TIMEOUT_MS);
    if (wait_result == WAIT_TIMEOUT)
    {
        fprintf(stderr, "Process timed out, terminating it.\n");
        ret = PROGRAM_RESULT_TIMEOUT;

        if (!TerminateProcess(info.hProcess, 1))
        {
            fprintf(stderr, "Cannot terminate process, last error %ld.\n", GetLastError());
            goto out;
        }

        wait_result = WaitForSingleObject(info.hProcess, INFINITE);
    }

    if (wait_result != WAIT_OBJECT_0)
    {
        fprintf(stderr, "Cannot wait for process termination, last error %ld.\n", GetLastError());
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }

    if (!GetExitCodeProcess(info.hProcess, &exit_code))
    {
        fprintf(stderr, "Cannot retrieve the process exit code, last error %ld.\n", GetLastError());
        ret = PROGRAM_RESULT_FAILURE;
        goto out;
    }

    ret = exit_code == 0 ? PROGRAM_RESULT_SUCCESS : PROGRAM_RESULT_FAILURE;

    printf("%s: %s\n", ret == PROGRAM_RESULT_SUCCESS ? "PASS" : "FAIL", cmdline);

out:
    if (info.hProcess && !CloseHandle(info.hProcess))
        fprintf(stderr, "Cannot close process, last error %ld.\n", GetLastError());
    if (info.hThread && !CloseHandle(info.hThread))
        fprintf(stderr, "Cannot close thread, last error %ld.\n", GetLastError());
    if (log != INVALID_HANDLE_VALUE && !CloseHandle(log))
        fprintf(stderr, "Cannot close log file, last error %ld.\n", GetLastError());

    return ret;
}

static bool run_tests_for_directory(const char *commit_dir)
{
    char cmdline[1024], log_filename[1024], list_filename[1024], line[1024];
    unsigned int success_count = 0, test_count = 0, timeout_count = 0;
    const char *test_arch = getenv("TEST_ARCH");
    enum program_result result;
    FILE *list_file;
    bool ret = true;

    if (!test_arch)
        test_arch = "64";

    printf("\e[0Ksection_start:%I64d:commit_%s\r\e[0KBuilding commit %s\n",
            (uint64_t)time(NULL), commit_dir, commit_dir);

    sprintf(list_filename, "artifacts/%s/tests/shader_tests.txt", commit_dir);
    list_file = fopen(list_filename, "r");

    if (!list_file)
    {
        fprintf(stderr, "Cannot open list file %s, errno %d.\n", list_filename, errno);
        ret = false;
    }
    else
    {
        while (fgets(line, sizeof(line), list_file) && timeout_count < MAX_TIMEOUT_COUNT)
        {
            size_t len = strlen(line);

            if (line[len - 1] == '\n')
                line[--len] = '\0';

            sprintf(cmdline, "artifacts/%s/tests/shader_runner.cross%s.exe %s", commit_dir, test_arch, line);

            /* Remove the .shader_test suffix. */
            line[len - 12] = '\0';
            sprintf(log_filename, "artifacts/%s/%s.log", commit_dir, line);

            ++test_count;
            result = run_program(cmdline, log_filename);
            success_count += result == PROGRAM_RESULT_SUCCESS;
            timeout_count += result == PROGRAM_RESULT_TIMEOUT;
         }

        fclose(list_file);
    }

    sprintf(list_filename, "artifacts/%s/tests/crosstests.txt", commit_dir);
    list_file = fopen(list_filename, "r");

    if (!list_file)
    {
        fprintf(stderr, "Cannot open list file %s, errno %d.\n", list_filename, errno);
        ret = false;
    }
    else
    {
        while (fgets(line, sizeof(line), list_file) && timeout_count < MAX_TIMEOUT_COUNT)
        {
            size_t len = strlen(line);

            if (line[len - 1] == '\n')
                line[len - 1] = '\0';

            sprintf(cmdline, "artifacts/%s/%s.cross%s.exe", commit_dir, line, test_arch);
            sprintf(log_filename, "artifacts/%s/%s.log", commit_dir, line);
            ++test_count;
            result = run_program(cmdline, log_filename);
            success_count += result == PROGRAM_RESULT_SUCCESS;
            timeout_count += result == PROGRAM_RESULT_TIMEOUT;
        }

        fclose(list_file);
    }

    if (timeout_count >= MAX_TIMEOUT_COUNT)
        fprintf(stderr, "Too many timeouts, aborting tests.\n");

    printf("=======\n");
    printf("Summary\n");
    printf("=======\n");

    printf("# TOTAL: %u\n", test_count);
    printf("# PASS:  %u\n", success_count);
    printf("# FAIL:  %u\n", test_count - success_count);

    if (test_count != success_count)
    {
        HANDLE handle;

        handle = CreateFileA("pipeline_failed", GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (handle == INVALID_HANDLE_VALUE)
        {
            fprintf(stderr, "Cannot create failure file, last error %ld.\n", GetLastError());
            ret = false;
        }
        else
        {
            if (!CloseHandle(handle))
                fprintf(stderr, "Cannot close failure file, last error %ld.\n", GetLastError());
        }
    }

    printf("\e[0Ksection_end:%I64d:commit_%s\r\e[0K\n",
            (uint64_t)time(NULL), commit_dir);

    return ret;
}

int wmain(int argc, WCHAR **wargv)
{
    char commit_num[16], commit_hash[16], commit_dir[16];

    SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX);

    if (argc != 3)
    {
        fprintf(stderr, "Call with commit number and hash.\n");
        return 1;
    }

    WideCharToMultiByte(CP_ACP, 0, wargv[1], -1, commit_num, sizeof(commit_num), NULL, NULL);
    WideCharToMultiByte(CP_ACP, 0, wargv[2], -1, commit_hash, sizeof(commit_hash), NULL, NULL);
    commit_num[sizeof(commit_num) - 1] = '\0';
    commit_hash[sizeof(commit_hash) - 1] = '\0';
    snprintf(commit_dir, sizeof(commit_dir), "%03d-%s", atoi(commit_num), commit_hash);
    commit_dir[sizeof(commit_dir) - 1] = '\0';

    return !run_tests_for_directory(commit_dir);
}
