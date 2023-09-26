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
#include <windows.h>

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
    enum program_result ret = PROGRAM_RESULT_SUCCESS;
    HANDLE log = INVALID_HANDLE_VALUE;
    SECURITY_ATTRIBUTES attrs = {0};
    PROCESS_INFORMATION info = {0};
    DWORD exit_code, wait_result;
    STARTUPINFOA startup = {0};
    char cmdline2[1024];

    strcpy(cmdline2, cmdline);

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
        fprintf(stderr, "Cannot retrive the process exit code, last error %ld.\n", GetLastError());
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

    printf("Building %s\n", commit_dir);
    printf("---\n");

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
        ret = false;

    return ret;
}

int wmain(void)
{
    WIN32_FIND_DATAA find_data;
    HANDLE find_handle;
    bool ret = true;

    SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX);

    find_handle = FindFirstFileA("artifacts/*-*", &find_data);
    if (find_handle == INVALID_HANDLE_VALUE)
    {
        fprintf(stderr, "Cannot list commits, last error %ld.\n", GetLastError());
        ret = false;
    }
    else
    {
        do
        {
            ret &= run_tests_for_directory(find_data.cFileName);
        } while (FindNextFileA(find_handle, &find_data));

        if (GetLastError() != ERROR_NO_MORE_FILES)
        {
            fprintf(stderr, "Cannot list tests, last error %ld.\n", GetLastError());
            ret = false;
        }

        FindClose(find_handle);
    }

    return !ret;
}
