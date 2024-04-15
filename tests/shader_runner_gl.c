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

#include "config.h"

#ifdef HAVE_OPENGL

#define COBJMACROS
#define VKD3D_TEST_NO_DEFS
#define GL_GLEXT_PROTOTYPES
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GL/gl.h>
#include "shader_runner.h"
#include "vkd3d_d3dcompiler.h"

static PFNGLSPECIALIZESHADERPROC p_glSpecializeShader;

enum shading_language
{
    GLSL,
    SPIR_V,
};

struct format_info
{
    enum DXGI_FORMAT f;
    unsigned int component_count;
    bool is_integer;
    bool is_shadow;
    GLenum internal_format;
    GLenum format;
    GLenum type;
};

struct gl_resource
{
    struct resource r;

    const struct format_info *format;
    GLuint id, tbo_id;
};

static struct gl_resource *gl_resource(struct resource *r)
{
    return CONTAINING_RECORD(r, struct gl_resource, r);
}

struct gl_runner
{
    struct shader_runner r;
    struct shader_runner_caps caps;

    EGLDisplay display;
    EGLContext context;

    uint32_t attribute_map;
    GLuint fbo_id;

    struct vkd3d_shader_combined_resource_sampler *combined_samplers;
    unsigned int combined_sampler_count;
    enum shading_language language;
};

static struct gl_runner *gl_runner(struct shader_runner *r)
{
    return CONTAINING_RECORD(r, struct gl_runner, r);
}

static void debug_output(GLenum source, GLenum type, GLuint id, GLenum severity,
        GLsizei length, const GLchar *message, const void *userParam)
{
    if (message[length - 1] == '\n')
        --length;
    trace("%.*s\n", length, message);
}

static bool check_gl_extension(const char *extension, GLint extension_count)
{
    for (GLint i = 0; i < extension_count; ++i)
    {
        if (!strcmp(extension, (const char *)glGetStringi(GL_EXTENSIONS, i)))
            return true;
    }

    return false;
}

static bool check_gl_extensions(struct gl_runner *runner)
{
    GLint count;

    static const char *required_extensions[] =
    {
        "GL_ARB_clip_control",
        "GL_ARB_compute_shader",
        "GL_ARB_sampler_objects",
        "GL_ARB_shader_image_load_store",
        "GL_ARB_texture_storage",
        "GL_ARB_internalformat_query",
    };

    glGetIntegerv(GL_NUM_EXTENSIONS, &count);

    if (runner->language == SPIR_V && !check_gl_extension("GL_ARB_gl_spirv", count))
        return false;

    for (unsigned int i = 0; i < ARRAY_SIZE(required_extensions); ++i)
    {
        if (!check_gl_extension(required_extensions[i], count))
            return false;
    }

    if (check_gl_extension("GL_ARB_gpu_shader_fp64", count))
        runner->caps.float64 = true;
    if (check_gl_extension("GL_ARB_gpu_shader_int64", count))
        runner->caps.int64 = true;

    return true;
}

static bool check_egl_client_extension(const char *extension)
{
    const char *extensions, *p;
    size_t len;

    if (!(extensions = eglQueryString(EGL_NO_DISPLAY, EGL_EXTENSIONS)))
        return false;

    len = strlen(extension);
    for (;;)
    {
        if (!(p = strchr(extensions, ' ')))
            p = &extensions[strlen(extensions)];
        if (p - extensions == len && !memcmp(extensions, extension, len))
            return true;
        if (!*p)
            break;
        extensions = p + 1;
    }
    return false;
}

static bool gl_runner_init(struct gl_runner *runner, enum shading_language language)
{
    PFNEGLQUERYDEVICESEXTPROC eglQueryDevicesEXT;
    const char *glsl_version = NULL;
    EGLDeviceEXT *devices;
    EGLContext context;
    EGLDisplay display;
    EGLBoolean ret;
    EGLint count;
    GLuint vao;

    static const char *const tags[] =
    {
        "glsl",
    };

    static const EGLint attributes[] =
    {
        EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
        EGL_CONTEXT_FLAGS_KHR, EGL_CONTEXT_OPENGL_DEBUG_BIT_KHR,
        EGL_CONTEXT_MAJOR_VERSION, 3,
        EGL_CONTEXT_MINOR_VERSION, 2,
        EGL_NONE,
    };

    memset(runner, 0, sizeof(*runner));
    runner->language = language;

    if (!check_egl_client_extension("EGL_EXT_device_enumeration")
            || !(eglQueryDevicesEXT = (void *)eglGetProcAddress("eglQueryDevicesEXT")))
    {
        skip("Failed to retrieve eglQueryDevicesEXT.\n");
        return false;
    }

    ret = eglQueryDevicesEXT(0, NULL, &count);
    ok(ret, "Failed to query device count.\n");

    devices = calloc(count, sizeof(*devices));
    ret = eglQueryDevicesEXT(count, devices, &count);
    ok(ret, "Failed to query devices.\n");

    for (unsigned int i = 0; i < count; ++i)
    {
        if ((display = eglGetPlatformDisplay(EGL_PLATFORM_DEVICE_EXT, devices[i], NULL)) == EGL_NO_DISPLAY)
        {
            trace("Failed to get EGL display connection for device %u.\n", i);
            continue;
        }

        if (!eglInitialize(display, NULL, NULL))
        {
            trace("Failed to initialise EGL display connection for device %u.\n", i);
            continue;
        }

        if (!eglBindAPI(EGL_OPENGL_API))
        {
            trace("Failed to bind OpenGL API for device %u.\n", i);
            eglTerminate(display);
            continue;
        }

        if ((context = eglCreateContext(display, NULL, EGL_NO_CONTEXT, attributes)) == EGL_NO_CONTEXT)
        {
            trace("Failed to create EGL context for device %u.\n", i);
            eglTerminate(display);
            continue;
        }

        if (!eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, context))
        {
            trace("Failed to make EGL context current for device %u.\n", i);
            eglDestroyContext(display, context);
            eglTerminate(display);
            continue;
        }

        glsl_version = (const char *)glGetString(GL_SHADING_LANGUAGE_VERSION);
        if (language == GLSL)
        {
            unsigned int major, minor;
            sscanf(glsl_version, "%u.%u", &major, &minor);
            if (major < 4 || (major == 4 && minor < 40))
            {
                trace("Device %u does not support GLSL 4.40.\n", i);
                eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
                eglDestroyContext(display, context);
                eglTerminate(display);
                continue;
            }
        }

        memset(&runner->caps, 0, sizeof(runner->caps));
        if (!check_gl_extensions(runner))
        {
            trace("Device %u lacks required extensions.\n", i);
            eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
            eglDestroyContext(display, context);
            eglTerminate(display);
            continue;
        }
        runner->caps.runner = language == SPIR_V ? "OpenGL/SPIR-V" : "OpenGL/GLSL";
        runner->caps.tags = tags;
        runner->caps.tag_count = runner->language == GLSL;
        runner->caps.minimum_shader_model = SHADER_MODEL_4_0;
        runner->caps.maximum_shader_model = SHADER_MODEL_5_1;

        trace("Using device %u.\n", i);
        runner->display = display;
        runner->context = context;
        break;
    }

    free(devices);

    if (!runner->context)
    {
        skip("Failed to find a usable OpenGL device.\n");
        return false;
    }

    trace("                  GL_VENDOR: %s\n", glGetString(GL_VENDOR));
    trace("                GL_RENDERER: %s\n", glGetString(GL_RENDERER));
    trace("                 GL_VERSION: %s\n", glGetString(GL_VERSION));
    trace("GL_SHADING_LANGUAGE_VERSION: %s\n", glsl_version);

    p_glSpecializeShader = (void *)eglGetProcAddress("glSpecializeShader");

    glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DEBUG_SEVERITY_NOTIFICATION, 0, NULL, GL_FALSE);
    glDebugMessageCallback(debug_output, NULL);
    glClipControl(GL_UPPER_LEFT, GL_ZERO_TO_ONE);
    glFrontFace(GL_CW);
    glProvokingVertex(GL_FIRST_VERTEX_CONVENTION);
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    return true;
}

static void reset_combined_samplers(struct gl_runner *runner)
{
    free(runner->combined_samplers);
    runner->combined_samplers = NULL;
    runner->combined_sampler_count = 0;
}

static void gl_runner_cleanup(struct gl_runner *runner)
{
    EGLBoolean ret;

    reset_combined_samplers(runner);

    if (runner->fbo_id)
        glDeleteFramebuffers(1, &runner->fbo_id);

    ret = eglMakeCurrent(runner->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    ok(ret, "Failed to release current EGL context.\n");
    ret = eglDestroyContext(runner->display, runner->context);
    ok(ret, "Failed to destroy EGL context.\n");
    ret = eglTerminate(runner->display);
    ok(ret, "Failed to terminate EGL display connection.\n");
}

static const struct format_info *get_format_info(enum DXGI_FORMAT format, bool is_shadow)
{
    size_t i;

    static const struct format_info format_info[] =
    {
        {DXGI_FORMAT_UNKNOWN,            1, true,  false, GL_R32UI,              GL_RED_INTEGER,     GL_UNSIGNED_INT},
        {DXGI_FORMAT_R32G32B32A32_FLOAT, 4, false, false, GL_RGBA32F,            GL_RGBA,            GL_FLOAT},
        {DXGI_FORMAT_R32G32B32A32_UINT,  4, true,  false, GL_RGBA32UI,           GL_RGBA_INTEGER,    GL_UNSIGNED_INT},
        {DXGI_FORMAT_R32G32B32A32_SINT,  4, true,  false, GL_RGBA32I,            GL_RGBA_INTEGER,    GL_INT},
        {DXGI_FORMAT_R32G32_FLOAT,       2, false, false, GL_RG32F,              GL_RG,              GL_FLOAT},
        {DXGI_FORMAT_R32G32_UINT,        2, true,  false, GL_RG32UI,             GL_RG_INTEGER,      GL_UNSIGNED_INT},
        {DXGI_FORMAT_R32G32_SINT,        2, true,  false, GL_RG32I,              GL_RG_INTEGER,      GL_INT},
        {DXGI_FORMAT_R32_FLOAT,          1, false, false, GL_R32F,               GL_RED,             GL_FLOAT},
        {DXGI_FORMAT_R32_FLOAT,          1, false, true,  GL_DEPTH_COMPONENT32F, GL_DEPTH_COMPONENT, GL_FLOAT},
        {DXGI_FORMAT_D32_FLOAT,          1, false, true,  GL_DEPTH_COMPONENT32F, GL_DEPTH_COMPONENT, GL_FLOAT},
        {DXGI_FORMAT_R32_UINT,           1, true,  false, GL_R32UI,              GL_RED_INTEGER,     GL_UNSIGNED_INT},
        {DXGI_FORMAT_R32_SINT,           1, true,  false, GL_R32I,               GL_RED_INTEGER,     GL_INT},
    };

    for (i = 0; i < ARRAY_SIZE(format_info); ++i)
    {
        if (format_info[i].f == format && format_info[i].is_shadow == is_shadow)
            return &format_info[i];
    }

    fatal_error("Failed to find format info for format %#x.\n", format);
}

static bool init_resource_2d(struct gl_resource *resource, const struct resource_params *params)
{
    unsigned int offset, w, h, i;

    resource->format = get_format_info(params->format, params->is_shadow);

    if (params->sample_count > 1)
    {
        GLint max_sample_count;

        glGetInternalformativ(GL_TEXTURE_2D_MULTISAMPLE, resource->format->internal_format, GL_SAMPLES, 1, &max_sample_count);
        if (max_sample_count < params->sample_count)
        {
            trace("Format #%x with sample count %u is not supported; skipping.\n", params->format, params->sample_count);
            return false;
        }
    }

    glGenTextures(1, &resource->id);
    glBindTexture(GL_TEXTURE_2D, resource->id);
    glTexStorage2D(GL_TEXTURE_2D, params->level_count,
            resource->format->internal_format, params->width, params->height);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    if (!params->data)
        return true;

    for (i = 0, offset = 0; i < params->level_count; ++i)
    {
        w = get_level_dimension(params->width, i);
        h = get_level_dimension(params->height, i);
        glTexSubImage2D(GL_TEXTURE_2D, i, 0, 0, w, h, resource->format->format,
                resource->format->type, params->data + offset);
        offset += w * h * params->texel_size;
    }

    return true;
}

static void init_resource_buffer(struct gl_resource *resource, const struct resource_params *params)
{
    resource->format = get_format_info(params->format, false);

    glGenBuffers(1, &resource->id);
    glBindBuffer(GL_TEXTURE_BUFFER, resource->id);
    glBufferData(GL_TEXTURE_BUFFER, params->data_size, params->data, GL_STATIC_DRAW);

    glGenTextures(1, &resource->tbo_id);
    glBindTexture(GL_TEXTURE_BUFFER, resource->tbo_id);
    glTexBuffer(GL_TEXTURE_BUFFER, resource->format->internal_format, resource->id);
}

static struct resource *gl_runner_create_resource(struct shader_runner *r, const struct resource_params *params)
{
    struct gl_resource *resource;

    resource = calloc(1, sizeof(*resource));
    init_resource(&resource->r, params);

    switch (params->type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
        case RESOURCE_TYPE_DEPTH_STENCIL:
        case RESOURCE_TYPE_TEXTURE:
        case RESOURCE_TYPE_UAV:
            if (params->dimension == RESOURCE_DIMENSION_BUFFER)
                init_resource_buffer(resource, params);
            else if (!init_resource_2d(resource, params))
                return NULL;
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            glGenBuffers(1, &resource->id);
            glBindBuffer(GL_ARRAY_BUFFER, resource->id);
            glBufferData(GL_ARRAY_BUFFER, params->data_size, params->data, GL_STATIC_DRAW);
            break;
    }

    return &resource->r;
}

static void gl_runner_destroy_resource(struct shader_runner *r, struct resource *res)
{
    struct gl_resource *resource = gl_resource(res);

    switch (resource->r.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
        case RESOURCE_TYPE_DEPTH_STENCIL:
        case RESOURCE_TYPE_TEXTURE:
        case RESOURCE_TYPE_UAV:
            if (res->dimension == RESOURCE_DIMENSION_BUFFER)
            {
                glDeleteTextures(1, &resource->tbo_id);
                glDeleteBuffers(1, &resource->id);
            }
            else
            {
                glDeleteTextures(1, &resource->id);
            }
            break;

        case RESOURCE_TYPE_VERTEX_BUFFER:
            glDeleteBuffers(1, &resource->id);
            break;
    }

    free(resource);
}

static ID3DBlob *compile_hlsl(const struct shader_runner *runner, const char *source, const char *type)
{
    ID3DBlob *blob = NULL, *errors = NULL;
    char profile[7];

    static const char *const shader_models[] =
    {
        [SHADER_MODEL_4_0] = "4_0",
        [SHADER_MODEL_4_1] = "4_1",
        [SHADER_MODEL_5_0] = "5_0",
        [SHADER_MODEL_5_1] = "5_1",
    };

    sprintf(profile, "%s_%s", type, shader_models[runner->minimum_shader_model]);
    D3DCompile(source, strlen(source), NULL, NULL, NULL, "main",
            profile, runner->compile_options, 0, &blob, &errors);
    if (errors)
    {
        trace("%s\n", (char *)ID3D10Blob_GetBufferPointer(errors));
        ID3D10Blob_Release(errors);
    }

    return blob;
}

static bool compile_shader(struct gl_runner *runner, ID3DBlob *blob, struct vkd3d_shader_code *out)
{
    struct vkd3d_shader_spirv_target_info spirv_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_SPIRV_TARGET_INFO};
    struct vkd3d_shader_interface_info interface_info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_INTERFACE_INFO};
    struct vkd3d_shader_compile_info info = {.type = VKD3D_SHADER_STRUCTURE_TYPE_COMPILE_INFO};
    struct vkd3d_shader_resource_binding bindings[MAX_RESOURCES + 1 /* CBV */];
    struct vkd3d_shader_scan_combined_resource_sampler_info combined_sampler_info;
    struct vkd3d_shader_combined_resource_sampler *sampler;
    struct vkd3d_shader_resource_binding *binding;
    unsigned int count, i;
    char *messages;
    int ret;

    const struct vkd3d_shader_compile_option options[] =
    {
        {VKD3D_SHADER_COMPILE_OPTION_API_VERSION, VKD3D_SHADER_API_VERSION_1_11},
        {VKD3D_SHADER_COMPILE_OPTION_FRAGMENT_COORDINATE_ORIGIN,
                VKD3D_SHADER_COMPILE_OPTION_FRAGMENT_COORDINATE_ORIGIN_LOWER_LEFT},
        {VKD3D_SHADER_COMPILE_OPTION_FEATURE, shader_runner_caps_get_feature_flags(&runner->caps)},
    };

    info.next = &combined_sampler_info;
    info.source.code = ID3D10Blob_GetBufferPointer(blob);
    info.source.size = ID3D10Blob_GetBufferSize(blob);
    info.source_type = VKD3D_SHADER_SOURCE_DXBC_TPF;
    info.target_type = runner->language == SPIR_V ? VKD3D_SHADER_TARGET_SPIRV_BINARY : VKD3D_SHADER_TARGET_GLSL;
    info.options = options;
    info.option_count = ARRAY_SIZE(options);
    info.log_level = VKD3D_SHADER_LOG_WARNING;

    combined_sampler_info.type = VKD3D_SHADER_STRUCTURE_TYPE_SCAN_COMBINED_RESOURCE_SAMPLER_INFO;
    combined_sampler_info.next = NULL;

    ret = vkd3d_shader_scan(&info, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    if (ret)
        return false;

    count = runner->combined_sampler_count + combined_sampler_info.combined_sampler_count;
    if (count && !(runner->combined_samplers = realloc(runner->combined_samplers,
            count * sizeof(*runner->combined_samplers))))
        fatal_error("Failed to allocate combined samplers array.\n");
    for (i = 0; i < combined_sampler_info.combined_sampler_count; ++i)
    {
        const struct vkd3d_shader_combined_resource_sampler_info *s = &combined_sampler_info.combined_samplers[i];

        sampler = &runner->combined_samplers[runner->combined_sampler_count];
        sampler->resource_space = s->resource_space;
        sampler->resource_index = s->resource_index;
        sampler->sampler_space = s->sampler_space;
        sampler->sampler_index = s->sampler_index;
        sampler->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
        /* We don't know if this combined sampler was created from a SRV buffer or a SRV image, so
         * we pass both flags, otherwise the combined sampler won't be recognized when emitting the
         * SPIR-V, which will result in a failing assertion. */
        sampler->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE | VKD3D_SHADER_BINDING_FLAG_BUFFER;
        sampler->binding.set = 0;
        sampler->binding.binding = runner->combined_sampler_count++;
        sampler->binding.count = 1;
    }
    vkd3d_shader_free_scan_combined_resource_sampler_info(&combined_sampler_info);

    if (runner->language == SPIR_V)
    {
        info.next = &spirv_info;
        spirv_info.next = &interface_info;
        spirv_info.environment = VKD3D_SHADER_SPIRV_ENVIRONMENT_OPENGL_4_5;
    }
    else
    {
        info.next = &interface_info;
    }

    if (runner->r.uniform_count)
    {
        binding = &bindings[interface_info.binding_count++];
        binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_CBV;
        binding->register_space = 0;
        binding->register_index = 0;
        binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
        binding->flags = VKD3D_SHADER_BINDING_FLAG_BUFFER;
        binding->binding.set = 0;
        binding->binding.binding = 0;
        binding->binding.count = 1;
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        const struct gl_resource *resource = gl_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_UAV:
                binding = &bindings[interface_info.binding_count++];
                binding->type = VKD3D_SHADER_DESCRIPTOR_TYPE_UAV;
                binding->register_space = 0;
                binding->register_index = resource->r.slot;
                binding->shader_visibility = VKD3D_SHADER_VISIBILITY_ALL;
                if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_BUFFER;
                else
                    binding->flags = VKD3D_SHADER_BINDING_FLAG_IMAGE;
                binding->binding.set = 0;
                binding->binding.binding = resource->r.slot;
                binding->binding.count = 1;
                break;

            default:
                break;
        }
    }

    interface_info.bindings = bindings;
    interface_info.combined_samplers = runner->combined_samplers;
    interface_info.combined_sampler_count = runner->combined_sampler_count;

    ret = vkd3d_shader_compile(&info, out, &messages);
    if (messages && vkd3d_test_state.debug_level)
        trace("%s\n", messages);
    vkd3d_shader_free_messages(messages);
    if (ret)
        return false;

    return true;
}

static void trace_info_log(GLuint id, bool program)
{
    const char *p, *end, *line;
    GLint length = 0;
    char *log;

    if (program)
        glGetProgramiv(id, GL_INFO_LOG_LENGTH, &length);
    else
        glGetShaderiv(id, GL_INFO_LOG_LENGTH, &length);

    if (length <= 1)
        return;

    log = malloc(length);
    if (program)
        glGetProgramInfoLog(id, length, NULL, log);
    else
        glGetShaderInfoLog(id, length, NULL, log);
    log[length - 1] = '\n';

    trace("Info log received from %s #%u:\n", program ? "program" : "shader", id);

    p = log;
    end = &log[length];
    while (p < end)
    {
        line = p;
        if ((p = memchr(line, '\n', end - line)))
            ++p;
        else
            p = end;
        trace("    %.*s", (int)(p - line), line);
    }
    free(log);
}

static GLuint compile_compute_shader_program(struct gl_runner *runner)
{
    struct vkd3d_shader_code cs_code;
    GLuint program_id, cs_id;
    const GLchar *source;
    ID3D10Blob *cs_blob;
    GLint status, size;
    bool ret;

    reset_combined_samplers(runner);
    if (!(cs_blob = compile_hlsl(&runner->r, runner->r.cs_source, "cs")))
        return false;
    ret = compile_shader(runner, cs_blob, &cs_code);
    ID3D10Blob_Release(cs_blob);
    if (!ret)
        return false;

    cs_id = glCreateShader(GL_COMPUTE_SHADER);
    if (runner->language == SPIR_V)
    {
        glShaderBinary(1, &cs_id, GL_SHADER_BINARY_FORMAT_SPIR_V, cs_code.code, cs_code.size);
    }
    else
    {
        source = cs_code.code;
        size = cs_code.size;
        glShaderSource(cs_id, 1, &source, &size);
        glCompileShader(cs_id);
    }
    vkd3d_shader_free_shader_code(&cs_code);
    if (runner->language == SPIR_V)
        p_glSpecializeShader(cs_id, "main", 0, NULL, NULL);
    glGetShaderiv(cs_id, GL_COMPILE_STATUS, &status);
    ok(status, "Failed to compile compute shader.\n");
    trace_info_log(cs_id, false);

    program_id = glCreateProgram();
    glAttachShader(program_id, cs_id);
    glLinkProgram(program_id);
    glGetProgramiv(program_id, GL_LINK_STATUS, &status);
    ok(status, "Failed to link program.\n");
    trace_info_log(program_id, true);

    glDeleteShader(cs_id);

    return program_id;
}

static bool gl_runner_dispatch(struct shader_runner *r, unsigned int x, unsigned int y, unsigned int z)
{
    struct gl_runner *runner = gl_runner(r);
    GLuint program_id, ubo_id = 0;
    unsigned int i;

    program_id = compile_compute_shader_program(runner);
    todo_if(runner->r.is_todo) ok(program_id, "Failed to compile shader program.\n");
    if (!program_id)
        return false;
    glUseProgram(program_id);

    if (runner->r.uniform_count)
    {
        glGenBuffers(1, &ubo_id);
        glBindBufferBase(GL_UNIFORM_BUFFER, 0, ubo_id);
        glBufferData(GL_UNIFORM_BUFFER, runner->r.uniform_count * sizeof(*runner->r.uniforms),
                runner->r.uniforms, GL_STATIC_DRAW);
    }

    for (i = 0; i < runner->r.resource_count; ++i)
    {
        struct gl_resource *resource = gl_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
            case RESOURCE_TYPE_DEPTH_STENCIL:
            case RESOURCE_TYPE_VERTEX_BUFFER:
            case RESOURCE_TYPE_TEXTURE:
                break;

            case RESOURCE_TYPE_UAV:
                if (resource->r.dimension != RESOURCE_DIMENSION_BUFFER)
                    glBindImageTexture(resource->r.slot, resource->id, 0, GL_TRUE,
                            0, GL_READ_WRITE, resource->format->internal_format);
                break;
        }
    }

    glDispatchCompute(x, y, z);

    glDeleteBuffers(1, &ubo_id);
    glDeleteProgram(program_id);

    return true;
}

static GLenum get_topology_gl(D3D_PRIMITIVE_TOPOLOGY topology)
{
    switch (topology)
    {
        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST:
            return GL_TRIANGLES;

        case D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP:
            return GL_TRIANGLE_STRIP;

        default:
            fatal_error("Unhandled topology %#x.\n", topology);
    }
}

static GLenum get_texture_wrap_gl(D3D12_TEXTURE_ADDRESS_MODE mode)
{
    switch (mode)
    {
        case D3D12_TEXTURE_ADDRESS_MODE_WRAP:
            return GL_REPEAT;

        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR:
            return GL_MIRRORED_REPEAT;

        case D3D12_TEXTURE_ADDRESS_MODE_CLAMP:
            return GL_CLAMP_TO_EDGE;

        case D3D12_TEXTURE_ADDRESS_MODE_BORDER:
            return GL_CLAMP_TO_BORDER_ARB;

        case D3D12_TEXTURE_ADDRESS_MODE_MIRROR_ONCE:
            return GL_MIRROR_CLAMP_TO_EDGE;

        default:
            fatal_error("Unhandled address mode %#x.\n", mode);
    }
}

static GLenum get_texture_filter_mag_gl(D3D12_FILTER filter)
{
    return filter & 0x4 ? GL_LINEAR : GL_NEAREST;
}

static GLenum get_texture_filter_min_gl(D3D12_FILTER filter)
{
    if (filter & 0x1)
        return filter & 0x10 ? GL_LINEAR_MIPMAP_LINEAR : GL_LINEAR_MIPMAP_NEAREST;
    else
        return filter & 0x10 ? GL_NEAREST_MIPMAP_LINEAR : GL_NEAREST_MIPMAP_NEAREST;
}

static GLenum get_compare_op_gl(D3D12_COMPARISON_FUNC op)
{
    switch (op)
    {
        case D3D12_COMPARISON_FUNC_NEVER:
            return GL_NEVER;
        case D3D12_COMPARISON_FUNC_LESS:
            return GL_LESS;
        case D3D12_COMPARISON_FUNC_EQUAL:
            return GL_EQUAL;
        case D3D12_COMPARISON_FUNC_LESS_EQUAL:
            return GL_LEQUAL;
        case D3D12_COMPARISON_FUNC_GREATER:
            return GL_GREATER;
        case D3D12_COMPARISON_FUNC_NOT_EQUAL:
            return GL_NOTEQUAL;
        case D3D12_COMPARISON_FUNC_GREATER_EQUAL:
            return GL_GEQUAL;
        case D3D12_COMPARISON_FUNC_ALWAYS:
            return GL_ALWAYS;
        default:
            fatal_error("Unhandled compare op %#x.\n", op);
    }
}

static GLuint compile_graphics_shader_program(struct gl_runner *runner, ID3D10Blob **vs_blob)
{
    ID3D10Blob *fs_blob, *hs_blob = NULL, *ds_blob = NULL;
    struct vkd3d_shader_code vs_code, fs_code;
    GLuint program_id, vs_id, fs_id;
    const GLchar *source;
    GLint status, size;
    bool succeeded;

    reset_combined_samplers(runner);

    *vs_blob = compile_hlsl(&runner->r, runner->r.vs_source, "vs");
    fs_blob = compile_hlsl(&runner->r, runner->r.ps_source, "ps");
    succeeded = *vs_blob && fs_blob;

    if (runner->r.hs_source)
    {
        hs_blob = compile_hlsl(&runner->r, runner->r.hs_source, "hs");
        succeeded = succeeded && hs_blob;
    }
    if (runner->r.ds_source)
    {
        ds_blob = compile_hlsl(&runner->r, runner->r.ds_source, "ds");
        succeeded = succeeded && ds_blob;
    }

    if (!succeeded)
    {
        if (*vs_blob)
            ID3D10Blob_Release(*vs_blob);
        if (fs_blob)
            ID3D10Blob_Release(fs_blob);
        if (hs_blob)
            ID3D10Blob_Release(hs_blob);
        if (ds_blob)
            ID3D10Blob_Release(ds_blob);
        return false;
    }

    if (!compile_shader(runner, *vs_blob, &vs_code))
    {
        ID3D10Blob_Release(fs_blob);
        ID3D10Blob_Release(*vs_blob);
        return false;
    }

    if (!compile_shader(runner, fs_blob, &fs_code))
    {
        vkd3d_shader_free_shader_code(&vs_code);
        ID3D10Blob_Release(fs_blob);
        ID3D10Blob_Release(*vs_blob);
        return false;
    }
    ID3D10Blob_Release(fs_blob);

    /* TODO: compile and use the hs and ds blobs too, but currently this
     * point is not reached because compile_hlsl() fails on these. */
    if (hs_blob)
        ID3D10Blob_Release(hs_blob);
    if (ds_blob)
        ID3D10Blob_Release(ds_blob);

    vs_id = glCreateShader(GL_VERTEX_SHADER);
    if (runner->language == SPIR_V)
    {
        glShaderBinary(1, &vs_id, GL_SHADER_BINARY_FORMAT_SPIR_V, vs_code.code, vs_code.size);
    }
    else
    {
        source = vs_code.code;
        size = vs_code.size;
        glShaderSource(vs_id, 1, &source, &size);
        glCompileShader(vs_id);
    }
    vkd3d_shader_free_shader_code(&vs_code);
    if (runner->language == SPIR_V)
        p_glSpecializeShader(vs_id, "main", 0, NULL, NULL);
    glGetShaderiv(vs_id, GL_COMPILE_STATUS, &status);
    ok(status, "Failed to compile vertex shader.\n");
    trace_info_log(vs_id, false);

    fs_id = glCreateShader(GL_FRAGMENT_SHADER);
    if (runner->language == SPIR_V)
    {
        glShaderBinary(1, &fs_id, GL_SHADER_BINARY_FORMAT_SPIR_V, fs_code.code, fs_code.size);
    }
    else
    {
        source = fs_code.code;
        size = fs_code.size;
        glShaderSource(fs_id, 1, &source, &size);
        glCompileShader(fs_id);
    }
    vkd3d_shader_free_shader_code(&fs_code);
    if (runner->language == SPIR_V)
        p_glSpecializeShader(fs_id, "main", 0, NULL, NULL);
    glGetShaderiv(fs_id, GL_COMPILE_STATUS, &status);
    ok(status, "Failed to compile fragment shader.\n");
    trace_info_log(fs_id, false);

    program_id = glCreateProgram();
    glAttachShader(program_id, vs_id);
    glAttachShader(program_id, fs_id);
    glLinkProgram(program_id);
    glGetProgramiv(program_id, GL_LINK_STATUS, &status);
    ok(status, "Failed to link program.\n");
    trace_info_log(program_id, true);

    glDeleteShader(fs_id);
    glDeleteShader(vs_id);

    return program_id;
}

static void gl_runner_clear(struct shader_runner *r, struct resource *res, const struct vec4 *clear_value)
{
    struct gl_resource *resource = gl_resource(res);
    struct gl_runner *runner = gl_runner(r);
    GLbitfield clear_mask;

    if (!runner->fbo_id)
        glGenFramebuffers(1, &runner->fbo_id);
    glBindFramebuffer(GL_FRAMEBUFFER, runner->fbo_id);

    switch (resource->r.type)
    {
        case RESOURCE_TYPE_RENDER_TARGET:
            glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, resource->id, 0);
            glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
            glClearColor(clear_value->x, clear_value->y, clear_value->z, clear_value->w);
            clear_mask = GL_COLOR_BUFFER_BIT;
            break;

        case RESOURCE_TYPE_DEPTH_STENCIL:
            glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, resource->id, 0);
            glDepthMask(GL_TRUE);
            glClearDepthf(clear_value->x);
            clear_mask = GL_DEPTH_BUFFER_BIT;
            break;

        default:
            fatal_error("Clears are not implemented for resource type %u.\n", resource->r.type);
    }

    glScissor(0, 0, res->width, res->height);
    glClear(clear_mask);
}

static bool gl_runner_draw(struct shader_runner *r,
        D3D_PRIMITIVE_TOPOLOGY topology, unsigned int vertex_count)
{
    struct vkd3d_shader_signature vs_input_signature;
    unsigned int attribute_idx, rt_count, i, j;
    struct gl_runner *runner = gl_runner(r);
    struct vkd3d_shader_code vs_dxbc;
    uint8_t *attribute_offsets[32];
    struct
    {
        GLuint id;
        GLsizei stride;
    } vbo_info[MAX_RESOURCES];
    GLuint program_id, ubo_id = 0;
    ID3D10Blob *vs_blob;
    uint32_t map;
    int ret;
    struct
    {
        GLuint id;
    } sampler_info[MAX_SAMPLERS];
    GLenum draw_buffers[8];

    program_id = compile_graphics_shader_program(runner, &vs_blob);
    todo_if(runner->r.is_todo) ok(program_id, "Failed to compile shader program.\n");
    if (!program_id)
        return false;
    glUseProgram(program_id);

    if (runner->r.uniform_count)
    {
        glGenBuffers(1, &ubo_id);
        glBindBufferBase(GL_UNIFORM_BUFFER, 0, ubo_id);
        glBufferData(GL_UNIFORM_BUFFER, runner->r.uniform_count * sizeof(*runner->r.uniforms),
                runner->r.uniforms, GL_STATIC_DRAW);
    }

    if (!runner->fbo_id)
        glGenFramebuffers(1, &runner->fbo_id);
    glBindFramebuffer(GL_FRAMEBUFFER, runner->fbo_id);

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        struct sampler *sampler = &runner->r.samplers[i];
        GLuint id;

        glGenSamplers(1, &id);
        glSamplerParameteri(id, GL_TEXTURE_WRAP_S, get_texture_wrap_gl(sampler->u_address));
        glSamplerParameteri(id, GL_TEXTURE_WRAP_T, get_texture_wrap_gl(sampler->v_address));
        glSamplerParameteri(id, GL_TEXTURE_WRAP_R, get_texture_wrap_gl(sampler->w_address));
        glSamplerParameteri(id, GL_TEXTURE_MAG_FILTER, get_texture_filter_mag_gl(sampler->filter));
        glSamplerParameteri(id, GL_TEXTURE_MIN_FILTER, get_texture_filter_min_gl(sampler->filter));
        if (sampler->func)
        {
            glSamplerParameteri(id, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
            glSamplerParameteri(id, GL_TEXTURE_COMPARE_FUNC, get_compare_op_gl(sampler->func));
        }
        sampler_info[i].id = id;
    }

    for (i = 0; i < runner->combined_sampler_count; ++i)
    {
        const struct vkd3d_shader_combined_resource_sampler *s = &runner->combined_samplers[i];
        struct resource *resource;
        struct sampler *sampler;

        if (s->resource_space || s->sampler_space)
            fatal_error("Unsupported register space.\n");

        if (!(resource = shader_runner_get_resource(r, RESOURCE_TYPE_TEXTURE, s->resource_index)))
            fatal_error("Resource not found.\n");

        if (resource->dimension == RESOURCE_DIMENSION_BUFFER)
        {
            glActiveTexture(GL_TEXTURE0 + s->binding.binding);
            glBindTexture(GL_TEXTURE_BUFFER, gl_resource(resource)->tbo_id);
        }
        else
        {
            glActiveTexture(GL_TEXTURE0 + s->binding.binding);
            glBindTexture(GL_TEXTURE_2D, gl_resource(resource)->id);
        }

        if (s->sampler_index == VKD3D_SHADER_DUMMY_SAMPLER_INDEX)
            continue;

        if (!(sampler = shader_runner_get_sampler(r, s->sampler_index)))
            fatal_error("Sampler not found.\n");
        glBindSampler(s->binding.binding, sampler_info[sampler - r->samplers].id);
    }

    memset(vbo_info, 0, sizeof(vbo_info));
    memset(draw_buffers, 0, sizeof(draw_buffers));
    for (i = 0, rt_count = 0; i < runner->r.resource_count; ++i)
    {
        struct gl_resource *resource = gl_resource(runner->r.resources[i]);

        switch (resource->r.type)
        {
            case RESOURCE_TYPE_RENDER_TARGET:
                glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + resource->r.slot, resource->id, 0);
                if (resource->r.slot >= ARRAY_SIZE(draw_buffers))
                    fatal_error("Unsupported render target index %u.\n", resource->r.slot);
                draw_buffers[resource->r.slot] = GL_COLOR_ATTACHMENT0 + resource->r.slot;
                if (resource->r.slot >= rt_count)
                    rt_count = resource->r.slot + 1;
                break;

            case RESOURCE_TYPE_DEPTH_STENCIL:
                glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, resource->id, 0);
                glEnable(GL_DEPTH_TEST);
                glDepthMask(GL_TRUE);
                glDepthFunc(get_compare_op_gl(runner->r.depth_func));
                break;

            case RESOURCE_TYPE_TEXTURE:
                break;

            case RESOURCE_TYPE_UAV:
                if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
                {
                    glBindImageTexture(resource->r.slot, resource->tbo_id, 0, GL_TRUE,
                            0, GL_READ_WRITE, resource->format->internal_format);
                }
                else
                {
                    glBindImageTexture(resource->r.slot, resource->id, 0, GL_TRUE,
                            0, GL_READ_WRITE, resource->format->internal_format);
                }
                break;

            case RESOURCE_TYPE_VERTEX_BUFFER:
                assert(resource->r.slot < ARRAY_SIZE(vbo_info));
                vbo_info[resource->r.slot].id = resource->id;
                for (j = 0; j < runner->r.input_element_count; ++j)
                {
                    if (runner->r.input_elements[j].slot != resource->r.slot)
                        continue;
                    assert(j < ARRAY_SIZE(attribute_offsets));
                    attribute_offsets[j] = (uint8_t *)(uintptr_t)vbo_info[resource->r.slot].stride;
                    vbo_info[resource->r.slot].stride += runner->r.input_elements[j].texel_size;
                }
                break;
        }
    }

    glViewport(0, 0, RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT);
    glScissor(0, 0, RENDER_TARGET_WIDTH, RENDER_TARGET_HEIGHT);
    glDrawBuffers(rt_count, draw_buffers);

    vs_dxbc.code = ID3D10Blob_GetBufferPointer(vs_blob);
    vs_dxbc.size = ID3D10Blob_GetBufferSize(vs_blob);
    ret = vkd3d_shader_parse_input_signature(&vs_dxbc, &vs_input_signature, NULL);
    ok(!ret, "Failed to parse input signature, error %d.\n", ret);

    map = runner->attribute_map;
    for (i = 0, runner->attribute_map = 0; i < runner->r.input_element_count; ++i)
    {
        const struct input_element *element = &runner->r.input_elements[i];
        const struct vkd3d_shader_signature_element *signature_element;
        const struct format_info *format;

        signature_element = vkd3d_shader_find_signature_element(&vs_input_signature,
                element->name, element->index, 0);
        ok(signature_element, "Cannot find signature element %s%u.\n", element->name, element->index);
        attribute_idx = signature_element->register_index;
        format = get_format_info(element->format, false);

        glBindBuffer(GL_ARRAY_BUFFER, vbo_info[element->slot].id);
        if (format->is_integer)
            glVertexAttribIPointer(attribute_idx, format->component_count, format->type,
                    vbo_info[element->slot].stride, attribute_offsets[i]);
        else
            glVertexAttribPointer(attribute_idx, format->component_count, format->type,
                    GL_FALSE, vbo_info[element->slot].stride, attribute_offsets[i]);
        glEnableVertexAttribArray(attribute_idx);
        runner->attribute_map |= attribute_idx;
    }
    vkd3d_shader_free_shader_signature(&vs_input_signature);
    map &= ~runner->attribute_map;
    for (attribute_idx = 0; map; ++attribute_idx, map >>= 1)
    {
        if (map & 1)
            glDisableVertexAttribArray(attribute_idx);
    }

    glDrawArrays(get_topology_gl(topology), 0, vertex_count);

    for (i = 0; i < runner->r.sampler_count; ++i)
    {
        glDeleteSamplers(1, &sampler_info[i].id);
    }
    glDeleteBuffers(1, &ubo_id);

    ID3D10Blob_Release(vs_blob);
    glDeleteProgram(program_id);

    return true;
}

struct gl_resource_readback
{
    struct resource_readback rb;
};

static struct resource_readback *gl_runner_get_resource_readback(struct shader_runner *r, struct resource *res)
{
    struct gl_resource *resource = gl_resource(res);
    struct resource_readback *rb;

    if (resource->r.type != RESOURCE_TYPE_RENDER_TARGET && resource->r.type != RESOURCE_TYPE_DEPTH_STENCIL
            && resource->r.type != RESOURCE_TYPE_UAV)
        fatal_error("Unhandled resource type %#x.\n", resource->r.type);

    rb = malloc(sizeof(*rb));

    rb->width = resource->r.width;
    rb->height = resource->r.height;
    rb->depth = 1;

    rb->row_pitch = rb->width * resource->r.texel_size;
    rb->data = malloc(rb->row_pitch * rb->height);

    if (resource->r.dimension == RESOURCE_DIMENSION_BUFFER)
    {
        glBindBuffer(GL_TEXTURE_BUFFER, resource->id);
        glGetBufferSubData(GL_TEXTURE_BUFFER, 0, rb->row_pitch * rb->height, rb->data);
    }
    else
    {
        glBindTexture(GL_TEXTURE_2D, resource->id);
        glGetTexImage(GL_TEXTURE_2D, 0, resource->format->format, resource->format->type, rb->data);
    }

    return rb;
}

static void gl_runner_release_readback(struct shader_runner *runner, struct resource_readback *rb)
{
    free(rb->data);
    free(rb);
}

static const struct shader_runner_ops gl_runner_ops =
{
    .create_resource = gl_runner_create_resource,
    .destroy_resource = gl_runner_destroy_resource,
    .dispatch = gl_runner_dispatch,
    .clear = gl_runner_clear,
    .draw = gl_runner_draw,
    .get_resource_readback = gl_runner_get_resource_readback,
    .release_readback = gl_runner_release_readback,
};

static void run_tests(enum shading_language language)
{
    struct gl_runner runner;

    if (!gl_runner_init(&runner, language))
        return;
    run_shader_tests(&runner.r, &runner.caps, &gl_runner_ops, NULL);
    gl_runner_cleanup(&runner);
}

void run_shader_tests_gl(void)
{
    const char *test_name;

    test_name = vkd3d_test_name;
    vkd3d_test_name = "shader_runner_gl";
    run_tests(SPIR_V);
    run_tests(GLSL);
    vkd3d_test_name = test_name;
}

#endif
