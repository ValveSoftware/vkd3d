ACLOCAL_AMFLAGS = -I m4
AM_CFLAGS = @VKD3D_CFLAGS@
AM_CPPFLAGS = -I$(srcdir)/include -I$(srcdir)/include/private -I$(builddir)/include/private
AM_LDFLAGS = -no-undefined

widl_headers = \
	include/vkd3d_d3d12.h \
	include/vkd3d_d3d12sdklayers.h \
	include/vkd3d_d3dcommon.h \
	include/vkd3d_d3dx9shader.h \
	include/vkd3d_dxgi.h \
	include/vkd3d_dxgi1_2.h \
	include/vkd3d_dxgi1_3.h \
	include/vkd3d_dxgi1_4.h \
	include/vkd3d_dxgibase.h \
	include/vkd3d_dxgiformat.h \
	include/vkd3d_dxgitype.h

vkd3d_public_headers = \
	include/vkd3d.h \
	include/vkd3d_d3d12.h \
	include/vkd3d_d3d12sdklayers.h \
	include/vkd3d_d3d9types.h \
	include/vkd3d_d3dcommon.h \
	include/vkd3d_d3dcompiler.h \
	include/vkd3d_d3dx9shader.h \
	include/vkd3d_dxgibase.h \
	include/vkd3d_dxgiformat.h \
	include/vkd3d_shader.h \
	include/vkd3d_types.h \
	include/vkd3d_utils.h \
	include/vkd3d_windows.h

vkd3d_demos_shaders = \
	demos/gears.hlsl \
	demos/gears_ps_flat.h \
	demos/gears_ps_smooth.h \
	demos/gears_vs.h \
	demos/triangle.hlsl \
	demos/triangle_ps.h \
	demos/triangle_vs.h

vkd3d_tests = \
	tests/vkd3d_api \
	tests/vkd3d_common \
	tests/vkd3d_shader_api

vkd3d_cross_tests = \
	tests/d3d12 \
	tests/d3d12_invalid_usage \
	tests/hlsl_d3d12

vkd3d_shader_runners = \
	tests/shader_runner_d3d12

vkd3d_shader_tests = \
	tests/abs.shader_test \
	tests/cast-to-float.shader_test \
	tests/cast-to-half.shader_test \
	tests/cast-to-int.shader_test \
	tests/cast-to-uint.shader_test \
	tests/compute.shader_test \
	tests/conditional.shader_test \
	tests/hlsl-array-dimension.shader_test \
	tests/hlsl-attributes.shader_test \
	tests/hlsl-bool-cast.shader_test \
	tests/hlsl-clamp.shader_test \
	tests/hlsl-comma.shader_test \
	tests/hlsl-duplicate-modifiers.shader_test \
	tests/hlsl-entry-point-overload.shader_test \
	tests/hlsl-entry-point-semantics.shader_test \
	tests/hlsl-for.shader_test \
	tests/hlsl-function.shader_test \
	tests/hlsl-function-cast.shader_test \
	tests/hlsl-function-overload.shader_test \
	tests/hlsl-function-return.shader_test \
	tests/hlsl-intrinsic-override.shader_test \
	tests/hlsl-invalid.shader_test \
	tests/hlsl-majority-pragma.shader_test \
	tests/hlsl-majority-typedef.shader_test \
	tests/hlsl-mul.shader_test \
	tests/hlsl-nested-arrays.shader_test \
	tests/hlsl-numthreads.shader_test \
	tests/hlsl-operations.shader_test \
	tests/hlsl-return.shader_test \
	tests/hlsl-return-implicit-conversion.shader_test \
	tests/hlsl-shape.shader_test \
	tests/hlsl-state-block-syntax.shader_test \
	tests/hlsl-static-initializer.shader_test \
	tests/hlsl-storage-qualifiers.shader_test \
	tests/hlsl-struct-assignment.shader_test \
	tests/hlsl-struct-semantics.shader_test \
	tests/hlsl-uav-out-param.shader_test \
	tests/hlsl-vector-indexing.shader_test \
	tests/hlsl-vector-indexing-uniform.shader_test \
	tests/math.shader_test \
	tests/max.shader_test \
	tests/pow.shader_test \
	tests/preproc-if.shader_test \
	tests/preproc-ifdef.shader_test \
	tests/preproc-if-expr.shader_test \
	tests/preproc-invalid.shader_test \
	tests/preproc-macro.shader_test \
	tests/preproc-misc.shader_test \
	tests/sampler.shader_test \
	tests/saturate.shader_test \
	tests/swizzle-0.shader_test \
	tests/swizzle-1.shader_test \
	tests/swizzle-2.shader_test \
	tests/swizzle-3.shader_test \
	tests/swizzle-4.shader_test \
	tests/swizzle-5.shader_test \
	tests/swizzle-6.shader_test \
	tests/swizzle-7.shader_test \
	tests/texture-load.shader_test \
	tests/texture-load-typed.shader_test \
	tests/trigonometry.shader_test \
	tests/uav.shader_test \
	tests/uav-load.shader_test \
	tests/writemask-assignop-0.shader_test \
	tests/writemask-assignop-1.shader_test \
	tests/writemask-assignop-2.shader_test \
	tests/writemask-assignop-3.shader_test

vkd3d_test_headers = \
	tests/d3d12_crosstest.h \
	tests/d3d12_test_utils.h

vkd3d_demos = \
	demos/vkd3d-gears \
	demos/vkd3d-triangle

vkd3d_demos_headers = \
	demos/demo.h \
	demos/demo_win32.h \
	demos/demo_xcb.h

BUILT_SOURCES = $(widl_headers) \
	include/private/vkd3d_version.h

noinst_LTLIBRARIES = libvkd3d-common.la
libvkd3d_common_la_SOURCES = \
	include/private/vkd3d_debug.h \
	libs/vkd3d-common/blob.c \
	libs/vkd3d-common/debug.c \
	libs/vkd3d-common/error.c \
	libs/vkd3d-common/memory.c \
	libs/vkd3d-common/utf8.c

lib_LTLIBRARIES = libvkd3d-shader.la libvkd3d.la libvkd3d-utils.la

VKD3D_V_FLEX = $(vkd3d_v_flex_@AM_V@)
vkd3d_v_flex_ = $(vkd3d_v_flex_@AM_DEFAULT_V@)
vkd3d_v_flex_0 = @echo "  FLEX    " $@;
vkd3d_v_flex_1 =

VKD3D_V_BISON = $(vkd3d_v_bison_@AM_V@)
vkd3d_v_bison_ = $(vkd3d_v_bison_@AM_DEFAULT_V@)
vkd3d_v_bison_0 = @echo "  BISON   " $@;
vkd3d_v_bison_1 =

libs/vkd3d-shader/preproc.yy.c: libs/vkd3d-shader/preproc.l
	@$(MKDIR_P) libs/vkd3d-shader
	$(VKD3D_V_FLEX)$(FLEX) $(LFLAGS) -o $@ $<

libs/vkd3d-shader/preproc.tab.c libs/vkd3d-shader/preproc.tab.h &: libs/vkd3d-shader/preproc.y
	@$(MKDIR_P) libs/vkd3d-shader
	$(VKD3D_V_BISON)$(BISON) $(YFLAGS) -d -o libs/vkd3d-shader/preproc.tab.c $<

libs/vkd3d-shader/hlsl.yy.c: $(srcdir)/libs/vkd3d-shader/hlsl.l
	@$(MKDIR_P) libs/vkd3d-shader
	$(VKD3D_V_FLEX)$(FLEX) $(LFLAGS) -o $@ $<

libs/vkd3d-shader/hlsl.tab.c libs/vkd3d-shader/hlsl.tab.h &: libs/vkd3d-shader/hlsl.y
	@$(MKDIR_P) libs/vkd3d-shader
	$(VKD3D_V_BISON)$(BISON) $(YFLAGS) -d -o libs/vkd3d-shader/hlsl.tab.c $<

BUILT_SOURCES += \
	libs/vkd3d-shader/hlsl.tab.h \
	libs/vkd3d-shader/preproc.tab.h

vkd3d_shader_yyfiles = \
	libs/vkd3d-shader/hlsl.tab.c \
	libs/vkd3d-shader/hlsl.tab.h \
	libs/vkd3d-shader/hlsl.yy.c \
	libs/vkd3d-shader/preproc.tab.c \
	libs/vkd3d-shader/preproc.tab.h \
	libs/vkd3d-shader/preproc.yy.c

CLEANFILES = $(vkd3d_shader_yyfiles)

nodist_libvkd3d_shader_la_SOURCES = $(vkd3d_shader_yyfiles)

libvkd3d_shader_la_SOURCES = \
	include/private/list.h \
	include/private/rbtree.h \
	include/private/vkd3d_common.h \
	include/private/vkd3d_debug.h \
	include/private/vkd3d_memory.h \
	include/vkd3d_shader.h \
	libs/vkd3d-shader/checksum.c \
	libs/vkd3d-shader/dxbc.c \
	libs/vkd3d-shader/glsl.c \
	libs/vkd3d-shader/hlsl.c \
	libs/vkd3d-shader/hlsl.h \
	libs/vkd3d-shader/hlsl_codegen.c \
	libs/vkd3d-shader/hlsl_sm1.c \
	libs/vkd3d-shader/hlsl_sm4.c \
	libs/vkd3d-shader/preproc.h \
	libs/vkd3d-shader/sm4.h \
	libs/vkd3d-shader/spirv.c \
	libs/vkd3d-shader/trace.c \
	libs/vkd3d-shader/vkd3d_shader.map \
	libs/vkd3d-shader/vkd3d_shader_main.c \
	libs/vkd3d-shader/vkd3d_shader_private.h
libvkd3d_shader_la_CFLAGS = $(AM_CFLAGS) -DLIBVKD3D_SHADER_SOURCE -I$(srcdir)/libs/vkd3d-shader @SPIRV_TOOLS_CFLAGS@
libvkd3d_shader_la_LDFLAGS = $(AM_LDFLAGS) -version-info 1:0:0
libvkd3d_shader_la_LIBADD = libvkd3d-common.la @SPIRV_TOOLS_LIBS@ -lm
if HAVE_LD_VERSION_SCRIPT
libvkd3d_shader_la_LDFLAGS += -Wl,--version-script=$(srcdir)/libs/vkd3d-shader/vkd3d_shader.map
EXTRA_libvkd3d_shader_la_DEPENDENCIES = $(srcdir)/libs/vkd3d-shader/vkd3d_shader.map
endif

libvkd3d_la_SOURCES = \
	include/private/vkd3d_blob.h \
	include/private/vkd3d_common.h \
	include/private/vkd3d_debug.h \
	include/private/vkd3d_memory.h \
	include/private/vkd3d_utf8.h \
	include/private/vkd3d_test.h \
	include/vkd3d_d3d12.idl \
	include/vkd3d_d3dcommon.idl \
	include/vkd3d_unknown.idl \
	libs/vkd3d/command.c \
	libs/vkd3d/device.c \
	libs/vkd3d/resource.c \
	libs/vkd3d/state.c \
	libs/vkd3d/utils.c \
	libs/vkd3d/vkd3d.map \
	libs/vkd3d/vkd3d_main.c \
	libs/vkd3d/vkd3d_private.h \
	libs/vkd3d/vkd3d_shaders.h \
	libs/vkd3d/vulkan_procs.h
libvkd3d_la_CFLAGS = $(AM_CFLAGS) -DLIBVKD3D_SOURCE
libvkd3d_la_LDFLAGS = $(AM_LDFLAGS) -version-info 3:0:2
libvkd3d_la_LIBADD = libvkd3d-common.la libvkd3d-shader.la @DL_LIBS@ @PTHREAD_LIBS@
if HAVE_LD_VERSION_SCRIPT
libvkd3d_la_LDFLAGS += -Wl,--version-script=$(srcdir)/libs/vkd3d/vkd3d.map
EXTRA_libvkd3d_la_DEPENDENCIES = $(srcdir)/libs/vkd3d/vkd3d.map
endif

libvkd3d_utils_la_SOURCES = \
	libs/vkd3d-utils/vkd3d_utils.map \
	libs/vkd3d-utils/vkd3d_utils_main.c \
	libs/vkd3d-utils/vkd3d_utils_private.h
libvkd3d_utils_la_CFLAGS = $(AM_CFLAGS) -DLIBVKD3D_UTILS_SOURCE
libvkd3d_utils_la_LDFLAGS = $(AM_LDFLAGS) -version-info 2:0:1
libvkd3d_utils_la_LIBADD = libvkd3d-common.la libvkd3d-shader.la libvkd3d.la @PTHREAD_LIBS@
if HAVE_LD_VERSION_SCRIPT
libvkd3d_utils_la_LDFLAGS += -Wl,--version-script=$(srcdir)/libs/vkd3d-utils/vkd3d_utils.map
EXTRA_libvkd3d_utils_la_DEPENDENCIES = $(srcdir)/libs/vkd3d-utils/vkd3d_utils.map
endif

EXTRA_DIST = ANNOUNCE LICENSE

pkgconfigdir = $(libdir)/pkgconfig
pkginclude_HEADERS = $(vkd3d_public_headers)
nodist_pkgconfig_DATA = libvkd3d.pc libvkd3d-shader.pc libvkd3d-utils.pc
CLEANFILES += libvkd3d.pc libvkd3d-shader.pc libvkd3d-utils.pc
EXTRA_DIST += \
	libs/vkd3d/libvkd3d.pc.in \
	libs/vkd3d-shader/libvkd3d-shader.pc.in \
	libs/vkd3d-utils/libvkd3d-utils.pc.in

bin_PROGRAMS = vkd3d-compiler
vkd3d_compiler_SOURCES = programs/vkd3d-compiler/main.c
vkd3d_compiler_CFLAGS = $(AM_CFLAGS) @NCURSES_CFLAGS@
vkd3d_compiler_LDADD = libvkd3d-shader.la @NCURSES_LIBS@

LDADD = libvkd3d.la libvkd3d-utils.la
AM_DEFAULT_SOURCE_EXT = .c

TEST_EXTENSIONS = .shader_test

if BUILD_TESTS
check_PROGRAMS = $(vkd3d_tests) $(vkd3d_cross_tests) $(vkd3d_shader_runners)
TESTS = $(vkd3d_tests) $(vkd3d_cross_tests) $(vkd3d_shader_tests)
tests_d3d12_LDADD = $(LDADD) @PTHREAD_LIBS@ @VULKAN_LIBS@
tests_d3d12_invalid_usage_LDADD = $(LDADD) @VULKAN_LIBS@
tests_hlsl_d3d12_LDADD = $(LDADD) @VULKAN_LIBS@
tests_shader_runner_d3d12_LDADD = $(LDADD) @VULKAN_LIBS@
tests_vkd3d_api_LDADD = libvkd3d.la @VULKAN_LIBS@
tests_vkd3d_shader_api_LDADD = libvkd3d-shader.la
SHADER_TEST_LOG_COMPILER = tests/shader_runner_d3d12
XFAIL_TESTS = \
	tests/cast-to-float.shader_test \
	tests/cast-to-half.shader_test \
	tests/cast-to-int.shader_test \
	tests/cast-to-uint.shader_test \
	tests/hlsl-array-dimension.shader_test \
	tests/hlsl-attributes.shader_test \
	tests/hlsl-duplicate-modifiers.shader_test \
	tests/hlsl-entry-point-overload.shader_test \
	tests/hlsl-for.shader_test \
	tests/hlsl-function-cast.shader_test \
	tests/hlsl-function-overload.shader_test \
	tests/hlsl-function-return.shader_test \
	tests/hlsl-intrinsic-override.shader_test \
	tests/hlsl-majority-pragma.shader_test \
	tests/hlsl-majority-typedef.shader_test \
	tests/hlsl-mul.shader_test \
	tests/hlsl-nested-arrays.shader_test \
	tests/hlsl-numthreads.shader_test \
	tests/hlsl-operations.shader_test \
	tests/hlsl-return.shader_test \
	tests/hlsl-return-implicit-conversion.shader_test \
	tests/hlsl-shape.shader_test \
	tests/hlsl-static-initializer.shader_test \
	tests/hlsl-storage-qualifiers.shader_test \
	tests/hlsl-uav-out-param.shader_test \
	tests/hlsl-vector-indexing.shader_test \
	tests/hlsl-vector-indexing-uniform.shader_test \
	tests/max.shader_test \
	tests/trigonometry.shader_test
endif

if BUILD_DEMOS
DEMOS_LDADD = $(LDADD) libvkd3d-shader.la @XCB_LIBS@ @VULKAN_LIBS@
DEMOS_CFLAGS = $(AM_CFLAGS) @XCB_CFLAGS@
bin_PROGRAMS += $(vkd3d_demos)

demos_vkd3d_gears_SOURCES = demos/gears.c
demos_vkd3d_gears_CFLAGS = $(DEMOS_CFLAGS)
demos_vkd3d_gears_LDADD = $(DEMOS_LDADD) -lm

demos_vkd3d_triangle_SOURCES = demos/triangle.c
demos_vkd3d_triangle_CFLAGS = $(DEMOS_CFLAGS)
demos_vkd3d_triangle_LDADD = $(DEMOS_LDADD)
endif

noinst_HEADERS = $(vkd3d_test_headers) $(vkd3d_demos_headers)

VKD3D_V_WIDL = $(vkd3d_v_widl_@AM_V@)
vkd3d_v_widl_ = $(vkd3d_v_widl_@AM_DEFAULT_V@)
vkd3d_v_widl_0 = @echo "  WIDL    " $@;
vkd3d_v_widl_1 =

if HAVE_WIDL
CLEANFILES += $(widl_headers)
endif
EXTRA_DIST += $(widl_headers) $(widl_headers:.h=.idl)
$(widl_headers): %.h: %.idl
if HAVE_WIDL
	$(VKD3D_V_WIDL)$(WIDL) -h -o $@ $<
else
	@echo "widl is required to generate $@"
endif

EXTRA_DIST += $(vkd3d_demos_shaders)

libvkd3d-utils.pc: $(srcdir)/libs/vkd3d-utils/libvkd3d-utils.pc.in Makefile
	$(AM_V_GEN)$(SED) -e 's![@]prefix[@]!$(prefix)!g' \
		-e 's![@]exec_prefix[@]!$(exec_prefix)!g' \
		-e 's![@]includedir[@]!$(includedir)!g' \
		-e 's![@]libdir[@]!$(libdir)!g' \
		-e 's![@]PACKAGE_VERSION[@]!$(PACKAGE_VERSION)!g' \
		$< > $@

libvkd3d-shader.pc: $(srcdir)/libs/vkd3d-shader/libvkd3d-shader.pc.in Makefile
	$(AM_V_GEN)$(SED) -e 's![@]prefix[@]!$(prefix)!g' \
		-e 's![@]exec_prefix[@]!$(exec_prefix)!g' \
		-e 's![@]includedir[@]!$(includedir)!g' \
		-e 's![@]libdir[@]!$(libdir)!g' \
		-e 's![@]PACKAGE_VERSION[@]!$(PACKAGE_VERSION)!g' \
		$< > $@

libvkd3d.pc: $(srcdir)/libs/vkd3d/libvkd3d.pc.in Makefile
	$(AM_V_GEN)$(SED) -e 's![@]prefix[@]!$(prefix)!g' \
		-e 's![@]exec_prefix[@]!$(exec_prefix)!g' \
		-e 's![@]includedir[@]!$(includedir)!g' \
		-e 's![@]libdir[@]!$(libdir)!g' \
		-e 's![@]PACKAGE_VERSION[@]!$(PACKAGE_VERSION)!g' \
		$< > $@

include/private/vkd3d_version.h: dummy-vkd3d-version
	@$(MKDIR_P) include/private
	version=`(GIT_DIR=$(top_srcdir)/.git git rev-parse --short HEAD 2>/dev/null || echo '') \
		| $(SED) -e 's!\(..*\)! (git \1)!' \
		| $(SED) -n -e '$$s!\(.*\)!#define VKD3D_VCS_ID "\1"!p'` \
		&& (echo $$version | cmp -s - $@) \
		|| echo $$version >$@ || ($(RM) $@ && exit 1)
.SILENT: include/private/vkd3d_version.h
CLEANFILES += include/private/vkd3d_version.h

.PHONY: dummy-vkd3d-version
dummy-vkd3d-version:

## Cross-compile tests
cross_implibs = crosslibs/d3d12
CROSS_CPPFLAGS = -I$(srcdir)/include -I$(srcdir)/include/private -I$(builddir)/include
CROSS_CFLAGS = -g -O2 -Wall -municode ${CROSS_CPPFLAGS}
EXTRA_DIST += $(cross_implibs:=.cross32.def) $(cross_implibs:=.cross64.def)

if HAVE_CROSSTARGET32
CROSS32_CC = @CROSSCC32@
CROSS32_DLLTOOL = @CROSSTARGET32@-dlltool
CROSS32_IMPLIBS = $(cross_implibs:=.cross32.a)
CROSS32_EXEFILES = $(vkd3d_cross_tests:=.cross32.exe) \
	$(vkd3d_demos:demos/vkd3d-%=demos/%.cross32.exe) \
	$(vkd3d_shader_runners:=.cross32.exe)
CROSS32_FILES = $(CROSS32_IMPLIBS) $(CROSS32_EXEFILES)

CLEANFILES += $(CROSS32_FILES)
crosstest32: $(CROSS32_FILES)

-include tests/$(DEPDIR)/*.cross32.Po
-include demos/$(DEPDIR)/*.cross32.Po

$(CROSS32_IMPLIBS): %.cross32.a: %.cross32.def
	@${MKDIR_P} crosslibs
	$(AM_V_GEN)$(CROSS32_DLLTOOL) -k -m i386 --as-flags=-32 -d $< -l $@

$(CROSS32_EXEFILES): %.cross32.exe: %.c $(CROSS32_IMPLIBS) $(widl_headers)
	$(AM_V_CCLD)depbase=`echo $@ | $(SED) 's![^/]*$$!$(DEPDIR)/&!;s!\.exe$$!!'`; \
	$(CROSS32_CC) $(CROSS_CFLAGS) -MT $@ -MD -MP -MF $$depbase.Tpo -o $@ $< $(CROSS32_IMPLIBS) -ldxgi -lgdi32 -ld3dcompiler_47 && \
	$(am__mv) $$depbase.Tpo $$depbase.Po
else
crosstest32:
endif

if HAVE_CROSSTARGET64
CROSS64_CC = @CROSSCC64@
CROSS64_DLLTOOL = @CROSSTARGET64@-dlltool
CROSS64_IMPLIBS = $(cross_implibs:=.cross64.a)
CROSS64_EXEFILES = $(vkd3d_cross_tests:=.cross64.exe) \
	$(vkd3d_demos:demos/vkd3d-%=demos/%.cross64.exe) \
	$(vkd3d_shader_runners:=.cross64.exe)
CROSS64_FILES = $(CROSS64_IMPLIBS) $(CROSS64_EXEFILES)

CLEANFILES += $(CROSS64_FILES)
crosstest64: $(CROSS64_FILES)

-include tests/$(DEPDIR)/*.cross64.Po
-include demos/$(DEPDIR)/*.cross64.Po

$(CROSS64_IMPLIBS): %.cross64.a: %.cross64.def
	@${MKDIR_P} crosslibs
	$(AM_V_GEN)$(CROSS64_DLLTOOL) -k -m i386:x86-64 --as-flags=-64 -d $< -l $@

$(CROSS64_EXEFILES): %.cross64.exe: %.c $(CROSS64_IMPLIBS) $(widl_headers)
	$(AM_V_CCLD)depbase=`echo $@ | sed 's![^/]*$$!$(DEPDIR)/&!;s!\.exe$$!!'`; \
	$(CROSS64_CC) $(CROSS_CFLAGS) -MT $@ -MD -MP -MF $$depbase.Tpo -o $@ $< $(CROSS64_IMPLIBS) -ldxgi -lgdi32 -ld3dcompiler_47 && \
	$(am__mv) $$depbase.Tpo $$depbase.Po
else
crosstest64:
endif

.PHONY: crosstest crosstest32 crosstest64
crosstest: crosstest32 crosstest64

if BUILD_DOC
@DX_RULES@
all: doxygen-doc
CLEANFILES += $(DX_CLEANFILES)
endif
