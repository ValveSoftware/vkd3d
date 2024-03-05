# What's new in vkd3d 1.11 (5 Mar 2024)

### libvkd3d

  - Descriptor updates happen asynchronously on an internal worker thread, for
    a minor performance improvement in applications that update many
    descriptors per frame.

  - When the VK_EXT_mutable_descriptor_type extension is available, libvkd3d
    will make more efficient use of descriptor pools and sets.

  - When the VK_EXT_shader_viewport_index_layer extension is available,
    libvkd3d supports indexing viewport and render target arrays from vertex
    and tessellation evaluation shaders.

  - Support for standard (i.e., black and white) border colours is
    implemented.

  - The GetResourceAllocationInfo1() method of the ID3D12Device4 interface is
    implemented.

  - The ID3D12Device7 interface is supported.

  - The ID3D12Resource2 interface is supported.

  - Several new feature queries are supported:
    - D3D12_FEATURE_D3D12_OPTIONS6
    - D3D12_FEATURE_D3D12_OPTIONS7
    - D3D12_FEATURE_D3D12_OPTIONS8
    - D3D12_FEATURE_D3D12_OPTIONS9
    - D3D12_FEATURE_D3D12_OPTIONS10
    - D3D12_FEATURE_D3D12_OPTIONS11
    - D3D12_FEATURE_D3D12_OPTIONS12
    - D3D12_FEATURE_D3D12_OPTIONS13

### libvkd3d-shader

  - Initial support for compiling legacy Direct3D bytecode to SPIR-V.

  - Experimental support for compiling DirectX Intermediate Language (DXIL) to
    SPIR-V and Direct3D shader assembly. Being an experimental feature, this
    requires building vkd3d with the ‘-DVKD3D_SHADER_UNSUPPORTED_DXIL’
    preprocessor option. Note that enabling this feature will affect the
    capabilities reported by libvkd3d as well, and may cause previously
    working applications to break due to attempting to use incomplete DXIL
    support. No API or ABI stability guarantees are provided for experimental
    features.

  - New features for the HLSL source type:
    - Initial support for the ‘fx_2_0’, ‘fx_4_0’, ‘fx_4_1’, and ‘fx_5_0’
      profiles, using the new ‘VKD3D_SHADER_TARGET_FX’ target type.
    - Support for ‘Buffer’ resources.
    - The acos(), asin(), atan(), and atan2() intrinsic functions are
      supported.
    - Explicit register assignment using the ‘register()’ keyword in shader
      model 1-3 profiles. This was previously only supported in shader model
      4+ profiles.
    - Casts from integer to floating-point types in shader model 1-3 profiles.
    - Support for various input/output semantics:
      - SV_InstanceID in shader model 4+ fragment shaders.
      - SV_PrimitiveID in shader model 4+ fragment shaders. In previous
        versions this was only supported in shader model 4+ geometry shaders.
      - SV_RenderTargetArrayIndex in shader model 4+ vertex and fragment shaders.
      - SV_ViewportArrayIndex in shader model 4+ vertex and fragment shaders.
    - Support for various rasteriser-ordered view types. Specifically:
      - RasterizerOrderedBuffer
      - RasterizerOrderedStructuredBuffer
      - RasterizerOrderedTexture1D
      - RasterizerOrderedTexture1DArray
      - RasterizerOrderedTexture2D
      - RasterizerOrderedTexture2DArray
      - RasterizerOrderedTexture3D

  - New features for the SPIR-V target type:
    - Support for globally coherent unordered access views. These have the
      ‘globallycoherent’ storage class in HLSL, and the ‘_glc’ suffix in
      Direct3D assembly.
    - Support for thread group unordered access view barriers. This
      corresponds to ‘sync_ugroup’ instructions in Direct3D assembly.
    - When the SPV_EXT_viewport_index_layer extension is supported, vertex and
      tessellation evaluation shaders can write render target and viewport
      array indices. This corresponds to the ‘SV_RenderTargetArrayIndex’ and
      ‘SV_ViewportArrayIndex’ HLSL output semantics.

  - New interfaces:
    - The VKD3D_SHADER_COMPILE_OPTION_FEATURE compile option can be used to
      specify features available in the target environment. The
      VKD3D_SHADER_COMPILE_OPTION_FEATURE_INT64 flag indicates support for
      64-bit integer types in the SPIR-V target environment. The
      VKD3D_SHADER_COMPILE_OPTION_FEATURE_FLOAT64 flag indicates support for
      64-bit floating-point types in the SPIR-V target environment. For
      backward compatibility, VKD3D_SHADER_API_VERSION_1_10 and earlier also
      imply support for 64-bit floating-point types.
    - The VKD3D_SHADER_SPIRV_EXTENSION_EXT_VIEWPORT_INDEX_LAYER enumeration
      value indicates support for the SPV_EXT_viewport_index_layer extension
      in the SPIR-V target environment.

### libvkd3d-utils

  - When available, the following Vulkan extensions are enabled by
    D3D12CreateDeviceVKD3D() and D3D12CreateDevice():
    - VK_KHR_android_surface
    - VK_KHR_wayland_surface
    - VK_KHR_win32_surface
    - VK_KHR_xlib_surface
    - VK_EXT_metal_surface
    - VK_MVK_ios_surface

    Previous versions of vkd3d-utils enabled VK_KHR_xcb_surface and
    VK_MVK_macos_surface. In practice this means that
    D3D12CreateDevice()/D3D12CreateDeviceVKD3D() can be used on the
    corresponding additional window systems.

  - New interfaces:
    - D3DReflect() is used to retrieve information about shaders. It currently
      supports retrieving information about input, output, and patch constant
      parameters using the ID3D12ShaderReflection interface.
    - D3DDisassemble() is used to disassemble legacy Direct3D bytecode (shader
      model 1-3) and ‘Tokenized Program Format’ (shader model 4 and 5)
      shaders.

### vkd3d-compiler

  - The new ‘fx’ target is used for outputting Direct3D effects when compiling
    HLSL ‘fx_2_0’, ‘fx_4_0’, ‘fx_4_1’, and ‘fx_5_0’ profiles.

### build

  - The minimum required version of Vulkan-Headers for this release is version
    1.3.228.

# What's new in vkd3d 1.10 (6 Dec 2023)

### libvkd3d

  - Creating pipeline state objects from pipeline state stream descriptions is
    implemented.
  - Depth-bounds testing is implemented.
  - When the VK_KHR_maintenance2 extension is available, libvkd3d will
    explicitly specify the usage flags of Vulkan image views. This is
    particularly useful on MoltenVK, where 2D-array views of 3D textures are
    subject to usage restrictions.
  - The D3D12_FORMAT_SUPPORT2_UAV_TYPED_LOAD and/or
    D3D12_FORMAT_SUPPORT2_UAV_TYPED_STORE feature flags are reported for
    UAV formats when the ‘shaderStorageImageReadWithoutFormat’ and/or
    ‘shaderStorageImageWriteWithoutFormat’ Vulkan device features are
    supported.
  - The ID3D12Device5 interface is supported.
  - The ID3D12GraphicsCommandList5 interface is supported.
  - The ID3D12Resource1 interface is supported.

### libvkd3d-shader

  - New features for the HLSL source type:
    - Support for the following intrinsic functions:
      - ceil()
      - degrees() and radians()
      - fwidth()
      - tan()
      - tex2Dlod(), tex2Dproj(), texCUBEproj(), and tex3Dproj()
    - Constant folding support for more expression types. In particular:
      - ternary operators and branches
      - reciprocal square roots
      - exponentials
      - logical ‘not’ on booleans
      - bitwise complements
      - left/right shifts
      - ceil(), floor(), frac(), and saturate()
    - Support for dynamic indexing of arrays.
    - Support for ‘break’ and ‘continue’ statements.
    - Support for ‘switch’ statements.
    - The ‘linear’, ‘centroid’, and ‘noperspective’ interpolation modifiers
      are supported.
    - The ‘RWTexture1DArray’ and ‘RWTexture2DArray’ unordered access view
      types are supported.
    - ‘\[loop\]’ attributes are accepted on loops.
    - u/U and l/L suffixes on integer constants.

  - Floating-point values are explicitly clamped to the upper and lower bounds
    of the target type by ‘ftoi’ and ‘ftou’ instructions when targeting
    SPIR-V. Similarly, NaNs are flushed to zero. Some hardware/drivers would
    already do this implicitly, but behaviour for such inputs is undefined as
    far as SPIR-V is concerned.

  - The VKD3D_SHADER_CONFIG environment variable can be used to modify the
    behaviour of libvkd3d-shader at run-time, analogous to the existing
    VKD3D_CONFIG environment variable for libvkd3d. See the README for a list
    of supported options.

  - When scanning legacy Direct3D bytecode using vkd3d_shader_scan(),
    descriptor information for shader model 2 and 3 combined resource-sampler
    pairs is returned in the vkd3d_shader_scan_descriptor_info structure.
    Note that this information is not yet available for shader model 1
    sources, although this will likely be added in a future release.

  - The Direct3D shader assembly target supports the ‘rasteriser ordered view’
    flag (‘_rov’) on unordered access view declarations.

  - New interfaces:
    - The VKD3D_SHADER_COMPILE_OPTION_BACKWARD_COMPATIBILITY compile
      option can be used to specify backward compatibility options. The
      VKD3D_SHADER_COMPILE_OPTION_BACKCOMPAT_MAP_SEMANTIC_NAMES flag is
      the only currently supported flag, and can be used to specify that
      shader model 1-3 semantic names should be mapped to their shader model
      4+ system value equivalents when compiling HLSL sources.
    - The VKD3D_SHADER_COMPILE_OPTION_FRAGMENT_COORDINATE_ORIGIN compile
      option can be used to specify the origin of fragment coordinates for
      SPIR-V targets. This is especially useful in OpenGL environments, where
      the origin may be different than in Direct3D or Vulkan environments.
    - The vkd3d_shader_scan_combined_resource_sampler_info structure
      extends the vkd3d_shader_compile_info structure, and can be used to
      retrieve information about the combined resource-sampler pairs used by a
      shader. This is especially useful when compiling shaders for usage in
      environments without separate binding points for samplers and resources,
      like OpenGL.
    - vkd3d_shader_free_scan_combined_resource_sampler_info() is used
      to free vkd3d_shader_scan_combined_resource_sampler_info
      structures.

### libvkd3d-utils

  - Passing the D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY flag to
    D3DCompile() and D3DCompile2() will enable mapping shader model 1-3
    semantic names to their shader model 4+ system value equivalents.

  - New interfaces:
    - D3DGetBlobPart() is used to retrieve specific parts of DXBC blobs.
    - D3DGetDebugInfo() is used to retrieve debug information from DXBC blobs.
    - D3DGetInputAndOutputSignatureBlob() is used to retrieve input and output
      signatures from DXBC blobs.
    - D3DGetInputSignatureBlob() is used to retrieve input signatures from
      DXBC blobs.
    - D3DGetOutputSignatureBlob() is used to retrieve output signatures from
      DXBC blobs.
    - D3DStripShader() is used to remove specific parts from DXBC blobs.

### vkd3d-compiler

  - The ‘--fragment-coordinate-origin’ option can be used to specify the
    origin of fragment coordinates for SPIR-V targets.

  - The ‘--semantic-compat-map’ option can be used to specify that shader
    model 1-3 semantic names should be mapped to their shader model 4+ system
    value equivalents when compiling HLSL sources.

### vkd3d-dxbc

  - The ‘--list’ and ‘--list-data’ options now also output the offsets of
    sections inside the input data.

### build

  - The minimum required version of Vulkan-Headers for this release is version
    1.2.148.

  - When available, the libEGL and libOpenGL libraries are used to run the
    vkd3d tests in additional configurations. These libraries are not used by
    vkd3d itself.

  - The SONAME_LIBDXCOMPILER configure variable can be used specify the
    shared object name of the dxcompiler library. When available, it's used to
    run the vkd3d tests in additional configurations. The dxcompiler library
    is not used by vkd3d itself.


# What's new in vkd3d 1.9 (21 Sep 2023)

### libvkd3d

- Copying between depth/stencil and colour formats in
  ID3D12GraphicsCommandList::CopyResource() is supported.
- The ID3D12Fence1 interface is supported.


### libvkd3d-shader

- vkd3d_shader_scan() supports retrieving descriptor information for ‘d3dbc’
  shaders. This is one of the requirements for eventual SPIR-V generation from
  ‘d3dbc’ sources.

- New features for the HLSL source type:
  - Support for the following intrinsic functions:
    - clip()
    - ddx_coarse() and ddy_coarse()
    - ddx_fine() and ddy_fine()
    - tex1D(), tex2D(), texCUBE(), and tex3D()
  - Constant folding support for more expression types. In particular:
    - comparison operators
    - floating-point min() and max()
    - logical ‘and’ and ‘or’
    - dot products
    - square roots
    - logarithms
  - Support for multi-sample texture object declarations without explicit
    sample counts in shader model 4.1 and later shaders.
  - Support for using constant expressions as sample counts in multi-sample
    texture object declarations.
  - Support for variable initialisers using variables declared earlier in the
    same declaration list. E.g., ‘float a = 1, b = a, c = b + 1;’.
  - The GetDimensions() texture object method is implemented.
  - Matrix swizzles are implemented.
  - Parser support for if-statement attributes like ‘[branch]’ and
    ‘[flatten]’.
  - Support for the ‘inline’ function modifier.

- Previously, vkd3d_shader_compile() would in some cases return VKD3D_OK
  despite compilation failing when targeting legacy Direct3D bytecode. These
  cases have been fixed.

- Various HLSL preprocessor fixes for edge cases related to stringification.

- SPIR-V target support for the ‘linear noperspective centroid’ input
  interpolation mode.

- New interfaces:
  - The vkd3d_shader_scan_signature_info structure extends the
    vkd3d_shader_compile_info structure, and can be used to retrieve
    descriptions of ‘dxbc-tpf’ and ‘d3dbc’ shader inputs and outputs.
  - vkd3d_shader_free_scan_signature_info() is used to free
    vkd3d_shader_scan_signature_info structures.
  - The VKD3D_SHADER_COMPILE_OPTION_PACK_MATRIX_ORDER compile option can be
    used to specify the default matrix packing order for HLSL sources.
  - The vkd3d_shader_varying_map_info structure extends the
    vkd3d_shader_compile_info structure, and can be used to specify a mapping
    between the outputs of a shader stage and the inputs of the next shader
    stage.
  - vkd3d_shader_build_varying_map() is used to build a mapping between the
    outputs of a shader stage and the inputs of the next shader stage.
  - The VKD3D_SHADER_DESCRIPTOR_INFO_FLAG_RAW_BUFFER flag returned as part of
    the vkd3d_shader_descriptor_info structure indicates the descriptor refers
    to a byte-addressed (‘raw’) buffer resource.


### vkd3d-compiler

- The ‘--matrix-storage-order’ option can used to specify the default matrix
  storage order for HLSL sources.


### vkd3d-dxbc

- vkd3d-dxbc is a new utility that can be used to inspect the contents of DXBC
  blobs.


# What's new in vkd3d 1.8 (22 Jun 2023)

### libvkd3d

- Performance improvements have been made to the code that handles descriptor
  updates. In some applications the improvement can be quite significant.

- Host-visible descriptor heaps are persistently mapped on creation. Some
  applications access resource data from the CPU after calling Unmap(), and
  that's supposed to work in practice.

- 1-dimensional texture unordered-access views and shader resource views are
  implemented.

- Shader resource view, unordered access view, and constant buffer view root
  descriptors with NULL GPU addresses are supported.

- Direct3D 12 descriptor heap destruction is delayed until all contained
  resources are destroyed.


### libvkd3d-shader

- New features for the HLSL source type:
  - Support for the ternary conditional operator "?:".
  - Support for "discard" statements.
  - Support for the "packoffset" keyword.
  - Support for semantics on array types.
  - Support for RWBuffer loads and stores.
  - Register allocation for arrays and structures of resources and samplers
    is implemented.
  - Support for the SV_IsFrontFace pixel shader system-value semantics.
  - Support for using constant expressions as array sizes and indices.
  - Support for dynamic selection of vector components.
  - Support for the following intrinsic functions:
    - D3DCOLORtoUBYTE4()
    - any()
    - asfloat()
    - ddx() and ddy()
    - fmod()
    - log(), log2(), and log10()
    - sign()
    - trunc()
  - The SampleBias(), SampleCmp(), SampleCmpLevelZero(), and SampleGrad()
    texture object methods are implemented.
  - Support for the case-insensitive variants of the "vector" and "matrix"
    data types.
  - Parser support for the "unroll" loop attribute. A warning is output for
    "unroll" without iteration count, and an error is output when an iteration
    count is specified. Actual unrolling is not implemented yet.
  - Parser support for RWStructuredBuffer resources.
  - Parser support for SamplerComparisonState objects. Note that outputting
    compiled effects is not supported yet, but parsing these allows shaders
    containing SamplerComparisonState state objects to be compiled.

- More improvements to HLSL support for the Direct3D shader model 1/2/3
  profiles.

- The section alignment of DXBC blobs produced by
  vkd3d_shader_serialize_dxbc() matches those produced by d3dcompiler more
  closely.

- The "main" function for shaders produced by the SPIR-V target is always
  terminated, even when the source was a TPF shader without explicit "ret"
  instruction.

- Relative addressing of shader input registers is supported by SPIR-V
  targets.


# What's new in vkd3d 1.7 (23 Mar 2023)

### libvkd3d-shader

- New features for the HLSL source type:
  - Support for calling user-defined functions.
  - Support for array parameters to user-defined functions.
  - Much improved support for the Direct3D shader model 1/2/3 profiles.
  - Support for the SV_DispatchThreadID, SV_GroupID, and SV_GroupThreadID
    compute shader system-value semantics.
  - Support for the optional "offset" parameter of the texture object Load()
    method.
  - Support for the all() intrinsic function.
  - Support for the distance() intrinsic function.
  - Support for the exp() and exp2() intrinsic functions.
  - Support for the frac() intrinsic function.
  - Support for the lit() intrinsic function.
  - Support for the reflect() intrinsic function.
  - Support for the sin() and cos() intrinsic functions.
  - Support for the smoothstep() intrinsic function.
  - Support for the sqrt() and rsqrt() intrinsic functions.
  - Support for the step() intrinsic function.
  - Support for the transpose() intrinsic function.
  - Support for the case-insensitive variants of the "float" and "dword" data
    types.
  - Partial support for minimum precision data types like "min16float". These
    are currently interpreted as their regular counterparts.
  - Improved constant propagation support, in particular to constant
    propagation through swizzles.

- HLSL static variables are now properly zero-initialised.

- The Direct3D shader model 4 and 5 disassembler outputs sample counts for
  multi-sampled resource declarations.

- New interfaces:
  - vkd3d_shader_parse_dxbc() provides support for parsing DXBC blobs.
  - vkd3d_shader_serialize_dxbc() provides support for serialising DXBC blobs.
  - vkd3d_shader_free_dxbc() is used to free vkd3d_shader_dxbc_desc
    structures, as returned by vkd3d_shader_parse_dxbc().
  - The VKD3D_SHADER_COMPILE_OPTION_WRITE_TESS_GEOM_POINT_SIZE compile option
    can be used to specify whether SPIR-V shaders targeting Vulkan
    environments should write point sizes for geometry and tessellation
    shaders. If left unspecified, point sizes will be written.


# What's new in vkd3d 1.6 (7 Dec 2022)

### libvkd3d-shader

- New features for the HLSL source type:
  - Initial support for compute shaders.
  - Improved support for initialisation and assignment of compound objects
    like structures and arrays, including casts and implicit conversions.
  - Support for loads and stores of texture resource unordered-access views.
  - Support for function attributes. In particular, the required "numthreads"
    attribute for compute shader entry points is now supported.
  - Support for the asuint() intrinsic function.
  - Support for the length() intrinsic function.
  - Support for the normalize() intrinsic function.
  - Support for integer division and modulus.
  - Support for taking the absolute value of integers.
  - Support for floating-point modulus.


- New interfaces:
  - The VKD3D_SHADER_DESCRIPTOR_INFO_FLAG_UAV_ATOMICS descriptor info flag is
    used to indicate that atomic operations are used on unordered-access view
    descriptors.


### libvkd3d-common

- vkd3d debug output is prefixed with "vkd3d:" in order to make it easier to
  distinguish from output produced by applications or other libraries.


### demos

- The demos now use libvkd3d-shader to compile HLSL shaders at run-time.


# What's new in vkd3d 1.5 (22 Sep 2022)

### libvkd3d-shader

- New features for the HLSL source type:
  - Improved support for HLSL object types (like e.g. ‘Texture2D’) inside
    structures and arrays.
  - Implicitly sized array initialisers.
  - Support for the dot() intrinsic function.
  - Support for the ldexp() intrinsic function.
  - Support for the lerp() intrinsic function.
  - Support for the logical ‘and’, ‘or’, and ‘not’ operators in shader model 4
    and 5 targets.
  - Support for casts from ‘bool’ types in shader model 4 and 5 targets.
  - Constant folding for integer bitwise operations.
  - Constant folding for integer min() and max().

- New interfaces:
  - The VKD3D_SHADER_COMPILE_OPTION_TYPED_UAV compile option can be used to
    specify the SPIR-V format to use for typed unordered access view loads.
    When set to ‘Unknown’, and the ‘shaderStorageImageReadWithoutFormat’
    feature is enabled in the target environment, this allows typed loads from
    multicomponent format unordered access views. If left unspecified, a R32
    format will be used, like in previous versions of libvkd3d-shader.


# What's new in vkd3d 1.4 (22 Jun 2022)

### libvkd3d

- A new descriptor heap implementation using the VK_EXT_descriptor_indexing
  extension. In particular, the new implementation is more efficient when
  large descriptor heaps are used by multiple command lists. The new
  ‘virtual_heaps’ configuration option can be used to select the original
  implementation even when the VK_EXT_descriptor_indexing extension is
  available.

- A new fence implementation using the VK_KHR_timeline_semaphore extension.
  The new implementation addresses a number of edge cases the original
  implementation was unable to, as well as being somewhat more efficient.

- When the VK_EXT_robustness2 extension is available, it is used to implement
  null views. This more accurately matches Direct3D 12 behaviour. For example,
  all reads from such a null view return zeroes, while that isn't necessarily
  the case for out-of-bounds reads with the original implementation.

- New interfaces:
  - vkd3d_set_log_callback() allows writing log output via a custom callback.
    This can be used to integrate vkd3d's log output with other logging
    systems.


### libvkd3d-shader

- New features for the HLSL source type:
  - Support for integer arithmetic, bitwise and shift operations.
  - Support for matrix and vector subscripting.
  - Support for the mul() intrinsic function.
  - Support for matrix copying, casting, and entry-wise operations.
  - Support for complex initialisers.
  - Support for the ‘nointerpolation’ modifier. This modifier is applied by
    default to integer variables.
  - Support for the SV_VertexID semantic.
  - Support for matrix-typed varyings.
  - Constant folding for a number of operators.
  - Copy propagation across branches and loops. This allows use of non-numeric
    variables anywhere in a program, as well as more optimised code for
    accessing numeric variables within branches and loops.

- The disassembler supports the shader model 5 ‘msad’ instruction.

- New interfaces:
  - vkd3d_shader_set_log_callback() allows writing log output via a custom
    callback.


### libvkd3d-utils

- New interfaces:
  - vkd3d_utils_set_log_callback() allows writing log output via a custom
    callback.


### build

- The minimum required version of Vulkan-Headers and SPIRV-Headers for this
  release is version 1.2.139.

- The SONAME_LIBVULKAN configure variable can be used to specify the shared
  object name of the Vulkan library. Because vkd3d loads the Vulkan library
  dynamically, specifying this removes the need for a Vulkan import library at
  build time.

- The ‘crosstests’ target no longer builds Win32/PE demos or tests when these
  were not enabled at configure time.


# What's new in vkd3d 1.3 (2 Mar 2022)

### libvkd3d

- Newly implemented Direct3D 12 features:
  - Root signature support for unbounded descriptor tables.
  - Unordered-access view counters in pixel shaders. These were previously
    only supported in compute shaders.
  - Output merger logical operations.
  - Retrieving CPU/GPU timestamp calibration values. This requires support for
    the VK_EXT_calibrated_timestamps extension.
  - The ‘mirror_once’ texture addressing mode. This requires support for the
    VK_KHR_sampler_mirror_clamp_to_edge extension.

- New interfaces:
  - The vkd3d_host_time_domain_info structure extends the
    vkd3d_instance_create_info structure, and can be used to specify how to
    convert between timestamps and tick counts. If left unspecified, a tick
    is assumed to take 100 nanoseconds.

- Various bug fixes.


### libvkd3d-shader

- New features:
  - Initial support for HLSL compilation and preprocessing. This is an ongoing
    effort; although support for many features is already implemented, support
    for many more isn't yet.
  - Support for disassembling Direct3D byte-code shaders to Direct3D assembly.
  - Support for parsing the legacy Direct3D byte-code format used by Direct3D
    shader model 1, 2, and 3 shaders. In the current vkd3d-shader release,
    only Direct3D assembly is supported as a target for these; we intend to
    support SPIR-V as a target in a future release.

- New features for the SPIR-V target:
  - Support for various aspects of Direct3D shader model 5.1 descriptor
    arrays, including unbounded descriptor arrays, UAV counter arrays, dynamic
    indexing of descriptor arrays, and non-uniform indexing of descriptor
    arrays. With the exception of some special cases, this requires support
    for the SPV_EXT_descriptor_indexing extension in the target environment.
  - Support for double precision floating-point operations.
  - Support for indirect addressing of tessellation control shader inputs.
  - Stencil export. I.e., writing stencil values from shaders. This requires
    support for the SPV_EXT_shader_stencil_export extension in the target
    environment.
  - Support for the Direct3D shader model 4+ ‘precise’ modifier.
  - Support for Direct3D shader model 4+ global resource memory barriers.

New interfaces:
  - vkd3d_shader_preprocess() provides support for preprocessing shaders.
  - The vkd3d_shader_preprocess_info structure extends the
    vkd3d_shader_compile_info structure, and can be used to specify
    preprocessing parameters like preprocessor macro definitions.
  - The vkd3d_shader_hlsl_source_info structure extends the
    vkd3d_shader_compile_info structure, and can be used to specify HLSL
    compilation parameters like the target profile and entry point.
  - The vkd3d_shader_descriptor_offset_info structure extends the
    vkd3d_shader_interface_info structure, and can be used to specify offsets
    into descriptor arrays referenced by shader interface bindings. This
    allows mapping multiple descriptor arrays in a shader to a single binding
    point in the target environment, and helps with mapping between the
    Direct3D 12 and Vulkan binding models.
  - The VKD3D_SHADER_COMPILE_OPTION_API_VERSION compile option can
    be used to specify the version of the libvkd3d-shader API the
    application is targetting. If left unspecified,
    VKD3D_SHADER_API_VERSION_1_2 will be used.

- Various shader translation fixes, for tessellation shaders in particular.


### vkd3d-compiler

- New source and target types:
  - The ‘hlsl’ source type specifies High Level Shader Language source code.
  - The ‘d3d-asm’ target type specifies Direct3D assembly shaders.
  - The ‘d3dbc’ format specifies legacy Direct3D byte-code, which is used for
    Direct3D shader model 1, 2, and 3 shaders.
  - The existing ‘dxbc-tpf’ format can now also be used as a target format.

- New command line options:
  - ‘-E’ can be used to specify the input should only be preprocessed.
  - ‘-e’/‘--entry’ can be used to specify the entry point for HLSL and/or
    SPIR-V shaders.
  - ‘-p’/‘--profile’ can be used to specify the target profile for HLSL
    shaders.

- When no source type is explicitly specified, vkd3d-compiler will attempt to
  determine the source type from the provided input. Note that this is
  intended as a convenience for interactive usage only, and the heuristics
  used are subject to future change. Non-interactive usage of vkd3d-compiler,
  for example in build scripts, should always explicitly specify source and
  target types.

- When no target type is explicitly specified, a default will be chosen based
  on the source type. Like the earlier mentioned source type detection, this
  is intended for interactive usage only.

- vkd3d-compiler will default to colour output if it can determine that the
  output is a colour-capable teleprinter.

- New environment variables:
  - NO_COLOUR/NO_COLOR can be used to disable default colour output.
  See the README for more detailed descriptions and how to use these.


### libvkd3d-utils

- New interfaces:
  - D3DCreateBlob() provides support for creating ID3DBlob objects.
  - D3DPreprocess() provides support for preprocessing HLSL source code.
  - D3DCompile() and D3DCompile2() provide support for compiling HLSL source
    code.


### build

- The ‘gears’ and ‘triangle’ demo applications are installed as ‘vkd3d-gears’
  and ‘vkd3d-triangle’. These were originally intended more as documentation
  than as end-user executables, but there's some value in using them for
  diagnostic purposes, much like e.g. ‘glxgears’.

- The VULKAN_LIBS configure variable is used when detecting the Vulkan
  library.

- Builds for the Microsoft Windows target platform no longer require support
  for POSIX threads. Windows synchronisation primitives are used instead.

- If ncurses is available, it will be use by vkd3d-compiler to determine the
  capabilities of the connected teleprinter, if any.


# What's new in vkd3d 1.2 (22 Sep 2020)

### libvkd3d

- Newly implemented Direct3D 12 features:
  - Multi-sampling.
  - Reserved resources.
  - Instance data step rates. This requires the
    VK_EXT_vertex_attribute_divisor extension.
  - ‘Private data’ APIs for all interfaces.
  - Shader-resource view component mappings.
  - Indirect indexed draws.
  - Indirect draws with a count buffer. This requires the
    VK_KHR_draw_indirect_count extension.
  - Stream output and stream output queries. This requires the
    VK_EXT_transform_feedback extension.
  - Predicated/conditional rendering.
  - Primitive restart.
  - Depth rendering without a pixel shader.
  - Depth clipping. This requires the VK_EXT_depth_clip_enable extension.
  - Rasteriser discard.
  - Dual-source blending.
  - Mapping placed resources.
  - The ReadFromSubresource() and WriteToSubresource() ID3D12Resource methods.
  - Simultaneous access to resources from multiple queues.
  - Null-views. I.e., views without an underlying resource.
  - Several more feature support queries.

- New interfaces:
  - vkd3d_serialize_versioned_root_signature() and
    vkd3d_create_versioned_root_signature_deserializer() provide support for
    versioned root signatures.
  - The vkd3d_application_info structure extends the
    vkd3d_instance_create_info structure, and can be used to pass information
    about the application to libvkd3d. It is analogous to the
    VkApplicationInfo structure in Vulkan. Its ‘api_version’ field should be
    set to the version of the libvkd3d API that the application targets.
  - The vkd3d_optional_device_extensions_info structure extends the
    vkd3d_device_create_info structure, and can be used to pass a list of
    device extensions to enable only when available to libvkd3d. It is
    analogous to the vkd3d_optional_instance_extensions_info structure for
    instance extensions.

- New environment variables:
  - VKD3D_CONFIG can be used to set options that change the behaviour of
    libvkd3d.
  - VKD3D_TEST_BUG can be used to disable bug_if() conditions in the test
    suite.
  - VKD3D_TEST_FILTER can be used to control which tests are run.
  - VKD3D_VULKAN_DEVICE can be used to override the Vulkan physical device
    used by vkd3d.
  See the README for more detailed descriptions and how to use these.

- When the VK_KHR_dedicated_allocation extension is available, dedicated
  allocations may be used for committed resources.

- When the VK_KHR_image_format_list extension is available, it will be used to
  inform the driver about the view formats that a particular mutable Vulkan
  image can be used with. This improves performance on some Vulkan
  implementations.

- When the VK_EXT_debug_marker extension is available, object names set with
  the ID3D12Object SetName() method will be propagated to the underlying
  Vulkan objects.

- Unordered-access view clears are supported on more formats. Previously these
  were limited to integer formats for texture resources, and single component
  integer formats for buffer resources.

- When the D24_UNORM_S8_UINT format is not supported by the Vulkan
  implementation, the D32_SFLOAT_S8_UINT format will be used instead to
  implement the D24_UNORM_S8_UINT and related DXGI formats. This is required
  because the DXGI D24_UNORM_S8_UINT format is mandatory, while the Vulkan
  D24_UNORM_S8_UINT format is optional.

- Various bug fixes.


### libvkd3d-shader

- libvkd3d-shader is now available as a public instead of an internal library.

- New features:
  - Tessellation shaders.
  - Root signature version 1.1 serialisation, deserialisation, and conversion.
  - Multi-sample masks.
  - Per-sample shading.
  - Early depth/stencil test.
  - Conservative depth output.
  - Dual-source blending.
  - Stream output.
  - Viewport arrays.

- Support for OpenGL SPIR-V target environments. This allows SPIR-V produced
  by libvkd3d-shader to be used with GL_ARB_gl_spirv. This includes support
  for OpenGL atomic counters and combined samplers.

- Preliminary support for shader model 5.1 shaders. This is still a work in
  progress. Notably, support for resource arrays is not yet implemented.

- When the SPV_EXT_demote_to_helper_invocation is available, it will be used
  to implement the ‘discard’ shader instruction instead of using SpvOpKill. In
  particular, this ensures the ‘deriv_rtx’ and ‘deriv_rty’ instruction return
  accurate results after a (conditional) ‘discard’ instruction.

- Support for using SPIR-V specialisation constants for shader parameters.

- Support for more shader instructions:
  - bufinfo,
  - eval_centroid,
  - eval_sample_index,
  - ld2ms,
  - sample_b,
  - sample_d,
  - sample_info,
  - samplepos.

- When built against SPIRV-Tools, libvkd3d-shader can produce SPIR-V shaders
  in text form.

- libvkd3d-shader now has its own environment variable (VKD3D_SHADER_DEBUG) to
  control debug output.

- Various shader translation fixes.


### vkd3d-compiler

- When supported by libvkd3d-shader, text form SPIR-V is available as a target
  format, in addition to the existing binary form SPIR-V target format.

- Input from standard input, and output to standard output is supported.


### libvkd3d-utils

- To specify the libvkd3d API version to use when creating vkd3d instances,
  define VKD3D_UTILS_API_VERSION to the desired version before including
  vkd3d_utils.h. If VKD3D_UTILS_API_VERSION is not explicitly defined,
  VKD3D_API_VERSION_1_0 will be used.

- Support for versioned root signatures is provided by the
  D3D12SerializeVersionedRootSignature() and
  D3D12CreateVersionedRootSignatureDeserializer() entry points.


### build

- The minimum required version of Vulkan-Headers and SPIRV-Headers for this
  release is version 1.1.113.

- The minimum required version of widl for this release is version 3.20.

- If doxygen is available, it will be used to build API documentation. By
  default, documentation will be generated in HTML and PDF formats.

- If debug logs are not required or desired, defining VKD3D_NO_TRACE_MESSAGES
  and VKD3D_NO_DEBUG_MESSAGES will prevent them from being included in the
  build. For example, a release build may want to configure with
  ‘CPPFLAGS="-DNDEBUG -DVKD3D_NO_TRACE_MESSAGES -DVKD3D_NO_DEBUG_MESSAGES"’.

- Microsoft Windows is now a supported target platform. To create a build for
  Windows, either cross-compile by configuring with an appropriate --host
  option like for example ‘--host=x86_64-w64-mingw32’, or build on Windows
  itself using an environment like MSYS2 or Cygwin.


# What's new in vkd3d 1.1 (5 Oct 2018)

### libvkd3d

- Initial support for memory heaps and placed resources.

- Improved support for resource views.

- ClearUnorderedAccessViewUint() is implemented for textures.

- Blend factor is implemented.

- Performance improvements.

- A new interface is available for enabling additional Vulkan instance
  extensions.

- A new public function is available for mapping VkFormats to DXGI_FORMATs.

- Support for more DXGI formats.

- Various bug fixes.


### libvkd3d-shader

- Support for geometry shaders.

- Pretty printing is implemented for shader code extracted from DXBC.

- Clip and cull distances are supported.

- Support for more shader instructions:
  - round_ne,
  - sincos,
  - ineg,
  - continue,
  - continuec,
  - gather4_po,
  - gather4_po_c,
  - gather4_c.

- Texel offsets are supported.

- Various shader translation fixes.


### libvkd3d-utils

- Vulkan WSI extensions are detected at runtime.


### build

- Demos are not built by default.

- libxcb is now an optional dependency required only for demos.

- MoltenVK is supported.


# What's included in vkd3d 1.0 (23 May 2018)

### libvkd3d

- libvkd3d is the main component of the vkd3d project. It's a 3D graphics
  library built on top of Vulkan with an API very similar to Direct3D 12.

- A significant number of Direct3D 12 features are implemented, including:
  - Graphics and compute pipelines.
  - Command lists, command allocators and command queues.
  - Descriptors and descriptor heaps.
  - Root signatures.
  - Constant buffer, shader resource, unordered access, render target and depth
    stencil views.
  - Samplers.
  - Static samplers.
  - Descriptors copying.
  - Committed resources.
  - Fences.
  - Queries and query heaps.
  - Resource barriers.
  - Root constants.
  - Basic support for indirect draws and dispatches.
  - Basic support for command signatures.
  - Various Clear*() methods.
  - Various Copy*() methods.


### libvkd3d-shader

- libvkd3d-shader is a library which translates shader model 4 and 5 bytecode
  to SPIR-V. In this release, libvkd3d-shader is an internal library. Its API
  isn't set in stone yet.

- Vertex, pixel and compute shaders are supported. Also, very simple geometry
  shaders should be translated correctly. Issues are expected when trying to
  translate tessellation shaders or more complex geometry shaders.

- A significant number of shader instructions are supported in this release,
  including:
  - Arithmetic instructions.
  - Bit instructions.
  - Comparison instructions.
  - Control flow instructions.
  - Sample, gather and load instructions.
  - Atomic instructions.
  - UAV instructions.

- Root signature serialization and deserialization is implemented.

- Shader model 4 and 5 bytecode parser is imported from wined3d.

- Shader model 5.1 is not supported yet.


### libvkd3d-utils

- libvkd3d-utils contains simple implementations of various functions which
  might be useful for source ports of Direct3D 12 applications.


### demos

- A simple hello triangle Direct3D 12 demo.
- A Direct3D 12 port of glxgears.
