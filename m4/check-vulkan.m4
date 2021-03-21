dnl VKD3D_CHECK_VULKAN
AC_DEFUN([VKD3D_CHECK_VULKAN],[
VKD3D_CHECK_SONAME([vulkan], [vkGetInstanceProcAddr],
                   [VULKAN_LIBS="$VULKAN_LIBS -lvulkan"],
                   [VKD3D_CHECK_SONAME([vulkan-1], [vkGetInstanceProcAddr],
                                       [VULKAN_LIBS="$VULKAN_LIBS -lvulkan-1"
                                        AC_DEFINE_UNQUOTED([SONAME_LIBVULKAN], ["$ac_cv_lib_soname_vulkan_1"])],
                                       [VKD3D_CHECK_SONAME([MoltenVK], [vkGetInstanceProcAddr],
                                                           [VULKAN_LIBS="$VULKAN_LIBS -lMoltenVK"
                                                            AC_DEFINE_UNQUOTED([SONAME_LIBVULKAN],
                                                                               ["$ac_cv_lib_soname_MoltenVK"])],
                                                           [AC_MSG_ERROR([libvulkan and libMoltenVK not found.])],
                                                           [$VULKAN_LIBS])],
                                       [$VULKAN_LIBS])],
                   [$VULKAN_LIBS])
])
