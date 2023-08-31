#!/bin/bash

BASE_PATH="/usr/local/opt/molten-vk/share/vulkan/icd.d"

cp "$BASE_PATH/MoltenVK_icd.json" gitlab/MoltenVK_icd.json

# The driver path is relative
sed -i -e 's|"library_path" *: "|"library_path" : "'"$BASE_PATH/"'|g' gitlab/MoltenVK_icd.json

# We pretend this is not a portability driver, otherwise vkd3d won't
# find it
sed -i -e 's|"is_portability_driver" *: *true|"is_portability_driver" : false|g' gitlab/MoltenVK_icd.json
