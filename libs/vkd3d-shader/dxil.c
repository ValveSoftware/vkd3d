/*
 * Copyright 2023 Conor McCarthy for CodeWeavers
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

#include "vkd3d_shader_private.h"

#define VKD3D_SM6_VERSION_MAJOR(version) (((version) >> 4) & 0xf)
#define VKD3D_SM6_VERSION_MINOR(version) (((version) >> 0) & 0xf)

#define BITCODE_MAGIC VKD3D_MAKE_TAG('B', 'C', 0xc0, 0xde)

enum bitcode_block_id
{
    BLOCKINFO_BLOCK           =  0,
    MODULE_BLOCK              =  8,
    PARAMATTR_BLOCK           =  9,
    PARAMATTR_GROUP_BLOCK     = 10,
    CONSTANTS_BLOCK           = 11,
    FUNCTION_BLOCK            = 12,
    VALUE_SYMTAB_BLOCK        = 14,
    METADATA_BLOCK            = 15,
    METADATA_ATTACHMENT_BLOCK = 16,
    TYPE_BLOCK                = 17,
    USELIST_BLOCK             = 18,
};

enum bitcode_blockinfo_code
{
    SETBID = 1,
    BLOCKNAME = 2,
    SETRECORDNAME = 3,
};

enum bitcode_block_abbreviation
{
    END_BLOCK = 0,
    ENTER_SUBBLOCK = 1,
    DEFINE_ABBREV = 2,
    UNABBREV_RECORD = 3,
};

enum bitcode_abbrev_type
{
    ABBREV_FIXED = 1,
    ABBREV_VBR   = 2,
    ABBREV_ARRAY = 3,
    ABBREV_CHAR  = 4,
    ABBREV_BLOB  = 5,
};

enum bitcode_address_space
{
    ADDRESS_SPACE_DEFAULT,
    ADDRESS_SPACE_DEVICEMEM,
    ADDRESS_SPACE_CBUFFER,
    ADDRESS_SPACE_GROUPSHARED,
};

enum bitcode_type_code
{
    TYPE_CODE_NUMENTRY     =  1,
    TYPE_CODE_VOID         =  2,
    TYPE_CODE_FLOAT        =  3,
    TYPE_CODE_DOUBLE       =  4,
    TYPE_CODE_LABEL        =  5,
    TYPE_CODE_INTEGER      =  7,
    TYPE_CODE_POINTER      =  8,
    TYPE_CODE_HALF         = 10,
    TYPE_CODE_ARRAY        = 11,
    TYPE_CODE_VECTOR       = 12,
    TYPE_CODE_METADATA     = 16,
    TYPE_CODE_STRUCT_ANON  = 18,
    TYPE_CODE_STRUCT_NAME  = 19,
    TYPE_CODE_STRUCT_NAMED = 20,
    TYPE_CODE_FUNCTION     = 21,
};

struct sm6_pointer_info
{
    const struct sm6_type *type;
    enum bitcode_address_space addr_space;
};

struct sm6_struct_info
{
    const char *name;
    unsigned int elem_count;
    const struct sm6_type *elem_types[];
};

struct sm6_function_info
{
    const struct sm6_type *ret_type;
    unsigned int param_count;
    const struct sm6_type *param_types[];
};

struct sm6_array_info
{
    unsigned int count;
    const struct sm6_type *elem_type;
};

enum sm6_type_class
{
    TYPE_CLASS_VOID,
    TYPE_CLASS_INTEGER,
    TYPE_CLASS_FLOAT,
    TYPE_CLASS_POINTER,
    TYPE_CLASS_STRUCT,
    TYPE_CLASS_FUNCTION,
    TYPE_CLASS_VECTOR,
    TYPE_CLASS_ARRAY,
    TYPE_CLASS_LABEL,
    TYPE_CLASS_METADATA,
};

struct sm6_type
{
    enum sm6_type_class class;
    union
    {
        unsigned int width;
        struct sm6_pointer_info pointer;
        struct sm6_struct_info *struc;
        struct sm6_function_info *function;
        struct sm6_array_info array;
    } u;
};

struct dxil_record
{
    unsigned int code;
    unsigned int operand_count;
    uint64_t operands[];
};

struct dxil_block
{
    const struct dxil_block *parent;
    enum bitcode_block_id id;
    unsigned int abbrev_len;
    unsigned int start;
    unsigned int length;
    unsigned int level;

    /* The abbrev, block and record structs are not relocatable. */
    struct dxil_abbrev **abbrevs;
    size_t abbrev_capacity;
    size_t abbrev_count;
    unsigned int blockinfo_bid;
    bool has_bid;

    struct dxil_block **child_blocks;
    size_t child_block_capacity;
    size_t child_block_count;

    struct dxil_record **records;
    size_t record_capacity;
    size_t record_count;
};

struct sm6_parser
{
    const uint32_t *ptr, *start, *end;
    unsigned int bitpos;

    struct dxil_block root_block;
    struct dxil_block *current_block;

    struct dxil_global_abbrev **abbrevs;
    size_t abbrev_capacity;
    size_t abbrev_count;

    struct sm6_type *types;
    size_t type_count;

    struct vkd3d_shader_parser p;
};

struct dxil_abbrev_operand
{
    uint64_t context;
    bool (*read_operand)(struct sm6_parser *sm6, uint64_t context, uint64_t *operand);
};

struct dxil_abbrev
{
    unsigned int count;
    bool is_array;
    struct dxil_abbrev_operand operands[];
};

struct dxil_global_abbrev
{
    unsigned int block_id;
    struct dxil_abbrev abbrev;
};

static struct sm6_parser *sm6_parser(struct vkd3d_shader_parser *parser)
{
    return CONTAINING_RECORD(parser, struct sm6_parser, p);
}

static bool sm6_parser_is_end(struct sm6_parser *sm6)
{
    return sm6->ptr == sm6->end;
}

static uint32_t sm6_parser_read_uint32(struct sm6_parser *sm6)
{
    if (sm6_parser_is_end(sm6))
    {
        sm6->p.failed = true;
        return 0;
    }
    return *sm6->ptr++;
}

static uint32_t sm6_parser_read_bits(struct sm6_parser *sm6, unsigned int length)
{
    unsigned int l, prev_len = 0;
    uint32_t bits;

    if (!length)
        return 0;

    assert(length < 32);

    if (sm6_parser_is_end(sm6))
    {
        sm6->p.failed = true;
        return 0;
    }

    assert(sm6->bitpos < 32);
    bits = *sm6->ptr >> sm6->bitpos;
    l = 32 - sm6->bitpos;
    if (l <= length)
    {
        ++sm6->ptr;
        if (sm6_parser_is_end(sm6) && l < length)
        {
            sm6->p.failed = true;
            return bits;
        }
        sm6->bitpos = 0;
        bits |= *sm6->ptr << l;
        prev_len = l;
    }
    sm6->bitpos += length - prev_len;

    return bits & ((1 << length) - 1);
}

static uint64_t sm6_parser_read_vbr(struct sm6_parser *sm6, unsigned int length)
{
    unsigned int bits, flag, mask, shift = 0;
    uint64_t result = 0;

    if (!length)
        return 0;

    if (sm6_parser_is_end(sm6))
    {
        sm6->p.failed = true;
        return 0;
    }

    flag = 1 << (length - 1);
    mask = flag - 1;
    do
    {
        bits = sm6_parser_read_bits(sm6, length);
        result |= (uint64_t)(bits & mask) << shift;
        shift += length - 1;
    } while ((bits & flag) && !sm6->p.failed && shift < 64);

    sm6->p.failed |= !!(bits & flag);

    return result;
}

static void sm6_parser_align_32(struct sm6_parser *sm6)
{
    if (!sm6->bitpos)
        return;

    if (sm6_parser_is_end(sm6))
    {
        sm6->p.failed = true;
        return;
    }

    ++sm6->ptr;
    sm6->bitpos = 0;
}

static bool dxil_block_handle_blockinfo_record(struct dxil_block *block, struct dxil_record *record)
{
    /* BLOCKINFO blocks must only occur immediately below the module root block. */
    if (block->level > 1)
    {
        WARN("Invalid blockinfo block level %u.\n", block->level);
        return false;
    }

    switch (record->code)
    {
        case SETBID:
            if (!record->operand_count)
            {
                WARN("Missing id operand.\n");
                return false;
            }
            if (record->operands[0] > UINT_MAX)
                WARN("Truncating block id %"PRIu64".\n", record->operands[0]);
            block->blockinfo_bid = record->operands[0];
            block->has_bid = true;
            break;
        case BLOCKNAME:
        case SETRECORDNAME:
            break;
        default:
            FIXME("Unhandled BLOCKINFO record type %u.\n", record->code);
            break;
    }

    return true;
}

static enum vkd3d_result dxil_block_add_record(struct dxil_block *block, struct dxil_record *record)
{
    unsigned int reserve;

    switch (block->id)
    {
        /* Rough initial reserve sizes for small shaders. */
        case CONSTANTS_BLOCK: reserve = 32; break;
        case FUNCTION_BLOCK: reserve = 128; break;
        case METADATA_BLOCK: reserve = 32; break;
        case TYPE_BLOCK: reserve = 32; break;
        default: reserve = 8; break;
    }
    reserve = max(reserve, block->record_count + 1);
    if (!vkd3d_array_reserve((void **)&block->records, &block->record_capacity, reserve, sizeof(*block->records)))
    {
        ERR("Failed to allocate %u records.\n", reserve);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    if (block->id == BLOCKINFO_BLOCK && !dxil_block_handle_blockinfo_record(block, record))
        return VKD3D_ERROR_INVALID_SHADER;

    block->records[block->record_count++] = record;

    return VKD3D_OK;
}

static enum vkd3d_result sm6_parser_read_unabbrev_record(struct sm6_parser *sm6)
{
    struct dxil_block *block = sm6->current_block;
    enum vkd3d_result ret = VKD3D_OK;
    unsigned int code, count, i;
    struct dxil_record *record;

    code = sm6_parser_read_vbr(sm6, 6);

    count = sm6_parser_read_vbr(sm6, 6);
    if (!(record = vkd3d_malloc(sizeof(*record) + count * sizeof(record->operands[0]))))
    {
        ERR("Failed to allocate record with %u operands.\n", count);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    record->code = code;
    record->operand_count = count;

    for (i = 0; i < count; ++i)
        record->operands[i] = sm6_parser_read_vbr(sm6, 6);
    if (sm6->p.failed)
        ret = VKD3D_ERROR_INVALID_SHADER;

    if (ret < 0 || (ret = dxil_block_add_record(block, record)) < 0)
        vkd3d_free(record);

    return ret;
}

static bool sm6_parser_read_literal_operand(struct sm6_parser *sm6, uint64_t context, uint64_t *op)
{
    *op = context;
    return !sm6->p.failed;
}

static bool sm6_parser_read_fixed_operand(struct sm6_parser *sm6, uint64_t context, uint64_t *op)
{
    *op = sm6_parser_read_bits(sm6, context);
    return !sm6->p.failed;
}

static bool sm6_parser_read_vbr_operand(struct sm6_parser *sm6, uint64_t context, uint64_t *op)
{
    *op = sm6_parser_read_vbr(sm6, context);
    return !sm6->p.failed;
}

static bool sm6_parser_read_char6_operand(struct sm6_parser *sm6, uint64_t context, uint64_t *op)
{
    *op = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._"[sm6_parser_read_bits(sm6, 6)];
    return !sm6->p.failed;
}

static bool sm6_parser_read_blob_operand(struct sm6_parser *sm6, uint64_t context, uint64_t *op)
{
    int count = sm6_parser_read_vbr(sm6, 6);
    sm6_parser_align_32(sm6);
    for (; count > 0; count -= 4)
        sm6_parser_read_uint32(sm6);
    FIXME("Unhandled blob operand.\n");
    return false;
}

static enum vkd3d_result dxil_abbrev_init(struct dxil_abbrev *abbrev, unsigned int count, struct sm6_parser *sm6)
{
    enum bitcode_abbrev_type prev_type, type;
    unsigned int i;

    abbrev->is_array = false;

    for (i = 0, prev_type = 0; i < count && !sm6->p.failed; ++i)
    {
        if (sm6_parser_read_bits(sm6, 1))
        {
            if (prev_type == ABBREV_ARRAY)
            {
                WARN("Unexpected literal abbreviation after array.\n");
                return VKD3D_ERROR_INVALID_SHADER;
            }
            abbrev->operands[i].context = sm6_parser_read_vbr(sm6, 8);
            abbrev->operands[i].read_operand = sm6_parser_read_literal_operand;
            continue;
        }

        switch (type = sm6_parser_read_bits(sm6, 3))
        {
            case ABBREV_FIXED:
            case ABBREV_VBR:
                abbrev->operands[i].context = sm6_parser_read_vbr(sm6, 5);
                abbrev->operands[i].read_operand = (type == ABBREV_FIXED) ? sm6_parser_read_fixed_operand
                        : sm6_parser_read_vbr_operand;
                break;

            case ABBREV_ARRAY:
                if (prev_type == ABBREV_ARRAY || i != count - 2)
                {
                    WARN("Unexpected array abbreviation.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }
                abbrev->is_array = true;
                --i;
                --count;
                break;

            case ABBREV_CHAR:
                abbrev->operands[i].read_operand = sm6_parser_read_char6_operand;
                break;

            case ABBREV_BLOB:
                if (prev_type == ABBREV_ARRAY || i != count - 1)
                {
                    WARN("Unexpected blob abbreviation.\n");
                    return VKD3D_ERROR_INVALID_SHADER;
                }
                abbrev->operands[i].read_operand = sm6_parser_read_blob_operand;
                break;
        }

        prev_type = type;
    }

    abbrev->count = count;

    return sm6->p.failed ? VKD3D_ERROR_INVALID_SHADER : VKD3D_OK;
}

static enum vkd3d_result sm6_parser_add_global_abbrev(struct sm6_parser *sm6)
{
    struct dxil_block *block = sm6->current_block;
    unsigned int count = sm6_parser_read_vbr(sm6, 5);
    struct dxil_global_abbrev *global_abbrev;
    enum vkd3d_result ret;

    assert(block->id == BLOCKINFO_BLOCK);

    if (!vkd3d_array_reserve((void **)&sm6->abbrevs, &sm6->abbrev_capacity, sm6->abbrev_count + 1, sizeof(*sm6->abbrevs))
            || !(global_abbrev = vkd3d_malloc(sizeof(*global_abbrev) + count * sizeof(global_abbrev->abbrev.operands[0]))))
    {
        ERR("Failed to allocate global abbreviation.\n");
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    if ((ret = dxil_abbrev_init(&global_abbrev->abbrev, count, sm6)) < 0)
    {
        vkd3d_free(global_abbrev);
        return ret;
    }

    if (!block->has_bid)
    {
        WARN("Missing blockinfo block id.\n");
        return VKD3D_ERROR_INVALID_SHADER;
    }
    if (block->blockinfo_bid == MODULE_BLOCK)
    {
        FIXME("Unhandled global abbreviation for module block.\n");
        return VKD3D_ERROR_INVALID_SHADER;
    }
    global_abbrev->block_id = block->blockinfo_bid;

    sm6->abbrevs[sm6->abbrev_count++] = global_abbrev;

    return VKD3D_OK;
}

static enum vkd3d_result sm6_parser_add_block_abbrev(struct sm6_parser *sm6)
{
    struct dxil_block *block = sm6->current_block;
    struct dxil_abbrev *abbrev;
    enum vkd3d_result ret;
    unsigned int count;

    if (block->id == BLOCKINFO_BLOCK)
        return sm6_parser_add_global_abbrev(sm6);

    count = sm6_parser_read_vbr(sm6, 5);
    if (!vkd3d_array_reserve((void **)&block->abbrevs, &block->abbrev_capacity, block->abbrev_count + 1, sizeof(*block->abbrevs))
            || !(abbrev = vkd3d_malloc(sizeof(*abbrev) + count * sizeof(abbrev->operands[0]))))
    {
        ERR("Failed to allocate block abbreviation.\n");
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    if ((ret = dxil_abbrev_init(abbrev, count, sm6)) < 0)
    {
        vkd3d_free(abbrev);
        return ret;
    }

    block->abbrevs[block->abbrev_count++] = abbrev;

    return VKD3D_OK;
}

static enum vkd3d_result sm6_parser_read_abbrev_record(struct sm6_parser *sm6, unsigned int abbrev_id)
{
    enum vkd3d_result ret = VKD3D_ERROR_INVALID_SHADER;
    struct dxil_block *block = sm6->current_block;
    struct dxil_record *temp, *record;
    unsigned int i, count, array_len;
    struct dxil_abbrev *abbrev;
    uint64_t code;

    if (abbrev_id >= block->abbrev_count)
    {
        WARN("Invalid abbreviation id %u.\n", abbrev_id);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    abbrev = block->abbrevs[abbrev_id];
    if (!(count = abbrev->count))
        return VKD3D_OK;
    if (count == 1 && abbrev->is_array)
        return VKD3D_ERROR_INVALID_SHADER;

    /* First operand is the record code. The array is included in the count, but will be done separately. */
    count -= abbrev->is_array + 1;
    if (!(record = vkd3d_malloc(sizeof(*record) + count * sizeof(record->operands[0]))))
    {
        ERR("Failed to allocate record with %u operands.\n", count);
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    if (!abbrev->operands[0].read_operand(sm6, abbrev->operands[0].context, &code))
        goto fail;
    if (code > UINT_MAX)
        FIXME("Truncating 64-bit record code %#"PRIx64".\n", code);
    record->code = code;

    for (i = 0; i < count; ++i)
        if (!abbrev->operands[i + 1].read_operand(sm6, abbrev->operands[i + 1].context, &record->operands[i]))
            goto fail;
    record->operand_count = count;

    /* An array can occur only as the last operand. */
    if (abbrev->is_array)
    {
        array_len = sm6_parser_read_vbr(sm6, 6);
        if (!(temp = vkd3d_realloc(record, sizeof(*record) + (count + array_len) * sizeof(record->operands[0]))))
        {
            ERR("Failed to allocate record with %u operands.\n", count + array_len);
            ret = VKD3D_ERROR_OUT_OF_MEMORY;
            goto fail;
        }
        record = temp;

        for (i = 0; i < array_len; ++i)
        {
            if (!abbrev->operands[count + 1].read_operand(sm6, abbrev->operands[count + 1].context,
                    &record->operands[count + i]))
            {
                goto fail;
            }
        }
        record->operand_count += array_len;
    }

    if ((ret = dxil_block_add_record(block, record)) < 0)
        goto fail;

    return VKD3D_OK;

fail:
    vkd3d_free(record);
    return ret;
}

static enum vkd3d_result dxil_block_init(struct dxil_block *block, const struct dxil_block *parent,
        struct sm6_parser *sm6);

static enum vkd3d_result dxil_block_read(struct dxil_block *parent, struct sm6_parser *sm6)
{
    unsigned int reserve = (parent->id == MODULE_BLOCK) ? 12 : 2;
    struct dxil_block *block;
    enum vkd3d_result ret;

    sm6->current_block = parent;

    do
    {
        unsigned int abbrev_id = sm6_parser_read_bits(sm6, parent->abbrev_len);

        switch (abbrev_id)
        {
            case END_BLOCK:
                sm6_parser_align_32(sm6);
                return VKD3D_OK;

            case ENTER_SUBBLOCK:
                if (parent->id != MODULE_BLOCK && parent->id != FUNCTION_BLOCK)
                {
                    WARN("Invalid subblock parent id %u.\n", parent->id);
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                if (!vkd3d_array_reserve((void **)&parent->child_blocks, &parent->child_block_capacity,
                        max(reserve, parent->child_block_count + 1), sizeof(*parent->child_blocks))
                        || !(block = vkd3d_calloc(1, sizeof(*block))))
                {
                    ERR("Failed to allocate block.\n");
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                }

                if ((ret = dxil_block_init(block, parent, sm6)) < 0)
                {
                    vkd3d_free(block);
                    return ret;
                }

                parent->child_blocks[parent->child_block_count++] = block;
                sm6->current_block = parent;
                break;

            case DEFINE_ABBREV:
                if ((ret = sm6_parser_add_block_abbrev(sm6)) < 0)
                    return ret;
                break;

            case UNABBREV_RECORD:
                if ((ret = sm6_parser_read_unabbrev_record(sm6)) < 0)
                {
                    WARN("Failed to read unabbreviated record.\n");
                    return ret;
                }
                break;

            default:
                if ((ret = sm6_parser_read_abbrev_record(sm6, abbrev_id - 4)) < 0)
                {
                    WARN("Failed to read abbreviated record.\n");
                    return ret;
                }
                break;
        }
    } while (!sm6->p.failed);

    return VKD3D_ERROR_INVALID_SHADER;
}

static size_t sm6_parser_compute_global_abbrev_count_for_block_id(struct sm6_parser *sm6,
        unsigned int block_id)
{
    size_t i, count;

    for (i = 0, count = 0; i < sm6->abbrev_count; ++i)
        count += sm6->abbrevs[i]->block_id == block_id;

    return count;
}

static void dxil_block_destroy(struct dxil_block *block)
{
    size_t i;

    for (i = 0; i < block->record_count; ++i)
        vkd3d_free(block->records[i]);
    vkd3d_free(block->records);

    for (i = 0; i < block->child_block_count; ++i)
    {
        dxil_block_destroy(block->child_blocks[i]);
        vkd3d_free(block->child_blocks[i]);
    }
    vkd3d_free(block->child_blocks);

    block->records = NULL;
    block->record_count = 0;
    block->child_blocks = NULL;
    block->child_block_count = 0;
}

static enum vkd3d_result dxil_block_init(struct dxil_block *block, const struct dxil_block *parent,
        struct sm6_parser *sm6)
{
    size_t i, abbrev_count = 0;
    enum vkd3d_result ret;

    block->parent = parent;
    block->level = parent ? parent->level + 1 : 0;
    block->id = sm6_parser_read_vbr(sm6, 8);
    block->abbrev_len = sm6_parser_read_vbr(sm6, 4);
    sm6_parser_align_32(sm6);
    block->length = sm6_parser_read_uint32(sm6);
    block->start = sm6->ptr - sm6->start;

    if (sm6->p.failed)
        return VKD3D_ERROR_INVALID_SHADER;

    if ((block->abbrev_count = sm6_parser_compute_global_abbrev_count_for_block_id(sm6, block->id)))
    {
        if (!vkd3d_array_reserve((void **)&block->abbrevs, &block->abbrev_capacity,
                block->abbrev_count, sizeof(*block->abbrevs)))
        {
            ERR("Failed to allocate block abbreviations.\n");
            return VKD3D_ERROR_OUT_OF_MEMORY;
        }

        for (i = 0; i < sm6->abbrev_count; ++i)
            if (sm6->abbrevs[i]->block_id == block->id)
                block->abbrevs[abbrev_count++] = &sm6->abbrevs[i]->abbrev;

        assert(abbrev_count == block->abbrev_count);
    }

    if ((ret = dxil_block_read(block, sm6)) < 0)
        dxil_block_destroy(block);

    for (i = abbrev_count; i < block->abbrev_count; ++i)
        vkd3d_free(block->abbrevs[i]);
    vkd3d_free(block->abbrevs);
    block->abbrevs = NULL;
    block->abbrev_count = 0;

    return ret;
}

static void dxil_global_abbrevs_cleanup(struct dxil_global_abbrev **abbrevs, size_t count)
{
    size_t i;

    for (i = 0; i < count; ++i)
        vkd3d_free(abbrevs[i]);
    vkd3d_free(abbrevs);
}

static const struct dxil_block *sm6_parser_get_level_one_block(const struct sm6_parser *sm6,
        enum bitcode_block_id id, bool *is_unique)
{
    const struct dxil_block *block, *found = NULL;
    size_t i;

    for (i = 0, *is_unique = true; i < sm6->root_block.child_block_count; ++i)
    {
        block = sm6->root_block.child_blocks[i];
        if (block->id != id)
            continue;

        if (!found)
            found = block;
        else
            *is_unique = false;
    }

    return found;
}

static char *dxil_record_to_string(const struct dxil_record *record)
{
    unsigned int i;
    char *str;

    if (!(str = vkd3d_calloc(record->operand_count + 1, 1)))
        return NULL;

    for (i = 0; i < record->operand_count; ++i)
        str[i] = record->operands[i];

    return str;
}

static bool dxil_record_validate_operand_min_count(const struct dxil_record *record, unsigned int min_count,
        struct sm6_parser *sm6)
{
    if (record->operand_count >= min_count)
        return true;

    WARN("Invalid operand count %u for code %u.\n", record->operand_count, record->code);
    vkd3d_shader_parser_error(&sm6->p, VKD3D_SHADER_ERROR_DXIL_INVALID_OPERAND_COUNT,
            "Invalid operand count %u for record code %u.", record->operand_count, record->code);
    return false;
}

static void dxil_record_validate_operand_max_count(const struct dxil_record *record, unsigned int max_count,
        struct sm6_parser *sm6)
{
    if (record->operand_count <= max_count)
        return;

    WARN("Ignoring %u extra operands for code %u.\n", record->operand_count - max_count, record->code);
    vkd3d_shader_parser_warning(&sm6->p, VKD3D_SHADER_WARNING_DXIL_IGNORING_OPERANDS,
            "Ignoring %u extra operands for record code %u.", record->operand_count - max_count, record->code);
}

static bool dxil_record_validate_operand_count(const struct dxil_record *record, unsigned int min_count,
        unsigned int max_count, struct sm6_parser *sm6)
{
    dxil_record_validate_operand_max_count(record, max_count, sm6);
    return dxil_record_validate_operand_min_count(record, min_count, sm6);
}

static enum vkd3d_result sm6_parser_type_table_init(struct sm6_parser *sm6)
{
    const struct dxil_record *record;
    size_t i, type_count, type_index;
    const struct dxil_block *block;
    char *struct_name = NULL;
    unsigned int j, count;
    struct sm6_type *type;
    uint64_t type_id;
    bool is_unique;

    sm6->p.location.line = 0;
    sm6->p.location.column = 0;

    if (!(block = sm6_parser_get_level_one_block(sm6, TYPE_BLOCK, &is_unique)))
    {
        WARN("No type definitions found.\n");
        return VKD3D_OK;
    }
    if (!is_unique)
        WARN("Ignoring invalid extra type table(s).\n");

    sm6->p.location.line = block->id;

    type_count = 0;
    for (i = 0; i < block->record_count; ++i)
        type_count += block->records[i]->code != TYPE_CODE_NUMENTRY && block->records[i]->code != TYPE_CODE_STRUCT_NAME;

    /* The type array must not be relocated. */
    if (!(sm6->types = vkd3d_calloc(type_count, sizeof(*sm6->types))))
    {
        ERR("Failed to allocate type array.\n");
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    for (i = 0; i < block->record_count; ++i)
    {
        sm6->p.location.column = i;
        record = block->records[i];

        type = &sm6->types[sm6->type_count];
        type_index = sm6->type_count;

        switch (record->code)
        {
            case TYPE_CODE_ARRAY:
            case TYPE_CODE_VECTOR:
                if (!dxil_record_validate_operand_count(record, 2, 2, sm6))
                    return VKD3D_ERROR_INVALID_SHADER;

                type->class = record->code == TYPE_CODE_ARRAY ? TYPE_CLASS_ARRAY : TYPE_CLASS_VECTOR;

                if (!(type->u.array.count = record->operands[0]))
                {
                    TRACE("Setting unbounded for type %zu.\n", type_index);
                    type->u.array.count = UINT_MAX;
                }

                if ((type_id = record->operands[1]) >= type_count)
                {
                    WARN("Invalid contained type id %"PRIu64" for type %zu.\n", type_id, type_index);
                    return VKD3D_ERROR_INVALID_SHADER;
                }
                type->u.array.elem_type = &sm6->types[type_id];
                break;

            case TYPE_CODE_DOUBLE:
                dxil_record_validate_operand_max_count(record, 0, sm6);
                type->class = TYPE_CLASS_FLOAT;
                type->u.width = 64;
                break;

            case TYPE_CODE_FLOAT:
                dxil_record_validate_operand_max_count(record, 0, sm6);
                type->class = TYPE_CLASS_FLOAT;
                type->u.width = 32;
                break;

            case TYPE_CODE_FUNCTION:
                if (!dxil_record_validate_operand_min_count(record, 2, sm6))
                    return VKD3D_ERROR_INVALID_SHADER;
                if (record->operands[0])
                    FIXME("Unhandled vararg function type %zu.\n", type_index);

                type->class = TYPE_CLASS_FUNCTION;

                if ((type_id = record->operands[1]) >= type_count)
                {
                    WARN("Invalid return type id %"PRIu64" for type %zu.\n", type_id, type_index);
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                count = record->operand_count - 2;
                if (vkd3d_object_range_overflow(sizeof(type->u.function), count, sizeof(type->u.function->param_types[0]))
                        || !(type->u.function = vkd3d_malloc(offsetof(struct sm6_function_info, param_types[count]))))
                {
                    ERR("Failed to allocate function parameter types.\n");
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                }

                type->u.function->ret_type = &sm6->types[type_id];
                type->u.function->param_count = count;
                for (j = 0; j < count; ++j)
                {
                    if ((type_id = record->operands[j + 2]) >= type_count)
                    {
                        WARN("Invalid parameter type id %"PRIu64" for type %zu.\n", type_id, type_index);
                        vkd3d_free(type->u.function);
                        return VKD3D_ERROR_INVALID_SHADER;
                    }
                    type->u.function->param_types[j] = &sm6->types[type_id];
                }
                break;

            case TYPE_CODE_HALF:
                dxil_record_validate_operand_max_count(record, 0, sm6);
                type->class = TYPE_CLASS_FLOAT;
                type->u.width = 16;
                break;

            case TYPE_CODE_INTEGER:
            {
                uint64_t width;

                if (!dxil_record_validate_operand_count(record, 1, 1, sm6))
                    return VKD3D_ERROR_INVALID_SHADER;

                type->class = TYPE_CLASS_INTEGER;

                switch ((width = record->operands[0]))
                {
                    case 1:
                    case 8:
                    case 16:
                    case 32:
                    case 64:
                        break;
                    default:
                        WARN("Invalid integer width %"PRIu64" for type %zu.\n", width, type_index);
                        return VKD3D_ERROR_INVALID_SHADER;
                }
                type->u.width = width;
                break;
            }

            case TYPE_CODE_LABEL:
                type->class = TYPE_CLASS_LABEL;
                break;

            case TYPE_CODE_METADATA:
                type->class = TYPE_CLASS_METADATA;
                break;

            case TYPE_CODE_NUMENTRY:
                continue;

            case TYPE_CODE_POINTER:
                if (!dxil_record_validate_operand_count(record, 1, 2, sm6))
                    return VKD3D_ERROR_INVALID_SHADER;

                type->class = TYPE_CLASS_POINTER;

                if ((type_id = record->operands[0]) >= type_count)
                {
                    WARN("Invalid pointee type id %"PRIu64" for type %zu.\n", type_id, type_index);
                    return VKD3D_ERROR_INVALID_SHADER;
                }
                type->u.pointer.type = &sm6->types[type_id];
                type->u.pointer.addr_space = (record->operand_count > 1) ? record->operands[1] : ADDRESS_SPACE_DEFAULT;
                break;

            case TYPE_CODE_STRUCT_ANON:
            case TYPE_CODE_STRUCT_NAMED:
                if (!dxil_record_validate_operand_min_count(record, 2, sm6))
                    return VKD3D_ERROR_INVALID_SHADER;
                if (record->code == TYPE_CODE_STRUCT_NAMED && !struct_name)
                {
                    WARN("Missing struct name before struct type %zu.\n", type_index);
                    return VKD3D_ERROR_INVALID_SHADER;
                }

                type->class = TYPE_CLASS_STRUCT;

                count = record->operand_count - 1;
                if (vkd3d_object_range_overflow(sizeof(type->u.struc), count, sizeof(type->u.struc->elem_types[0]))
                        || !(type->u.struc = vkd3d_malloc(offsetof(struct sm6_struct_info, elem_types[count]))))
                {
                    ERR("Failed to allocate struct element types.\n");
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                }

                if (record->operands[0])
                    FIXME("Ignoring struct packed attribute.\n");

                type->u.struc->elem_count = count;
                for (j = 0; j < count; ++j)
                {
                    if ((type_id = record->operands[j + 1]) >= type_count)
                    {
                        WARN("Invalid contained type id %"PRIu64" for type %zu.\n", type_id, type_index);
                        vkd3d_free(type->u.struc);
                        return VKD3D_ERROR_INVALID_SHADER;
                    }
                    type->u.struc->elem_types[j] = &sm6->types[type_id];
                }

                if (record->code == TYPE_CODE_STRUCT_ANON)
                {
                    type->u.struc->name = NULL;
                    break;
                }

                type->u.struc->name = struct_name;
                struct_name = NULL;
                break;

            case TYPE_CODE_STRUCT_NAME:
                if (!(struct_name = dxil_record_to_string(record)))
                {
                    ERR("Failed to allocate struct name.\n");
                    return VKD3D_ERROR_OUT_OF_MEMORY;
                }
                if (!struct_name[0])
                    WARN("Struct name is empty for type %zu.\n", type_index);
                continue;

            case TYPE_CODE_VOID:
                dxil_record_validate_operand_max_count(record, 0, sm6);
                type->class = TYPE_CLASS_VOID;
                break;

            default:
                FIXME("Unhandled type %u at index %zu.\n", record->code, type_index);
                return VKD3D_ERROR_INVALID_SHADER;
        }
        ++sm6->type_count;
    }

    assert(sm6->type_count == type_count);

    if (struct_name)
    {
        WARN("Unused struct name %s.\n", struct_name);
        vkd3d_free(struct_name);
    }

    return VKD3D_OK;
}

static void sm6_type_table_cleanup(struct sm6_type *types, size_t count)
{
    size_t i;

    if (!types)
        return;

    for (i = 0; i < count; ++i)
    {
        switch (types[i].class)
        {
            case TYPE_CLASS_STRUCT:
                vkd3d_free((void *)types[i].u.struc->name);
                vkd3d_free(types[i].u.struc);
                break;
            case TYPE_CLASS_FUNCTION:
                vkd3d_free(types[i].u.function);
                break;
            default:
                break;
        }
    }

    vkd3d_free(types);
}

static void sm6_parser_destroy(struct vkd3d_shader_parser *parser)
{
    struct sm6_parser *sm6 = sm6_parser(parser);

    dxil_block_destroy(&sm6->root_block);
    dxil_global_abbrevs_cleanup(sm6->abbrevs, sm6->abbrev_count);
    shader_instruction_array_destroy(&parser->instructions);
    sm6_type_table_cleanup(sm6->types, sm6->type_count);
    free_shader_desc(&parser->shader_desc);
    vkd3d_free(sm6);
}

static const struct vkd3d_shader_parser_ops sm6_parser_ops =
{
    .parser_destroy = sm6_parser_destroy,
};

static enum vkd3d_result sm6_parser_init(struct sm6_parser *sm6, const uint32_t *byte_code, size_t byte_code_size,
        const char *source_name, struct vkd3d_shader_message_context *message_context)
{
    const struct vkd3d_shader_location location = {.source_name = source_name};
    uint32_t version_token, dxil_version, token_count, magic;
    unsigned int chunk_offset, chunk_size;
    enum bitcode_block_abbreviation abbr;
    struct vkd3d_shader_version version;
    struct dxil_block *block;
    enum vkd3d_result ret;
    size_t count, length;

    count = byte_code_size / sizeof(*byte_code);
    if (count < 6)
    {
        WARN("Invalid data size %zu.\n", byte_code_size);
        vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_SIZE,
                "DXIL chunk size %zu is smaller than the DXIL header size.", byte_code_size);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    version_token = byte_code[0];
    TRACE("Compiler version: 0x%08x.\n", version_token);
    token_count = byte_code[1];
    TRACE("Token count: %u.\n", token_count);

    if (token_count < 6 || count < token_count)
    {
        WARN("Invalid token count %u (word count %zu).\n", token_count, count);
        vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_CHUNK_SIZE,
                "DXIL chunk token count %#x is invalid (word count %zu).", token_count, count);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    if (byte_code[2] != TAG_DXIL)
        WARN("Unknown magic number 0x%08x.\n", byte_code[2]);

    dxil_version = byte_code[3];
    if (dxil_version > 0x102)
        WARN("Unknown DXIL version: 0x%08x.\n", dxil_version);
    else
        TRACE("DXIL version: 0x%08x.\n", dxil_version);

    chunk_offset = byte_code[4];
    if (chunk_offset < 16 || chunk_offset >= byte_code_size)
    {
        WARN("Invalid bitcode chunk offset %#x (data size %zu).\n", chunk_offset, byte_code_size);
        vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_CHUNK_OFFSET,
                "DXIL bitcode chunk has invalid offset %#x (data size %#zx).", chunk_offset, byte_code_size);
        return VKD3D_ERROR_INVALID_SHADER;
    }
    chunk_size = byte_code[5];
    if (chunk_size > byte_code_size - chunk_offset)
    {
        WARN("Invalid bitcode chunk size %#x (data size %zu, chunk offset %#x).\n",
                chunk_size, byte_code_size, chunk_offset);
        vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_CHUNK_SIZE,
                "DXIL bitcode chunk has invalid size %#x (data size %#zx, chunk offset %#x).",
                chunk_size, byte_code_size, chunk_offset);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    sm6->start = (const uint32_t *)((const char*)&byte_code[2] + chunk_offset);
    if ((magic = sm6->start[0]) != BITCODE_MAGIC)
    {
        WARN("Unknown magic number 0x%08x.\n", magic);
        vkd3d_shader_parser_warning(&sm6->p, VKD3D_SHADER_WARNING_DXIL_UNKNOWN_MAGIC_NUMBER,
                "DXIL bitcode chunk magic number 0x%08x is not the expected 0x%08x.", magic, BITCODE_MAGIC);
    }

    sm6->end = &sm6->start[(chunk_size + sizeof(*sm6->start) - 1) / sizeof(*sm6->start)];

    if ((version.type = version_token >> 16) >= VKD3D_SHADER_TYPE_COUNT)
    {
        FIXME("Unknown shader type %#x.\n", version.type);
        vkd3d_shader_parser_warning(&sm6->p, VKD3D_SHADER_WARNING_DXIL_UNKNOWN_SHADER_TYPE,
                "Unknown shader type %#x.", version.type);
    }

    version.major = VKD3D_SM6_VERSION_MAJOR(version_token);
    version.minor = VKD3D_SM6_VERSION_MINOR(version_token);

    if ((abbr = sm6->start[1] & 3) != ENTER_SUBBLOCK)
    {
        WARN("Initial block abbreviation %u is not ENTER_SUBBLOCK.\n", abbr);
        vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_BITCODE,
                "DXIL bitcode chunk has invalid initial block abbreviation %u.", abbr);
        return VKD3D_ERROR_INVALID_SHADER;
    }

    /* Estimate instruction count to avoid reallocation in most shaders. */
    count = max(token_count, 400) - 400;
    vkd3d_shader_parser_init(&sm6->p, message_context, source_name, &version, &sm6_parser_ops,
            (count + (count >> 2)) / 2u + 10);
    sm6->ptr = &sm6->start[1];
    sm6->bitpos = 2;

    block = &sm6->root_block;
    if ((ret = dxil_block_init(block, NULL, sm6)) < 0)
    {
        if (ret == VKD3D_ERROR_OUT_OF_MEMORY)
            vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_OUT_OF_MEMORY,
                    "Out of memory parsing DXIL bitcode chunk.");
        else if (ret == VKD3D_ERROR_INVALID_SHADER)
            vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_BITCODE,
                    "DXIL bitcode chunk has invalid bitcode.");
        else
            vkd3d_unreachable();
        return ret;
    }

    dxil_global_abbrevs_cleanup(sm6->abbrevs, sm6->abbrev_count);
    sm6->abbrevs = NULL;
    sm6->abbrev_count = 0;

    length = sm6->ptr - sm6->start - block->start;
    if (length != block->length)
    {
        WARN("Invalid block length %zu; expected %u.\n", length, block->length);
        vkd3d_shader_parser_warning(&sm6->p, VKD3D_SHADER_WARNING_DXIL_INVALID_BLOCK_LENGTH,
                "Root block ends with length %zu but indicated length is %u.", length, block->length);
    }
    if (sm6->ptr != sm6->end)
    {
        size_t expected_length = sm6->end - sm6->start;
        length = sm6->ptr - sm6->start;
        WARN("Invalid module length %zu; expected %zu.\n", length, expected_length);
        vkd3d_shader_parser_warning(&sm6->p, VKD3D_SHADER_WARNING_DXIL_INVALID_MODULE_LENGTH,
                "Module ends with length %zu but indicated length is %zu.", length, expected_length);
    }

    if ((ret = sm6_parser_type_table_init(sm6)) < 0)
    {
        if (ret == VKD3D_ERROR_OUT_OF_MEMORY)
            vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_OUT_OF_MEMORY,
                    "Out of memory parsing DXIL type table.");
        else if (ret == VKD3D_ERROR_INVALID_SHADER)
            vkd3d_shader_error(message_context, &location, VKD3D_SHADER_ERROR_DXIL_INVALID_TYPE_TABLE,
                    "DXIL type table is invalid.");
        else
            vkd3d_unreachable();
        return ret;
    }

    dxil_block_destroy(&sm6->root_block);

    return VKD3D_OK;
}

int vkd3d_shader_sm6_parser_create(const struct vkd3d_shader_compile_info *compile_info,
        struct vkd3d_shader_message_context *message_context, struct vkd3d_shader_parser **parser)
{
    struct vkd3d_shader_desc *shader_desc;
    uint32_t *byte_code = NULL;
    struct sm6_parser *sm6;
    int ret;

    if (!(sm6 = vkd3d_calloc(1, sizeof(*sm6))))
    {
        ERR("Failed to allocate parser.\n");
        return VKD3D_ERROR_OUT_OF_MEMORY;
    }

    shader_desc = &sm6->p.shader_desc;
    shader_desc->is_dxil = true;
    if ((ret = shader_extract_from_dxbc(&compile_info->source, message_context, compile_info->source_name,
            shader_desc)) < 0)
    {
        WARN("Failed to extract shader, vkd3d result %d.\n", ret);
        vkd3d_free(sm6);
        return ret;
    }

    sm6->p.shader_desc = *shader_desc;
    shader_desc = &sm6->p.shader_desc;

    if (((uintptr_t)shader_desc->byte_code & (VKD3D_DXBC_CHUNK_ALIGNMENT - 1)))
    {
        /* LLVM bitcode should be 32-bit aligned, but before dxc v1.7.2207 this was not always the case in the DXBC
         * container due to missing padding after signature names. Get an aligned copy to prevent unaligned access. */
        if (!(byte_code = vkd3d_malloc(align(shader_desc->byte_code_size, VKD3D_DXBC_CHUNK_ALIGNMENT))))
            ERR("Failed to allocate aligned chunk. Unaligned access will occur.\n");
        else
            memcpy(byte_code, shader_desc->byte_code, shader_desc->byte_code_size);
    }

    ret = sm6_parser_init(sm6, byte_code ? byte_code : shader_desc->byte_code, shader_desc->byte_code_size,
            compile_info->source_name, message_context);
    vkd3d_free(byte_code);

    if (ret < 0)
    {
        WARN("Failed to initialise shader parser.\n");
        sm6_parser_destroy(&sm6->p);
        return ret;
    }

    *parser = &sm6->p;

    return ret;
}
