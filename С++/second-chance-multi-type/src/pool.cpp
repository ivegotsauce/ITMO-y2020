#include "pool.h"

#include <cstddef>
#include <iostream>
#include <new>

using std::size_t;

PoolAllocator::PoolAllocator(const std::size_t count, std::initializer_list<std::size_t> sizes)
    : m_obj_count(count)
    , m_storage(count * sizes.size())
    , sizes(sizes)
{
    size_t m_size = 0;
    for (const size_t size : sizes) {
        m_size += count / size;
    }
    m_used_map.resize(m_size, false);
}

std::pair<size_t, size_t> PoolAllocator::find_empty_place(const size_t n) const
{
    size_t map_offset = 0;
    size_t slab_offset = 0;
    size_t obj_size = 0;
    for (const size_t size : sizes) {
        if (size == n) {
            obj_size = n;
            break;
        }
        slab_offset += m_obj_count;
        map_offset += m_obj_count / size;
    }
    size_t end = std::min(m_used_map.size(), map_offset + m_obj_count / obj_size);
    for (size_t i = map_offset; i < end; ++i) {
        if (!m_used_map[i]) {
            return {i, slab_offset};
        }
        slab_offset += obj_size;
    }
    return {npos, 0};
}

void * PoolAllocator::allocate(const size_t obj_size)
{
    std::pair<std::size_t, std::size_t> positions = find_empty_place(obj_size);
    const size_t pos = positions.first;
    if (pos != npos) {
        m_used_map[pos] = true;
        return &m_storage[positions.second];
    }
    throw std::bad_alloc{};
}

void PoolAllocator::deallocate(const void * ptr)
{
    const std::byte * b_ptr = static_cast<const std::byte *>(ptr);
    const std::byte * begin = &m_storage[0];
    if (b_ptr >= begin) {
        const size_t offset = (b_ptr - begin);
        size_t slab_offset = 0;
        size_t map_offset = 0;
        size_t obj_size = 0;
        for (const size_t size : sizes) {
            slab_offset += m_obj_count;
            if (offset < slab_offset) {
                obj_size = size;
                break;
            }
            map_offset += m_obj_count / size;
        }
        slab_offset -= m_obj_count;
        if (offset < m_storage.size()) {
            m_used_map[map_offset + (offset - slab_offset) / obj_size] = false;
        }
    }
}
