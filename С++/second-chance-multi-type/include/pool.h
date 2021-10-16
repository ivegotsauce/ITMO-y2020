#pragma once

#include <cstdint>
#include <functional>
#include <iostream>
#include <type_traits>
#include <vector>

class PoolAllocator
{
public:
    PoolAllocator(std::size_t count, std::initializer_list<std::size_t> sizes);

public:
    void * allocate(size_t n);
    void deallocate(const void * ptr);

private:
    static constexpr size_t npos = static_cast<size_t>(-1);

private:
    std::pair<size_t, size_t> find_empty_place(size_t n) const;

private:
    const size_t m_obj_count;
    std::vector<std::byte> m_storage;
    std::vector<bool> m_used_map;
    std::vector<size_t> sizes;
};
