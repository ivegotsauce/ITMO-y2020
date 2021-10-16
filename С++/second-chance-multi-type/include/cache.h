#pragma once

#include <algorithm>
#include <cstddef>
#include <deque>
#include <list>
#include <new>
#include <ostream>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

public:
    std::size_t size() const
    {
        return m_queue.size();
    }

    bool empty() const
    {
        return m_queue.empty();
    }

    template <class T>
    T & get(const Key & key);

private:
    const std::size_t m_max_size;
    AllocatorWithPool m_alloc;
    std::list<std::pair<KeyProvider *, bool>> m_queue;
};

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    using value_type = std::pair<KeyProvider *, bool>;
    using iterator_type = typename std::list<value_type>::iterator;
    iterator_type it = std::find_if(
            m_queue.begin(), m_queue.end(), [&key](const value_type elem) {
                return *(elem.first) == key;
            });
    bool all_used = true;
    for (const std::pair pair : m_queue) {
        all_used = all_used && pair.second;
    }
    if (it != m_queue.end()) {
        it->second = true;
    }
    else {
        if (m_max_size == m_queue.size()) {
            if (all_used) {
                for (iterator_type iter = m_queue.begin(); iter != m_queue.end(); iter++) {
                    iter->second = false;
                }
            }
            else {
                while (m_queue.back().second) {
                    value_type elem = m_queue.back();
                    m_queue.pop_back();
                    m_queue.push_front(std::make_pair(elem.first, false));
                }
            }
            m_alloc.template destroy<KeyProvider>(m_queue.back().first);
            m_queue.pop_back();
        }
        m_queue.push_front(std::make_pair(m_alloc.template create<T>(key), false));
        m_queue.front().second = false;
        return *static_cast<T *>(m_queue.front().first);
    }
    return *static_cast<T *>((*it).first);
}