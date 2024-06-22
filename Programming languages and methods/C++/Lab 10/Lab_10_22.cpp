/*
Кольцевой буфер с двунаправленным итератором по элементам представляемой им очереди 
(элементы должны перебираться в том порядке, в каком они добавлялись в очередь).
*/
#include <iostream>
#include <vector>
#include <iterator>

using namespace std;

template<typename T>
class CircularBuffer {
public:
    class Iterator;

    CircularBuffer(size_t capacity)
        : capacity(capacity), start(0), endIndex(0), size(0), buffer(capacity) {}

    void add(const T& item) {
        buffer[endIndex] = item;
        endIndex = (endIndex + 1) % capacity;
        if (size == capacity) {
            start = (start + 1) % capacity;
        } else {
            ++size;
        }
    }

    ~CircularBuffer() {
        buffer.clear();
    }

    T& operator[](size_t index) {
        return buffer[(start + index) % capacity];
    }

    size_t getSize() const {
        return size;
    }

    Iterator begin() {
        return Iterator(this, 0);
    }

    Iterator end() {
        return Iterator(this, size - 1);
    }

    class Iterator {
    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = T;
        using difference_type = std::ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        Iterator(CircularBuffer* buffer, size_t position)
            : buffer(buffer), position(position) {}

        reference operator*() {
            return (*buffer)[position];
        }

        pointer operator->() {
            return &(*buffer)[position];
        }

        Iterator& operator++() {
            if (position == buffer->capacity - 1) {
                position = 0;
            } else {
                ++position;
            }
            return *this;
        }

        Iterator operator++(int) {
            Iterator temp = *this;
            ++(*this);
            return temp;
        }

        
        Iterator& operator--() {
            if (position == 0) {
                position = buffer->capacity - 1;
            } else {
                --position;
            }
            return *this;
        }
        Iterator operator--(int) {
            Iterator temp = *this;
            --(*this);
            return temp;
        }

        bool operator==(const Iterator& other) const {
            return position == other.position && buffer == other.buffer;
        }

        bool operator!=(const Iterator& other) const {
            return !(*this == other);
        }

    private:
        CircularBuffer* buffer;
        size_t position;
    };

private:
    size_t capacity;
    size_t start;
    size_t endIndex;
    size_t size;
    std::vector<T> buffer;
};

int main() {
    CircularBuffer<int> cb(6);

    cb.add(7);
    cb.add(8);
    cb.add(0);
    cb.add(1);
    cb.add(1);
    cb.add(2);

    for (auto it = cb.begin(); it != cb.end(); ++it) {
        std::cout << *it << " ";
    }
    std::cout << *cb.end() << " ";
    std::cout << std::endl;

    auto it = cb.begin();
    ++it;
    ++it;
    --it;
    std::cout << *it << std::endl;

    return 0;
}
