/*
Последовательность множеств с константным двунаправленным итератором по 
пересечениям соседних множеств последовательности. Обращение к элементам 
последовательности должно осуществляется с помощью перегруженной операции «[ ]».
*/

#include <iostream>
#include <set>
#include <algorithm>

template<typename T>
class SetSequence {
private:
    std::set<T> *sets;
    size_t size;

public:
    SetSequence(std::set<T> *sets, size_t size) : sets(sets), size(size) {}

    const std::set<T>& operator[](size_t index) const {
        return sets[index];
    }

    size_t getSize() const {
        return size;
    }

    class IntersectionIterator {
    private:
        const SetSequence<T> &sequence;
        size_t current;

    public:
        IntersectionIterator(const SetSequence<T> &sequence, size_t current) : sequence(sequence), current(current) {}

        bool operator==(const IntersectionIterator& other) const {
            return &sequence == &other.sequence && current == other.current;
        }

        bool operator!=(const IntersectionIterator& other) const {
            return !(*this == other);
        }

        IntersectionIterator& operator++() {
            ++current;
            return *this;
        }

        IntersectionIterator& operator--() {
            --current;
            return *this;
        }

        const std::set<T>& operator*() const {
            static std::set<T> intersection;
            intersection.clear();
            if (current < sequence.getSize() - 1) {
                std::set_intersection(sequence[current].begin(), sequence[current].end(),
                                      sequence[current + 1].begin(), sequence[current + 1].end(),
                                      std::inserter(intersection, intersection.begin()));
            }
            return intersection;
        }
    };

    IntersectionIterator begin() const {
        return IntersectionIterator(*this, 0);
    }

    IntersectionIterator end() const {
        return IntersectionIterator(*this, size - 1);
    }
};



int main() {
    std::set<int> set1 = {1, 2, 3};
    std::set<int> set2 = {2, 3, 4};
    std::set<int> set3 = {3, 4, 5};

    SetSequence<int> sequence(&set1, 3);

    for (const auto& intersection : sequence) {
        std::cout << "Intersection: ";
        for (const auto& element : intersection) {
            std::cout << element << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}