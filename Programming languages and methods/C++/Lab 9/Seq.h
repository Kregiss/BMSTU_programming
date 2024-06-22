/*
Seq<T> – последовательность отсортированных по возрастанию значений типа T. 
(Подразумевается, что для типа T определены операции «<» и «==».) Операции:

    1. «+» – слияние двух последовательностей в одну;
    2. «∗» – пересечение двух последовательностей (в результирующей 
    последовательности остаются только элементы, общие для двух 
    пересекаемых последовательностей);
    3. «−» – разность последовательностей (результирующая 
    последовательность содержит элементы, присутствующие в первом 
    операнде и отсутствующие во втором);
    4. «[ ]» – получение i-го элемента последовательности.
*/

#include <vector>
#include <algorithm>

template<typename T>
class Seq {
private:
    std::vector<T> sequence;


public:
    Seq() {}

    Seq(const std::vector<T>& elements) : sequence(elements) {
        std::sort(sequence.begin(), sequence.end());
    }
    
    void sort() {
        quickSort(sequence, 0, sequence.size() - 1);
    }
    
    

    Seq operator+(const Seq& other) const {
        std::vector<T> result(sequence);
        result.insert(result.end(), other.sequence.begin(), other.sequence.end());
        std::sort(result.begin(), result.end());
        return Seq(result);
    }

    Seq operator*(const Seq& other) const {
        std::vector<T> result;
        std::set_intersection(sequence.begin(), sequence.end(),
                              other.sequence.begin(), other.sequence.end(),
                              std::back_inserter(result));
        return Seq(result);
    }

    Seq operator-(const Seq& other) const {
        std::vector<T> result;
        std::set_difference(sequence.begin(), sequence.end(),
                            other.sequence.begin(), other.sequence.end(),
                            std::back_inserter(result));
        return Seq(result);
    }

    T operator[](int index) const {
        return sequence[index];
    }

    size_t size() const {
        return sequence.size();
    }
};