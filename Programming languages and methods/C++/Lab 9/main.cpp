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

#include <iostream>
#include "Seq.h"

int main() {
    Seq<int> seq1({1, 3, 5, 7, 9});
    Seq<int> seq2({2, 3, 5, 8});
    Seq<int> seq4({2, -7, 5, -8});

    Seq<int> mergeResult = seq1 + seq4;
    std::cout << "Слияние: ";
    for (int i = 0; i < mergeResult.size(); ++i) {
        std::cout << mergeResult[i] << " ";
    }
    std::cout << std::endl;

    Seq<int> intersectionResult = seq1 * seq4;
    std::cout << "Пересечение: ";
    for (int i = 0; i < intersectionResult.size(); ++i) {
        std::cout << intersectionResult[i] << " ";
    }
    std::cout << std::endl;

    Seq<int> differenceResult = seq1 - seq2;
    std::cout << "Разность последовательностей: ";
    for (int i = 0; i < differenceResult.size(); ++i) {
        std::cout << differenceResult[i] << " ";
    }
    std::cout << std::endl;
    
    std::cout << "First element: " << seq1[0] << std::endl;
    std::cout << "Third element: " << seq1[2] << std::endl;

    return 0;
}
