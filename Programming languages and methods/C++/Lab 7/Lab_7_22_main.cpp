/*
Последовательность символов ASCII с операциями: 
1. получение количества символов; 
2. получение ссылки на i-тый символ; 
3. вставка нового символа в i-тую позицию последовательности; 
4. проверка, является ли последовательность палиндромом. 
*/

#include "Lab_7_22_declaration.h"
#include <cstring>
#include <iostream>

using namespace std;


int main() {
    CharacterSequence seq;
    seq.insert(0, 'a');
    seq.insert(1, 'b');
    seq.insert(2, 'c');
    seq.insert(2, 'b');
    seq.insert(3, 'a');
    seq.insert(0, 'c');

    std::cout << "Последовательность: ";
    for (int i = 0; i < seq.getLength(); ++i) {
        std::cout << seq[i];
    }
    std::cout << std::endl;
    
    std::cout << "Количество символов в последовательности: " << seq.getLength() << std::endl;

    std::cout << "i-тый символ: " << seq[2] << std::endl;

    if (seq.isPalindrome()) {
        std::cout << "Это палиндром" << std::endl;
    } else {
        std::cout << "Нет, это не палиндром" << std::endl;
    }

    return 0;
}