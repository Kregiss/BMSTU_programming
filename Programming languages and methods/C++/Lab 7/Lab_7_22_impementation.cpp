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

CharacterSequence::CharacterSequence(const char* str) : length(strlen(str)) {
    data = new char[length + 1];
    strcpy(data, str);
}

CharacterSequence::CharacterSequence(const CharacterSequence& other) : length(other.length) {
    data = new char[length + 1];
    strcpy(data, other.data);
}

CharacterSequence::~CharacterSequence() {
    delete[] data;
}

CharacterSequence& CharacterSequence::operator=(const CharacterSequence& other) {
    if (this != &other) {
        delete[] data;
        length = other.length;
        data = new char[length + 1];
        strcpy(data, other.data);
    }
    return *this;
}

int CharacterSequence::getLength() const {
    return length;
}

char& CharacterSequence::operator[](int index) {
    return data[index];
}

const char& CharacterSequence::operator[](int index) const {
    return data[index];
}

void CharacterSequence::insert(int index, char ch) {
    if (index >= 0 && index <= length) {
        char* newData = new char[length + 2];
        strncpy(newData, data, index);
        newData[index] = ch;
        strncpy(newData + index + 1, data + index, length - index + 1);
        delete[] data;
        data = newData;
        length++;
    }
}

bool CharacterSequence::isPalindrome() const {
    for (int i = 0; i < length / 2; ++i) {
        if (data[i] != data[length - i - 1]) {
            return false;
        }
    }
    return true;
}