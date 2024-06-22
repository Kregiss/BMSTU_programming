/*
Последовательность символов ASCII с операциями: 
1. получение количества символов; 
2. получение ссылки на i-тый символ; 
3. вставка нового символа в i-тую позицию последовательности; 
4. проверка, является ли последовательность палиндромом. 
*/

class CharacterSequence {
private:
    char* data;
    int length;

public:
    CharacterSequence(const char* str = "");
    CharacterSequence(const CharacterSequence& other);
    ~CharacterSequence();
    CharacterSequence& operator=(const CharacterSequence& other);
    int getLength() const;
    char& operator[](int index);
    const char& operator[](int index) const;
    void insert(int index, char ch);
    bool isPalindrome() const;
};
