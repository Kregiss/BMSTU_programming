/*
Квадратная матрица, элементы которой являются рациональными числами, с операциями: 
1. получение порядка матрицы; 
2. получение ссылки на указанный элемент; 
3. формирование подматрицы, полученной путём вычёркивания i-той строки и j-го столбца; 
4. вычисление определителя матрицы. 

Для представления рациональных чисел требуется реализовать класс нормализованных дробей с 
необходимыми арифметическими операциями. 
Конструктор матрицы должен принимать в качестве параметра её порядок и формировать нулевую матрицу. 
*/

#include <iostream>

class Rational {
private:
    int numerator;
    int denominator;

public:
    Rational(int num = 0, int denom = 1);
    Rational(const Rational& other);
    int abs(int n);
    ~Rational();
    Rational& operator=(const Rational& other);
    void normalize();
    int findGCD(int a, int b);
    Rational operator+(const Rational& other) const;
    Rational operator-(const Rational& other) const;
    Rational operator*(const Rational& other) const;
    Rational operator/(const Rational& other) const;
    friend std::ostream& operator<<(std::ostream& os, const Rational& rat);
};

class RationalMatrix {
private:
    int order;
    Rational** data;

public:
    RationalMatrix(int n);
    RationalMatrix(const RationalMatrix& other);
    ~RationalMatrix();
    RationalMatrix& operator=(const RationalMatrix& other);
    int getOrder() const;
    Rational& getElement(int row, int col);
    RationalMatrix getSubmatrix(int row, int col) const;
    Rational determinant() const;
};
