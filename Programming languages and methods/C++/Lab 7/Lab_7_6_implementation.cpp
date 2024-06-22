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

#include "Lab_7_6_declaration.h"
#include <iostream>
using namespace std;
Rational::Rational(int num, int denom) : numerator(num), denominator(denom) {
    normalize();
}

Rational::Rational(const Rational& other) : numerator(other.numerator), denominator(other.denominator) {}

Rational::~Rational() {}

Rational& Rational::operator=(const Rational& other) {
    if (this != &other) {
        numerator = other.numerator;
        denominator = other.denominator;
    }
    return *this;
}

int Rational::abs(int n) {
    return n < 0 ? -n : n;
}

void Rational::normalize() {
    if (denominator < 0) {
        numerator *= -1;
        denominator *= -1;
    }
    int gcd = findGCD(abs(numerator), denominator);
    numerator /= gcd;
    denominator /= gcd;
}

int Rational::findGCD(int a, int b) { // НОД
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

Rational Rational::operator+(const Rational& other) const {
    return Rational(numerator * other.denominator + other.numerator * denominator, denominator * other.denominator);
}

Rational Rational::operator-(const Rational& other) const {
    return Rational(numerator * other.denominator - other.numerator * denominator, denominator * other.denominator);
}

Rational Rational::operator*(const Rational& other) const {
    return Rational(numerator * other.numerator, denominator * other.denominator);
}

Rational Rational::operator/(const Rational& other) const {
    return Rational(numerator * other.denominator, denominator * other.numerator);
}

std::ostream& operator<<(std::ostream& os, const Rational& rat) {
    os << rat.numerator;
    if (rat.denominator != 1) {
        os << "/" << rat.denominator;
    }
    return os;
}





RationalMatrix::RationalMatrix(int n) : order(n) {
    data = new Rational*[n];
    for (int i = 0; i < n; ++i) {
        data[i] = new Rational[n];
    }
}

RationalMatrix::RationalMatrix(const RationalMatrix& other) : order(other.order) {
    data = new Rational*[order];
    for (int i = 0; i < order; ++i) {
        data[i] = new Rational[order];
        for (int j = 0; j < order; ++j) {
            data[i][j] = other.data[i][j];
        }
    }
}

RationalMatrix::~RationalMatrix() {
    for (int i = 0; i < order; ++i) {
        delete[] data[i];
    }
    delete[] data;
}

RationalMatrix& RationalMatrix::operator=(const RationalMatrix& other) {
    if (this != &other) {
        for (int i = 0; i < order; ++i) {
            delete[] data[i];
        }
        delete[] data;

        order = other.order;
        data = new Rational*[order];
        for (int i = 0; i < order; ++i) {
            data[i] = new Rational[order];
            for (int j = 0; j < order; ++j) {
                data[i][j] = other.data[i][j];
            }
        }
    }
    return *this;
}

int RationalMatrix::getOrder() const {
    return order;
}

Rational& RationalMatrix::getElement(int row, int col) {
    return data[row][col];
}

RationalMatrix RationalMatrix::getSubmatrix(int row, int col) const {
    RationalMatrix submatrix(order - 1);
    for (int i = 0, k = 0; i < order; ++i) {
        if (i == row) continue;
        for (int j = 0, l = 0; j < order; ++j) {
            if (j == col) continue;
            submatrix.getElement(k, l) = data[i][j];
            ++l;
        }
        ++k;
    }
    return submatrix;
}

Rational RationalMatrix::determinant() const {
    if (order == 1) {
        return data[0][0];
    }
    Rational det;
    for (int j = 0; j < order; ++j) {
        RationalMatrix submatrix = getSubmatrix(0, j);
        det = det + (data[0][j] * (j % 2 == 0 ? 1 : -1) * submatrix.determinant());
    }
    return det;
}