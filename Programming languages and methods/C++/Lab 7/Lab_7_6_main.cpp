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

int main() {
    RationalMatrix mat(3);
    mat.getElement(0, 0) = Rational(1, 2);
    mat.getElement(0, 1) = Rational(3, 4);
    mat.getElement(0, 2) = Rational(5, 6);
    mat.getElement(1, 0) = Rational(7, 8);
    mat.getElement(1, 1) = Rational(9, 10);
    mat.getElement(1, 2) = Rational(11, 12);
    mat.getElement(2, 0) = Rational(13, 14);
    mat.getElement(2, 1) = Rational(15, 16);
    mat.getElement(2, 2) = Rational(17, 18);

    cout << "Первоначальная матрица:" << std::endl;
    for (int i = 0; i < mat.getOrder(); ++i) {
        for (int j = 0; j < mat.getOrder(); ++j) {
            cout << mat.getElement(i, j) << " ";
        }
        cout << std::endl;
    }

    cout << "Порядок матрицы: " << mat.getOrder() << std::endl;

    cout << "Элемент в (1, 1): " << mat.getElement(1, 1) << std::endl;

    RationalMatrix submat = mat.getSubmatrix(1, 1);
    cout << "Подматрица после преобразования:" << std::endl;
    for (int i = 0; i < submat.getOrder(); ++i) {
        for (int j = 0; j < submat.getOrder(); ++j) {
            cout << submat.getElement(i, j) << " ";
        }
        cout << std::endl;
    }

    cout << "Определитель матрицы: " << mat.determinant() << std::endl;

    return 0;
}