#include <iostream>
#include "Matrix.h"

int main() {
    constexpr size_t M = 3.0;
    constexpr size_t N = 3.0;

    Matrix<M, N> mat;
    mat(0, 0) = 1.1; mat(0, 1) = 2.1; mat(0, 2) = 3.1;
    mat(1, 0) = 4.1; mat(1, 1) = 5.1; mat(1, 2) = 6.1;
    mat(2, 0) = 7.1; mat(2, 1) = 8.1; mat(2, 2) = 9.1;

    std::cout << "Начальная матрица:" << std::endl;
    mat.print();
    
    int& elem = mat(1, 1);
    std::cout << "Элемент в (1,1): " << elem << std::endl;
    
    Matrix<M, N> squaredMat = mat.square();

    std::cout << "Квадратная матрица:" << std::endl;
    squaredMat.print();
        
    constexpr size_t M2 = 2;
    constexpr size_t N2 = 3;

    Matrix<M2, N2> mat2;
    mat2(0, 0) = 1; mat2(0, 1) = 2; mat2(0, 2) = 3;
    mat2(1, 0) = 4; mat2(1, 1) = 5; mat2(1, 2) = 6;

    std::cout << "Начальная матрица:" << std::endl;
    mat2.print();
    

    return 0;
}
