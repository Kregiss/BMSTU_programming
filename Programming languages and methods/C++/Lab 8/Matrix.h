/*
Matrix<M,N> – целочисленная матрица размера M × N с операцией, возвращающей ссылку 
на указанный элемент. Если M = N, то для матрицы должна быть доступна операция 
возведения в квадрат.
*/


#include <iostream>
#include <vector>
#include <stdexcept>

template<size_t M, size_t N>
class Matrix {
    std::vector<int> data;

public:
    Matrix() : data(M * N, 0) {}

    int& operator()(size_t row, size_t col) {
        return data[row * N + col];
    }

    const int& operator()(size_t row, size_t col) const {
        return data[row * N + col];
    }

    void print() const {
        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                std::cout << (*this)(i, j) << " ";
            }
            std::cout << std::endl;
        }
    }

    template<size_t M2 = M, size_t N2 = N>
    typename std::enable_if<M2 == N2, Matrix<M, N>>::type square() const {
        Matrix<M, N> result;

        for (size_t i = 0; i < M; ++i) {
            for (size_t j = 0; j < N; ++j) {
                for (size_t k = 0; k < N; ++k) {
                    result(i, j) += (*this)(i, k) * (*this)(k, j);
                }
            }
        }

        return result;
    }
};
