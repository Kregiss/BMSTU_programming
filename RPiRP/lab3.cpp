#include <omp.h>
#include <vector>
#include <cmath>
#include <iostream>

using std::vector;

double dotProduct(const vector<double>& v1, const vector<double>& v2) {
    double sum = 0.0;
    int n = (int)v1.size();

#pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < (int)n; ++i)
        sum += v1[i] * v2[i];

    return sum;
}

double norm(const vector<double>& v) {
    return std::sqrt(dotProduct(v, v));
}

vector<double> multiplyA(const vector<vector<double>>& A, const vector<double>& x) {
    size_t n = A.size();
    size_t m = A[0].size();
    vector<double> res(n, 0.0);

#pragma omp parallel for
    for (int i = 0; i < n; ++i) {
        double s = 0.0;
        for (int j = 0; j < m; ++j)
            s += A[i][j] * x[j];
        res[i] = s;
    }
    return res;
}

vector<double> solveCG(const vector<vector<double>>& A,
    vector<double> x,
    const vector<double>& b,
    double eps)
{
    size_t n = b.size();

    vector<double> r(n), z(n);

    vector<double> Ax = multiplyA(A, x);
#pragma omp parallel for
    for (int i = 0; i < n; ++i) {
        r[i] = b[i] - Ax[i];
        z[i] = r[i];
    }

    double rr_old = dotProduct(r, r);
    double norm_b = norm(b);
    if (norm_b == 0.0) norm_b = 1.0;

    int max_iter = (int)n;
    int iter = 0;

    for (; iter < max_iter; ++iter) {

        vector<double> Az = multiplyA(A, z);
        double Az_z = dotProduct(Az, z);

        if (Az_z == 0.0) {
            std::cerr << "INFO: Az_z == 0 on iter " << iter << "\n";
            break;
        }

        double alpha = rr_old / Az_z;

#pragma omp parallel for
        for (int i = 0; i < n; ++i) {
            x[i] += alpha * z[i];
            r[i] -= alpha * Az[i];
        }

        double rr_new = dotProduct(r, r);
        double rel_res = std::sqrt(rr_new) / norm_b;

        if (iter % 10 == 0 || iter < 5 || rel_res < eps) {
            std::cout << "iter " << iter << " rel_res=" << rel_res
                << " rr_new=" << rr_new << " Az_z=" << Az_z << "\n";
        }

        if (rel_res < eps) {
            std::cout << "Converged at iter " << iter
                << " rel_res=" << rel_res << "\n";
            break;
        }

        double beta = rr_new / rr_old;

#pragma omp parallel for
        for (int i = 0; i < n; ++i)
            z[i] = r[i] + beta * z[i];

        rr_old = rr_new;
    }

    std::cout << "Finished iterations: " << iter << "\n";

    return x;
}

vector<vector<double>> generateMatrix(int n) {
    vector<vector<double>> A(n, vector<double>(n, 1.0));
    for (int i = 0; i < n; ++i)
        A[i][i] = 2.0;
    return A;
}

int main() {

    int n = 36864;
    omp_set_num_threads(16);   

    vector<vector<double>> A = generateMatrix(n);
    vector<double> b(n, n + 1.0);
    vector<double> x(n, 1000.0);

    double eps = 1e-22;

    double t_start = omp_get_wtime();
    x = solveCG(A, x, b, eps);
    double t_end = omp_get_wtime();

    std::cout << "Total time = " << t_end - t_start << " sec\n";

    double err = 0.0;
#pragma omp parallel for reduction(+:err)
    for (int i = 0; i < n; ++i) {
        double d = x[i] - 1.0;
        err += d * d;
    }
    std::cout << "RMSE = " << std::sqrt(err / n) << "\n";

    return 0;
}

// .\rprp_lab3.exe