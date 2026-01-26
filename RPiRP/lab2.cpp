#include <mpi.h>
#include <vector>
#include <cmath>
#include <iostream>

using std::vector;

int mpi_rank, mpi_size;
int n;
int local_n;       
int local_start;    

double dotProduct(const vector<double>& v1, const vector<double>& v2) {
    double local_sum = 0.0;
    for (int i = 0; i < v1.size(); ++i) local_sum += v1[i] * v2[i];

    double global_sum = 0.0;
    MPI_Allreduce(&local_sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
    return global_sum;
}

double norm(const vector<double>& v) {
    return std::sqrt(dotProduct(v, v));
}

vector<double> multiplyA(const vector<vector<double>>& A_local, const vector<double>& x,
    const vector<int>& recvcounts, const vector<int>& displs) {
    vector<double> local_res(local_n, 0.0);
    for (int i = 0; i < local_n; ++i) {
        double sum = 0.0;
        for (int j = 0; j < n; ++j) sum += A_local[i][j] * x[j];
        local_res[i] = sum;
    }


    vector<double> global_res(n);
    MPI_Allgatherv(local_res.data(), local_n, MPI_DOUBLE,
        global_res.data(), recvcounts.data(), displs.data(), MPI_DOUBLE,
        MPI_COMM_WORLD);
    return global_res;
}

vector<double> solveCG(const vector<vector<double>>& A_local, vector<double> x, const vector<double>& b,
    double eps, const vector<int>& recvcounts, const vector<int>& displs) {

    vector<double> r(local_n, 0.0);
    vector<double> z(local_n, 0.0);

    vector<double> Ax = multiplyA(A_local, x, recvcounts, displs);
    for (int i = 0; i < local_n; ++i) {
        r[i] = b[local_start + i] - Ax[local_start + i];
        z[i] = r[i];
    }

    double rr_old = dotProduct(r, r);
    double norm_b = std::sqrt(dotProduct(vector<double>(b.begin() + 0, b.begin() + n),
        vector<double>(b.begin() + 0, b.begin() + n)));
    if (norm_b == 0.0) norm_b = 1.0;

    int max_iter = n;
    for (int iter = 0; iter < max_iter; ++iter) {
        
        vector<double> z_global(n, 0.0);
        MPI_Allgatherv(z.data(), local_n, MPI_DOUBLE, z_global.data(),
            recvcounts.data(), displs.data(), MPI_DOUBLE, MPI_COMM_WORLD);

        vector<double> Az = multiplyA(A_local, z_global, recvcounts, displs);
        vector<double> Az_local(local_n, 0.0);
        for (int i = 0; i < local_n; ++i) Az_local[i] = Az[local_start + i];

        double Az_z = dotProduct(Az_local, z);
        if (std::isnan(Az_z) || std::isinf(Az_z)) {
            if (mpi_rank == 0) std::cerr << "ERROR: Az_z is NaN/Inf at iter " << iter << "\n";
            break;
        }
        if (Az_z == 0.0) {
            if (mpi_rank == 0) std::cerr << "INFO: Az_z == 0 at iter " << iter << " (breaking)\n";
            break;
        }

        double alpha = rr_old / Az_z;

        for (int i = 0; i < local_n; ++i) {
            x[local_start + i] += alpha * z[i];
            r[i] -= alpha * Az_local[i];
        }

        double rr_new = dotProduct(r, r);
        double rel_res = std::sqrt(rr_new) / norm_b;

        if (rel_res < eps) {
            if (mpi_rank == 0) std::cout << "Converged at iter " << iter << " rel_res=" << rel_res << std::endl;
            break;
        }

        double beta = rr_new / rr_old;
        for (int i = 0; i < local_n; ++i) z[i] = r[i] + beta * z[i];

        rr_old = rr_new;
    } 

    return x;
}

vector<vector<double>> generateLocalMatrix(int local_n, int n, int start_row) {
    vector<vector<double>> A(local_n, vector<double>(n, 1.0));
    for (int i = 0; i < local_n; ++i)
        A[i][start_row + i] = 2.0;
    return A;
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    n = 36864;
    local_n = n / mpi_size;

    int base = n / mpi_size;
    int rem = n % mpi_size;
    local_n = base + (mpi_rank < rem ? 1 : 0);
    local_start = base * mpi_rank + (mpi_rank < rem ? mpi_rank : rem);

    vector<int> recvcounts(mpi_size);
    vector<int> displs(mpi_size);
    for (int i = 0; i < mpi_size; ++i) {
        recvcounts[i] = base + (i < rem ? 1 : 0);
        displs[i] = base * i + (i < rem ? i : rem);
    }

    MPI_Barrier(MPI_COMM_WORLD);
    double t_start = MPI_Wtime();

    vector<vector<double>> A_local = generateLocalMatrix(local_n, n, local_start);
    vector<double> b(n, n + 1.0);   
    vector<double> x(n, 1000.0);  

    double eps = 1e-22;
    x = solveCG(A_local, x, b, eps, recvcounts, displs);

    MPI_Barrier(MPI_COMM_WORLD);
    double t_end = MPI_Wtime();

    if (mpi_rank == 0) {
        std::cout << "Total time = " << t_end - t_start << " sec" << std::endl;

        double err = 0.0;
        for (int i = 0; i < n; ++i) {
            double d = x[i] - 1.0;
            err += d * d;
        }
    }

    MPI_Finalize();
    return 0;
}



// PS C:\Users\1\source\repos\mpi_test> cd .\x64\Debug\
// PS C:\Users\1\source\repos\mpi_test\x64\Debug> mpiexec -n 1 .\mpi_test.exe
