package Lab1.java;

import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Lab1 {

    static double[][] randomMatrix(int n, long seed) {
        Random rnd = new Random(seed);
        double[][] M = new double[n][n];
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                M[i][j] = rnd.nextDouble();
            }
        }
        return M;
    }

    // Однопоточная стандартная: строки
    static double[][] multiplyRowMajor(double[][] A, double[][] B) {
        int n = A.length;
        double[][] C = new double[n][n];
        for (int i = 0; i < n; ++i) {
            for (int k = 0; k < n; ++k) {
                double aik = A[i][k];
                for (int j = 0; j < n; ++j) {
                    C[i][j] += aik * B[k][j];
                }
            }
        }
        return C;
    }

    // Однопоточная версия: столбцы
    static double[][] multiplyColMajor(double[][] A, double[][] B) {
        int n = A.length;
        double[][] C = new double[n][n];
        for (int j = 0; j < n; ++j) {
            for (int k = 0; k < n; ++k) {
                double bkj = B[k][j];
                for (int i = 0; i < n; ++i) {
                    C[i][j] += A[i][k] * bkj;
                }
            }
        }
        return C;
    }

    // Многопоточная версия
    static double[][] multiplyParallelBlocks(double[][] A, double[][] B, int numThreads) throws InterruptedException {
        int n = A.length;
        double[][] C = new double[n][n];
        CountDownLatch latch = new CountDownLatch(numThreads);

        int rowsPerThread = n / numThreads;

        for (int t = 0; t < numThreads; ++t) {
            int startRow = t * rowsPerThread;
            int endRow = (t == numThreads - 1) ? n : (startRow + rowsPerThread);

            Thread worker = new Thread(() -> {
                for (int i = startRow; i < endRow; ++i) {
                    for (int k = 0; k < n; ++k) {
                        double aik = A[i][k];
                        double[] Bk = B[k];
                        double[] Ci = C[i];
                        for (int j = 0; j < n; ++j) {
                            Ci[j] += aik * Bk[j];
                        }
                    }
                }
                latch.countDown();
            });
            worker.start();
        }

        latch.await();
        return C;
    }

    static boolean equalMatrices(double[][] X, double[][] Y, double eps) {
        int n = X.length;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                if (Math.abs(X[i][j] - Y[i][j]) > eps) {
                    System.err.printf("Mismatch at [%d,%d]: %g vs %g%n", i, j, X[i][j], Y[i][j]);
                    return false;
                }
            }
        }
        return true;
    }

    static long timeMs(Runnable r) {
        long t0 = System.nanoTime();
        r.run();
        long t1 = System.nanoTime();
        return (t1 - t0) / 1_000_000;
    }

    public static void main(String[] args) throws InterruptedException {
        int n = 2000;

        double[][] A = randomMatrix(n, 12345L);
        double[][] B = randomMatrix(n, 54321L);

        // Однопоточная (строки)
        long tRow = timeMs(() -> {
            double[][] C1 = multiplyRowMajor(A, B);
        });
        System.out.printf("Single-thread row-major multiply: %d ms%n", tRow);

        // Однопоточная (столбцы)
        long tCol = timeMs(() -> {
            double[][] C2 = multiplyColMajor(A, B);
        });
        System.out.printf("Single-thread col-major multiply: %d ms%n", tCol);

        double[][] C_ref = multiplyRowMajor(A, B);


        int[] threadTests = {2, 4, 8};
        for (int t : threadTests) {
            final int threads = t;
            long tPar = timeMs(() -> {
                try {
                    double[][] Cpar = multiplyParallelBlocks(A, B, threads);
                    if (!equalMatrices(Cpar, C_ref, 1e-9)) {
                        System.err.println("Ошибка: параллельный результат не совпадает с эталоном!");
                    }
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
            System.out.printf("Parallel multiply (%d threads): %d ms%n", threads, tPar);
        }

        System.out.println("Done");
    }
}
