#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

int main(int argc, char **argv)
{
    long long x;
    scanf("%lld", &x);

    // пока идём до максимального запоминаем все предыдущие вычисленные значения fib
    //long long N = 9223372036854775807;
    if (x == 0){ printf("%d", 0); }
    else{
        long long N = 2; 
        long long *arr = NULL;
        arr = (long long*)malloc(N * sizeof(long long));
        arr[0] = 1;
        arr[1] = 1;

        // нашли максимальное число меньшее x
        long long n = 1, a = arr[n] + arr[n - 1], mx_fib = arr[n - 1];
        while (a <= x) {
            mx_fib = a;
            //printf("%ld", arr[n]);
            n = n + 1;
            if (n == N){
                N = N + 1;
                arr = (long long*)realloc(arr, N * sizeof(long long));
                a = arr[n - 1] + arr[n - 2];
            }
            arr[n] = a;
        }
        //printf("%lld", n);

        //n = n - 1;
        while (x > 0) {
            if (arr[n - 1] > x) {
                printf("%d", 0);
            } else {
                printf("%d", 1);
                x = x - arr[n - 1];
            }
            n = n - 1;
        }

        if (n > 1) {
            for (long long i = n - 2; i >= 0; i--) { printf("%d", 0); }
        }
        free(arr);
        arr = NULL;
    }

    return 0;
}