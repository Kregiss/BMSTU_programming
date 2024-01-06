#include <stdio.h>
#include <string.h>
#include <malloc.h>

// Функция для вычисления префикс-функции
void ComputePrefixFunction(const char *S, int *pi) {
    int S_length = strlen(S); 
    pi[0] = 0;
    int k = 0;
    for (int i = 1; i < S_length; i++) {
        while (k > 0 && S[k] != S[i]) {
            k = pi[k - 1];
        }
        if (S[k] == S[i]) {
            k++;
        }
        pi[i] = k;
    }
}

void findPeriodicPrefixes(const char* S) {
    int m = strlen(S);
    int* pi = (int*)malloc(m * sizeof(int));
    ComputePrefixFunction(S, pi);
    for (int i = 1; i < m; ++i) {
        if (pi[i] > 0 && (i + 1) % (i + 1 - pi[i]) == 0) {
            int n = i + 1;
            int k = (i + 1) / (i + 1 - pi[i]);
            printf("%d %d\n", n, k);
        }
    }
    free(pi);
}

int main(int argc, char** argv) {

    char* S = argv[1];
    findPeriodicPrefixes(S);

    return 0;
}