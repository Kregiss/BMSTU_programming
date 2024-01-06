#include <stdio.h>
#include <string.h>

// Функция для вычисления префикс-функции (массива π)
void ComputePrefixFunction(const char *S, int S_length, int *pi) {
    int length = 0;  // длина текущего собственного префикса
    pi[0] = 0;
    for (int i = 1; i < S_length; i++) {
        while (length > 0 && S[length] != S[i]) {
            length = pi[length - 1];
        }
        if (S[length] == S[i]) {
            length++;
        }
        pi[i] = length;
    }
}

// Функция для поиска всех вхождений подстроки S в строке T
void KMPSubst(const char *S, const char *T, int *k) {
    int S_length = strlen(S);
    int T_length = strlen(T);
    int pi[S_length];

    ComputePrefixFunction(S, S_length, pi); // Вычисляем префикс-функцию
    int q = 0; // номер символа в S
    *k = 0; // номер символа в T
    while (*k < T_length) {
        while (q > 0 && S[q] != T[*k]) {
            q = pi[q - 1];
        }
        if (S[q] == T[*k]) {
            q++;
        }
        if (q == S_length) {
            printf("%d\n", *k - S_length + 1);
            q = pi[q - 1]; // Сбрасываем позицию в S
        }
        (*k)++;
    }
}

int main(int argc, char *argv[]) {
    
    char *pattern = argv[1];
    char *text = argv[2];

    int k;

    KMPSubst(pattern, text, &k);

    return 0;
}

// aacaaabcd
// aab
// Answer: 4