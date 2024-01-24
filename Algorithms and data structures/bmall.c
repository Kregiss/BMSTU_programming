#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#define ALPHABET_LEN 94 // Количество печатных ASCII символов от 33 до 126 включительно
#define ASCII_OFFSET 33 // Смещение ASCII для первого печатного символа

int mx(int a, int b) {
    return (a > b) ? a : b;
}

// Функция создания таблицы смещений бед-характер (bad-character) шага
void Delta1(char* S, int* delta1) {
    int lenS = strlen(S);
    for (int i = 0; i < ALPHABET_LEN; i++) {
        delta1[i] = lenS; // По умолчанию смещение равно длине подстроки
    }
    for (int i = 0; i < lenS - 1; i++) {
        delta1[S[i] - ASCII_OFFSET] = lenS - 1 - i;
    }
}

void Suffix(char* S, int* sigma) {
    int length = strlen(S);
    int i, t;

    sigma[length - 1] = t = (int)length - 1;
    i = (int)length - 2;

    while (i >= 0) {
        while (t < (int)length - 1 && S[t] != S[i]) {
            t = sigma[t + 1];
        }
        if (S[t] == S[i]) {
            t--;
        }
        sigma[i] = t;
        i--;
    }
}

void Delta2(char* S, int* delta2) {
    int length = (int)strlen(S);
    int* sigma = (int*)malloc(length * sizeof(int));
    Suffix(S, sigma);

    int i = 0;
    int t = sigma[0];

    while (i < length) {
        while (t < i) {
            t = sigma[t + 1];
        }
        delta2[i] = t + length - i; // Случай 2
        i++;
    }

    i = 0;
    while (i < length - 1) {
        t = i;
        do {
            t = sigma[t + 1];
        } while (t < length - 1 && S[i] == S[t]);

        if (S[i] != S[t]) {
            delta2[t] = length - (i + 1); // Случай 1
        }
        i++;
    }

    free(sigma);
}

// Алгоритм Бойера-Мура
void BMSubst(char* S, char* T) {
    int lenS = strlen(S);
    int lenT = strlen(T);

    // Используем Delta1 для создания таблицы смещений
    int delta1[ALPHABET_LEN];
    Delta1(S, delta1);

    // Используем Delta2 для создания второй таблицы смещений    
    int* delta2 = (int*)malloc(lenS * sizeof(int));
    Delta2(S, delta2);

    int k = lenS - 1;
    while (k < lenT) {
        int i = lenS - 1;
        while (i >= 0 && T[k] == S[i]) {
            if (i == 0) {
                printf("%d ", k);
                break;
            }
            --i;
            --k;
        }
        int delta = mx(delta1[T[k] - ASCII_OFFSET], delta2[i]);
        k += delta;
    }

    free(delta2);
}


int main(int argc, char* argv[]) {

    char* S = argv[1];
    char* T = argv[2];

    BMSubst(S, T);

    return 0;
}

/* 
at.that
which.finally_.halts...at.that.point
*/
// 23

// eKeyK
// e7qqeK1eKeK3eqJleKeyKe4lP4eKe42e1SeKeyKqFeKeJ1eKe3eKeyFeKPeKeybe
// 16 34
