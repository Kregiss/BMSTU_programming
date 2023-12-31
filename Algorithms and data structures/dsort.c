#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Функция, которая предоставляет ключ элемента (в этом примере ключом является сам элемент)
int key(char element) {
    return element - 'a';  // предполагаем, что строки содержат только маленькие латинские буквы
}

// Функция сортировки распределением (Counting Sort)
void DistributionSort(char *S, int n, int m) {
    // Этап 1: Инициализация массива подсчета
    int *count = (int*)calloc(m, sizeof(int));  // массив счётчиков
    
    // Этап 2: Подсчет элементов
    for (int j = 0; j < n; j++) {
        count[key(S[j])]++;
    }
    
    // Этап 3: Обновление массива подсчета так, чтобы он содержал позиции
    for (int i = 1; i < m; i++) {
        count[i] += count[i - 1];
    }
    
    // Этап 4: Создание массива для отсортированного результата
    char *D = (char*)malloc(n * sizeof(char)); // выходная последовательность
    
    // Этап 5: Распределение элементов в выходном массиве
    for (int j = n - 1; j >= 0; j--) {
        int k = key(S[j]);
        D[--count[k]] = S[j];  // уменьшаем count[k] сразу перед использованием в качестве индекса
    }
    
    // Копирование отсортированной последовательности обратно в S
    memcpy(S, D, n * sizeof(char));
    S[n] = '\0';
    
    // Освободим выделенную память
    free(D);
    free(count);
}

int main() {
    char S[1000001];  // максимальная длина строки - 1 миллион
    fgets(S, sizeof(S), stdin);  // читаем строку из стандартного потока ввода
    
    int n = strlen(S); // удаляем символ новой строки '\n' из длины строки, если он есть
    if (S[n-1] == '\n') {
        S[--n] = 0; // заменяем символ новой строки на нулевой символ и уменьшаем длину строки
    } 
    int m = 26;  // количество латинских букв
    
    DistributionSort(S, n, m);
    
    // Печать отсортированной строки
    printf("%s", S);
    
    return 0;
}

// encyclopedia
// accdeeilnopy