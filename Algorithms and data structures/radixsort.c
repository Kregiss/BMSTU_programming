#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h> // Для использования INT_MIN и т.д.
#include <malloc.h>

// Объявляем объединение для доступа к отдельным байтам целых чисел
union Int32 {
    int x;
    unsigned char bytes[4];
};

// Cмещаем знаковый бит для корректной работы с отрицательными числами
int getByte(union Int32 key, int byteIndex) {
    // Если это старший байт, который содержит знаковый бит, то смещаем его значения
    if (byteIndex == 3) {
        return (int)(key.bytes[byteIndex] ^ 0x80);
    } else {
        return (int)key.bytes[byteIndex];
    }
}

// Сортировка подсчетом для определенного байта
void countingSort(union Int32 *array, int size, int byteIndex) {
    int output[size];  // Временный массив для хранения отсортированных элементов
    int count[256] = {0};  // Поскольку байт может принимать значения от 0 до 255
    int i;
    union Int32 temp;

    // Подсчитываем количество вхождений каждого байта
    for (i = 0; i < size; i++) {
        count[getByte(array[i], byteIndex)]++;
    }

    // Изменяем count[i] так, чтобы count[i] содержал позицию данного байта в output[]
    for (i = 1; i < 256; i++) {
        count[i] += count[i - 1];
    }

    // Строим отсортированный массив
    for (i = size - 1; i >= 0; i--) {
        temp = array[i];
        output[count[getByte(temp, byteIndex)] - 1] = temp.x;
        count[getByte(temp, byteIndex)]--;
    }

    // Копируем отсортированный массив в array[], чтобы array[] содержал отсортированный порядок по текущему байту
    for (i = 0; i < size; i++) {
        array[i].x = output[i];
    }
}

// Функция поразрядной сортировки
void radixSort(union Int32 *array, int size) {
    // Поразрядная сортировка для каждого из 4 байтов
    for (int byteIndex = 0; byteIndex < 4; byteIndex++) {
        countingSort(array, size, byteIndex);
    }
}

int main() {
    int n;  // Размер массива
    scanf("%d", &n);
    
    union Int32 *array = (union Int32 *)malloc(n * sizeof(union Int32)); // Выделяем память под массив

    // Чтение входных данных
    for (int i = 0; i < n; i++) {
        scanf("%d", &array[i].x);
    }

    // Вызываем функцию поразрядной сортировки
    radixSort(array, n);

    // Вывод отсортированного массива
    for (int i = 0; i < n; i++) {
        printf("%d ", array[i].x);
    }

    free(array);
    return 0;
}


// 5
// 1000 700 -5000 2038 0

// -5000 0 700 1000 2038