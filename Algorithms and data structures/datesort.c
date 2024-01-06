#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

struct Date {
    int Day, Month, Year;
};

// Прототипы функций
void radixSort(struct Date *dates, int n);
int getKey(struct Date date, int digit);
void countingSort(struct Date *dates, int n, int digit);

// Функция getKey извлекает ключ в зависимости от разряда
int getKey(struct Date date, int digit) {
    switch(digit) {
        case 0: return date.Year - 1970;
        case 1: return date.Month - 1;
        case 2: return date.Day - 1;
    }
}

// Сортировка подсчетом для определенного разряда
void countingSort(struct Date *dates, int n, int digit) {
    int range = (digit == 0) ? 2030 - 1970 + 1 : (digit == 1) ? 12 : 31;
    int i, key;
    struct Date *output = (struct Date*)malloc(n * sizeof(struct Date));
    int *count = (int*)calloc(range, sizeof(int));

    for (i = 0; i < n; i++) {
        key = getKey(dates[i], digit);
        count[key]++;
    }

    for (i = 1; i < range; i++) {
        count[i] += count[i - 1];
    }

    for (i = n - 1; i >= 0; i--) {
        key = getKey(dates[i], digit);
        output[count[key] - 1] = dates[i];
        count[key]--;
    }

    for (i = 0; i < n; i++) {
        dates[i] = output[i];
    }

    free(output);
    free(count);
}

void radixSort(struct Date *dates, int n) {
    for (int digit = 2; digit >= 0; digit--) {
        countingSort(dates, n, digit);
    }
}

int main() {
    int n;
    scanf("%d", &n);

    struct Date *dates = (struct Date*)malloc(n * sizeof(struct Date));

    for (int i = 0; i < n; i++) {
        scanf("%04d %02d %02d", &dates[i].Year, &dates[i].Month, &dates[i].Day);
    }

    radixSort(dates, n);

    for (int i = 0; i < n; i++) {
        printf("%04d %02d %02d\n", dates[i].Year, dates[i].Month, dates[i].Day);
    }

    free(dates);
    return 0;
}

// 5
// 2005 01 12
// 1977 02 01
// 1994 03 01
// 2004 02 29
// 1977 08 01

// 1977 02 01
// 1977 08 01
// 1994 03 01
// 2004 02 29
// 2005 01 12