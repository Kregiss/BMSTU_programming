#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int count_a(const char* str) {
    int count = 0;
    while (*str) {
        if (*str == 'a') {
            count++;
        }
        str++;
    }
    return count;
}

int compare_strings_by_a_count(const void *a, const void *b) {
    const char *str_a = *(const char**)a;
    const char *str_b = *(const char**)b;
    int count_a_str_a = count_a(str_a);
    int count_a_str_b = count_a(str_b);
    return count_a_str_a - count_a_str_b;
}

void heapify(void *base, size_t nel, size_t width, int (*compare)(const void *, const void *), int i) {
    char *arr = base;
    int largest = i; 
    int left = 2 * i + 1; 
    int right = 2 * i + 2; 

    if (left < nel && compare(arr + (left * width), arr + (largest * width)) > 0)
        largest = left;

    if (right < nel && compare(arr + (right * width), arr + (largest * width)) > 0)
        largest = right;

    if (largest != i) {
        char temp[width];
        memcpy(temp, arr + (i * width), width);
        memcpy(arr + (i * width), arr + (largest * width), width);
        memcpy(arr + (largest * width), temp, width);
        heapify(arr, nel, width, compare, largest);
    }
}

void buildHeap(void *base, size_t nel, size_t width, int (*compare)(const void *, const void *)) {
    for (int i = nel / 2 - 1; i >= 0; i--)
        heapify(base, nel, width, compare, i);
}

void hsort(void *base, size_t nel, size_t width, int (*compare)(const void *a, const void *b)) {
    buildHeap(base, nel, width, compare);

    for (size_t i = nel - 1; i > 0; i--) {
        char temp[width];
        memcpy(temp, base, width);
        memcpy(base, (char*)base + (i * width), width);
        memcpy((char*)base + (i * width), temp, width);

        heapify(base, i, width, compare, 0);
    }
}

int main() {
    size_t n;
    scanf("%zu\n", &n);

    char *arr[n];
    char buffer[1000];
    for (size_t i = 0; i < n; i++) {
        if (fgets(buffer, 1000, stdin) == NULL) {
            fprintf(stderr, "Error reading string\n");
            exit(1);
        }
        buffer[strcspn(buffer, "\n")] = 0; // Удаляем символ новой строки
        arr[i] = strdup(buffer);
    }

    hsort(arr, n, sizeof(char*), compare_strings_by_a_count);

    printf("Отсортированный массив строк:\n");
    for (size_t i = 0; i < n; i++) {
        printf("%s\n", arr[i]);
        free(arr[i]); // Освобождаем выделенную память
    }

    return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

void swap(void *a, void *b, size_t width) {
    char *p1 = a;
    char *p2 = b;
    char temp;

    for (size_t i = 0; i < width; ++i) {
        temp = p1[i];
        p1[i] = p2[i];
        p2[i] = temp;
    }
}

int compare_strings(const void *a, const void *b) {
    const char *str_a = *(const char **)a;
    const char *str_b = *(const char **)b;

    int count_a = 0;
    int count_b = 0;

    for (int i = 0; str_a[i] != '\0'; i++) {
        if (str_a[i] == 'a')
            count_a++;
    }

    for (int i = 0; str_b[i] != '\0'; i++) {
        if (str_b[i] == 'a')
            count_b++;
    }

    return count_a - count_b;
}

void heapify(void *arr, size_t nel, size_t width, int (*compare)(const void *, const void *), int parent) {
    int largest = parent;
    int left = 2 * parent + 1;
    int right = 2 * parent + 2;
    char *base = (char *)arr; 

    if (left < nel && compare(base + left * width, base + largest * width) > 0) {
        largest = left;
    }

    if (right < nel && compare(base + right * width, base + largest * width) > 0) {
        largest = right;
    }

    if (largest != parent) {
        swap(base + parent * width, base + largest * width, width);
        heapify(base, nel, width, compare, largest);
    }
}


void hsort(void *base, size_t nel, size_t width, int (*compare)(const void *a, const void *b)) {
    char *p_base = (char *)base;

    for (int i = nel / 2 - 1; i >= 0; i--) {
        heapify(p_base, nel, width, compare, i);
    }

    for (int i = nel - 1; i > 0; i--) {
        swap(p_base, p_base + i * width, width);
        heapify(p_base, i, width, compare, 0);
    }
}

int main() {
    int buffer_size = 1000;
    char input_buffer[buffer_size];

    fgets(input_buffer, buffer_size, stdin);
    int num_strings = atoi(input_buffer);

    char **strings = (char **)malloc(num_strings * sizeof(char *));
    for (int i = 0; i < num_strings; i++) {
        strings[i] = (char *)malloc(buffer_size * sizeof(char));
        fgets(strings[i], buffer_size, stdin);
    }

    hsort(strings, num_strings, sizeof(char *), compare_strings);

    for (int i = 0; i < num_strings; i++) {
        printf("%s", strings[i]);
        free(strings[i]);
    }

    free(strings);
    return 0;
}