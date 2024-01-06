#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#define MAX_SIZE 100000

void merge(int arr[], int left[], int left_size, int right[], int right_size) {
    int i = 0, j = 0, k = 0;

    while (i < left_size && j < right_size) {
        if (abs(left[i]) <= abs(right[j])) {
            arr[k] = left[i];
            i++;
        } else {
            arr[k] = right[j];
            j++;
        }
        k++;
    }

    while (i < left_size) {
        arr[k] = left[i];
        i++;
        k++;
    }

    while (j < right_size) {
        arr[k] = right[j];
        j++;
        k++;
    }
}

void mergeSort(int arr[], int size) {
    if (size < 5) {

        // Используем сортировку вставками для последовательностей длиной меньше 5
        for (int i = 1; i < size; i++) {
            int key = arr[i];
            int j = i - 1;
            while (j >= 0 && abs(arr[j]) > abs(key)) {
                arr[j + 1] = arr[j];
                j--;
            }
            arr[j + 1] = key;
        }
    } else {
        int mid = size / 2;
        int *left = (int *)malloc(mid * sizeof(int));
        int *right = (int *)malloc((size - mid) * sizeof(int));

        for (int i = 0; i < mid; i++)
            left[i] = arr[i];

        for (int i = mid; i < size; i++)
            right[i - mid] = arr[i];

        mergeSort(left, mid);
        mergeSort(right, size - mid);

        merge(arr, left, mid, right, size - mid);

        free(left);
        free(right);
    }
}

int main() {
    int arr[MAX_SIZE];
    int size;

    scanf("%d", &size);

    for (int i = 0; i < size; i++)
        scanf("%d", &arr[i]);

    mergeSort(arr, size);

    for (int i = 0; i < size; i++)
        printf("%d ", arr[i]);

    return 0;
}