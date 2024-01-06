#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

void swap(int *a, int *b) {
    int t = *a;
    *a = *b;
    *b = t;
}

int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = (low - 1);
    
    for (int j = low; j <= high - 1; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
}

void insertionSort(int arr[], int low, int n) {
    int i, key, j;
    for (i = low + 1; i <= n; i++) {
        key = arr[i];
        j = i - 1;

        while (j >= low && arr[j] > key) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = key;
    }
}

void quickSortRec(int arr[], int low, int high, int m) {
    while (low < high) {
        if (high - low < m) {
            insertionSort(arr, low, high);
            break;
        } else {
            int pi = partition(arr, low, high);

            if (pi - low < high - pi) {
                quickSortRec(arr, low, pi - 1, m);
                low = pi + 1;
            } else {
                quickSortRec(arr, pi + 1, high, m);
                high = pi - 1;
            }
        }
    }
}

void quickSort(int arr[], int n, int m) {
    quickSortRec(arr, 0, n - 1, m);
}

int main() {
    int n;
    int m;
    scanf("%d %d", &n, &m);

    int *arr = (int*)malloc(n * sizeof(int));

    for(int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }

    quickSort(arr, n, m);

    for(int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }

    printf("\n");
    free(arr);
    return 0;
}

// 8 9 1 6 48 5 9 1 98 74 156
// 1 1 5 6 9 48 74 98 156