// https://www.programiz.com/dsa/quick-sort

#include <stdio.h>

struct Task {
    int low, high;
};

// Функция для обмена элементов массива
void swap(int arr[], int i, int j) {
    int temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
}

// Функция для разделения массива и возврата индекса опорного элемента
int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(arr, i, j);
        }
    }

    swap(arr, i + 1, high);
    return i + 1;
}

void quickSort(int arr[], int n) {
    struct Task stack[n];
    int top = -1;

    stack[++top].low = 0;
    stack[top].high = n - 1;

    while (top >= 0) {
        int low = stack[top].low;
        int high = stack[top].high;
        top--;

        int pivotIndex = partition(arr, low, high);

        if (pivotIndex - 1 > low) {
            stack[++top].low = low;
            stack[top].high = pivotIndex - 1;
        }

        if (pivotIndex + 1 < high) {
            stack[++top].low = pivotIndex + 1;
            stack[top].high = high;
        }
    }
}

int main() {
    int n;
    scanf("%d", &n);

    int arr[n];

    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }

    quickSort(arr, n);

    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }

    return 0;
}
