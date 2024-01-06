#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int value;
    int arrayIndex;
    int elementIndex;
} ArrayElement;

typedef struct {
    ArrayElement *heap;
    int capacity;
    int size;
} MinHeap;

void initializeMinHeap(MinHeap *minHeap, int capacity) {
    minHeap->heap = (ArrayElement *)malloc(capacity * sizeof(ArrayElement));
    minHeap->capacity = capacity;
    minHeap->size = 0;
}

void swap(ArrayElement *a, ArrayElement *b) {
    ArrayElement temp = *a;
    *a = *b;
    *b = temp;
}

void heapifyUp(MinHeap *minHeap, int index) {
    while (index > 0) {
        int parentIndex = (index - 1) / 2;
        if (minHeap->heap[index].value < minHeap->heap[parentIndex].value) {
            swap(&minHeap->heap[index], &minHeap->heap[parentIndex]);
            index = parentIndex;
        } else {
            break;
        }
    }
}

void insertElement(MinHeap *minHeap, int value, int arrayIndex, int elementIndex) {
    if (minHeap->size < minHeap->capacity) {
        ArrayElement newElement = {value, arrayIndex, elementIndex};
        minHeap->heap[minHeap->size] = newElement;
        heapifyUp(minHeap, minHeap->size);
        minHeap->size++;
    }
}

void heapifyDown(MinHeap *minHeap, int index) {
    while (1) {
        int leftChild = 2 * index + 1;
        int rightChild = 2 * index + 2;
        int smallest = index;

        if (leftChild < minHeap->size && minHeap->heap[leftChild].value < minHeap->heap[smallest].value) {
            smallest = leftChild;
        }

        if (rightChild < minHeap->size && minHeap->heap[rightChild].value < minHeap->heap[smallest].value) {
            smallest = rightChild;
        }

        if (smallest != index) {
            swap(&minHeap->heap[index], &minHeap->heap[smallest]);
            index = smallest;
        } else {
            break;
        }
    }
}

// Функция для извлечения минимального элемента из min-heap
ArrayElement extractMin(MinHeap *minHeap) {
    if (minHeap->size > 0) {
        ArrayElement minElement = minHeap->heap[0];
        minHeap->size--;
        if (minHeap->size > 0) {
            minHeap->heap[0] = minHeap->heap[minHeap->size];
            heapifyDown(minHeap, 0);
        }
        return minElement;
    }
}

void freeMinHeap(MinHeap *minHeap) {
    free(minHeap->heap);
}

// Функция для объединения k отсортированных массивов
int *mergeSortedArrays(int **arrays, int *sizes, int k, int totalSize) {
    int *result = (int *)malloc(totalSize * sizeof(int));

    // Инициализация min-heap
    MinHeap minHeap;
    initializeMinHeap(&minHeap, k);

    for (int i = 0; i < k; i++) {
        if (sizes[i] > 0) {
            insertElement(&minHeap, arrays[i][0], i, 0);
        }
    }

    int resultIndex = 0;

    // Объединение массивов
    while (minHeap.size > 0) {
        ArrayElement minElement = extractMin(&minHeap);

        result[resultIndex] = minElement.value;
        resultIndex++;

        int nextIndex = minElement.elementIndex + 1;
        if (nextIndex < sizes[minElement.arrayIndex]) {
            insertElement(&minHeap, arrays[minElement.arrayIndex][nextIndex], minElement.arrayIndex, nextIndex);
        }
    }

    freeMinHeap(&minHeap);

    return result;
}

void printArray(int *arr, int size) {
    for (int i = 0; i < size; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
}

int main() {
    int k;
    scanf("%d", &k);

    int *sizes = (int *)malloc(k * sizeof(int));
    int totalSize = 0;

    for (int i = 0; i < k; i++) {
        scanf("%d", &sizes[i]);
        totalSize += sizes[i];
    }

    int **arrays = (int **)malloc(k * sizeof(int *));

    for (int i = 0; i < k; i++) {
        arrays[i] = (int *)malloc(sizes[i] * sizeof(int));
        for (int j = 0; j < sizes[i]; j++) {
            scanf("%d", &arrays[i][j]);
        }
    }

    int *result = mergeSortedArrays(arrays, sizes, k, totalSize);

    printArray(result, totalSize);

    free(sizes);
    for (int i = 0; i < k; i++) {
        free(arrays[i]);
    }
    free(arrays);
    free(result);

    return 0;
}