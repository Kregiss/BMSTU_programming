#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <malloc.h>
#include <string.h>

#define INITIAL_SIZE 4

typedef struct {
    int *data;
    int cap;
    int count;
    int head;
    int tail;
} CircularQueue;

void InitQueue(CircularQueue *q) {
    q->data = (int *)malloc(INITIAL_SIZE * sizeof(int));
    q->cap = INITIAL_SIZE;
    q->count = 0;
    q->head = 0;
    q->tail = 0;
}

bool QueueEmpty(CircularQueue *q) {
    return q->count == 0;
}

void Enqueue(CircularQueue *q, int x) {
    if (q->count == q->cap) {
        int newCap = q->cap * 2;
        int *newData = (int *)malloc(newCap * sizeof(int));

        for (int i = 0; i < q->count; i++) {
            newData[i] = q->data[(q->head + i) % q->cap];
        }

        free(q->data);
        q->data = newData;
        q->head = 0;
        q->tail = q->count;
        q->cap = newCap;
    }

    q->data[q->tail] = x;
    q->tail = (q->tail + 1) % q->cap;
    q->count++;
}

int Dequeue(CircularQueue *q) {
    if (QueueEmpty(q)) {
        return 0;
    }

    int x = q->data[q->head];
    q->head = (q->head + 1) % q->cap;
    q->count--;

    return x;
}

int main() {
    CircularQueue queue;
    InitQueue(&queue);

    char operation[5];
    int value;

    while (1) {
        scanf("%s", operation);

        if (strcmp(operation, "ENQ") == 0) {
            scanf("%d", &value);
            Enqueue(&queue, value);
        } else if (strcmp(operation, "DEQ") == 0) {
            printf("%d\n", Dequeue(&queue));
        } else if (strcmp(operation, "EMPTY") == 0) {
            printf("%s\n", QueueEmpty(&queue) ? "true" : "false");
        } else if (strcmp(operation, "END") == 0) {
            break;
        }
    }

    free(queue.data);

    return 0;
}