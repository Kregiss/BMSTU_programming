#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    int *data;
    int *maxData;
    int capacity;
    int top;
} Stack;

bool InitStack(Stack *s, int capacity) {
    s->data = (int *)malloc(capacity * sizeof(int));
    if (!s->data) // Проверка на NULL
        return false;

    s->maxData = (int *)malloc(capacity * sizeof(int));
    if (!s->maxData) { // Проверка на NULL
        free(s->data); // Освобождаем выделенную память для data
        return false;
    }

    s->capacity = capacity;
    s->top = -1;
    return true;
}

bool StackEmpty(Stack *s) {
    return s->top == -1;
}

bool StackFull(Stack *s) {
    return s->top == s->capacity - 1;
}

void Push(Stack *s, int x, bool *error) {
    if (StackFull(s)) { 
        if (error) *error = true;
        return; 
    }
    if (error) *error = false;
    s->data[++s->top] = x;
    s->maxData[s->top] = (s->top == 0) ? x : ((s->maxData[s->top - 1] > x) ? s->maxData[s->top - 1] : x);
}


int Pop(Stack *s, bool *error) {
    if (StackEmpty(s)) {
        if (error) *error = true;
        return 0;
    }
    if (error) *error = false;
    return s->data[s->top--];
}

int Maximum(Stack *s, bool *error) {
    if (StackEmpty(s)) {
        if (error) *error = true;
        return 0; 
    }
    if (error) *error = false;
    return s->maxData[s->top];
}

void FreeStack(Stack *s) {
    free(s->data);
    free(s->maxData);
}

typedef struct {
    Stack mainStack;
    Stack helperStack; 
} QueueMax;

// Инициализация очереди с максимумом
void InitQueueMax(QueueMax *q, int capacity) {
    InitStack(&q->mainStack, capacity);
    InitStack(&q->helperStack, capacity);
}

void Enqueue(QueueMax *q, int x) {
    bool error;
    Push(&q->mainStack, x, &error); 
    if (error) {
        exit(EXIT_FAILURE);
    }
}

int Dequeue(QueueMax *q) {
    bool error;
    int poppedValue;
    if (StackEmpty(&q->helperStack)) {
        while (!StackEmpty(&q->mainStack)) {
            int temp = Pop(&q->mainStack, &error);
            if (error) {
                exit(EXIT_FAILURE);
            }
            Push(&q->helperStack, temp, &error);
            if (error) {
                exit(EXIT_FAILURE);
            }
        }
    }
    poppedValue = Pop(&q->helperStack, &error);
    if (error) {
        exit(EXIT_FAILURE);
    }
    return poppedValue;
}

bool QueueEmpty(QueueMax *q) {
    return StackEmpty(&q->mainStack) && StackEmpty(&q->helperStack);
}

int MaximumInQueue(QueueMax *q) {
    bool error;
    int maxVal;
    
    if (QueueEmpty(q)) {
        exit(EXIT_FAILURE);
    }
    
    if (StackEmpty(&q->mainStack)) {
        maxVal = Maximum(&q->helperStack, &error);
    } else if (StackEmpty(&q->helperStack)) {
        maxVal = Maximum(&q->mainStack, &error);
    } else {
        int maxMainStack = Maximum(&q->mainStack, &error);
        if (error) {
            exit(EXIT_FAILURE);
        }
        int maxHelperStack = Maximum(&q->helperStack, &error);
        if (error) {
            exit(EXIT_FAILURE);
        }
        maxVal = (maxMainStack > maxHelperStack) ? maxMainStack : maxHelperStack;
    }
    
    if (error) {
        exit(EXIT_FAILURE);
    }
    
    return maxVal;
}

int main() {
    QueueMax queue;
    InitQueueMax(&queue, 10000000);

    char operation[10];
    int value;

    while (scanf("%s", operation) == 1) { 
        if (strcmp(operation, "ENQ") == 0) {
            if (scanf("%d", &value) != 1) {
                break;
            }
            Enqueue(&queue, value);
        } else if (strcmp(operation, "DEQ") == 0) {
            if (QueueEmpty(&queue)) {
                printf("Queue is empty\n");
            } else {
                printf("%d\n", Dequeue(&queue));
            }
        } else if (strcmp(operation, "MAX") == 0) {
            if (QueueEmpty(&queue)) {
                printf("Queue is empty\n");
            } else {
                printf("%d\n", MaximumInQueue(&queue));
            }
        } else if (strcmp(operation, "EMPTY") == 0) {
            printf("%s\n", QueueEmpty(&queue) ? "true" : "false");
        } else if (strcmp(operation, "END") == 0) {
            break;
        }
    }

    FreeStack(&queue.mainStack);
    FreeStack(&queue.helperStack);

    return 0;
}