#include <stdio.h>
#include <stdlib.h>

typedef struct {
    size_t startTime;
    size_t executionTime;
} Task;

typedef struct {
    size_t *heap;
    size_t count, capacity, timeOfWork;
} Cluster;

int Compare(size_t a, size_t b, Cluster *cluster) {
    if (cluster->heap[a] == cluster->heap[b]) return 0;
    return (cluster->heap[a] > cluster->heap[b]) ? 1 : -1;
}

void Swap(size_t a, size_t b, Cluster *cluster) {
    size_t temp = cluster->heap[a];
    cluster->heap[a] = cluster->heap[b];
    cluster->heap[b] = temp;
}

void SiftDown(size_t i, Cluster *cluster) {
    while (1) {
        size_t left = 2 * i + 1;
        size_t right = 2 * i + 2;
        size_t smallest = i;

        if (left < cluster->count && Compare(left, smallest, cluster) < 0) {
            smallest = left;
        }
        if (right < cluster->count && Compare(right, smallest, cluster) < 0) {
            smallest = right;
        }
        if (smallest == i) {
            break;
        }
        Swap(i, smallest, cluster);
        i = smallest;
    }
}

void ClusterInit(size_t capacity, Cluster *cluster) {
    cluster->heap = (size_t *)malloc(capacity * sizeof(size_t));
    cluster->capacity = capacity;
    cluster->count = 0;
    cluster->timeOfWork = 0;
}

void DecreaseKey(size_t i, size_t newKey, Cluster *cluster) {
    if (cluster->heap[i] > newKey) {
        cluster->heap[i] = newKey;
        while (i != 0 && Compare((i - 1) / 2, i, cluster) > 0) {
            Swap(i, (i - 1) / 2, cluster);
            i = (i - 1) / 2;
        }
    }
}

void ClusterInsert(size_t startTime, size_t executionTime, Cluster *cluster) {
    size_t i = cluster->count;

    if (cluster->count >= cluster->capacity) {
        // Находим время окончания работы для новой задачи
        size_t earliestCompletionTime = cluster->heap[0];
        size_t workCompletionTime = earliestCompletionTime + executionTime;
        
        // Учитываем задержку старта, если задача начинается позже
        if (startTime > earliestCompletionTime) {
            workCompletionTime = startTime + executionTime;
        }

        // Сравниваем с текущим временем работы кластера и обновляем при необходимости
        if (cluster->timeOfWork < workCompletionTime) {
            cluster->timeOfWork = workCompletionTime;
        }

        // Устанавливаем новое время окончания работы для кучи и просеиваем вниз
        cluster->heap[0] = workCompletionTime;
        SiftDown(0, cluster);
    } else {
        // Добавляем новую задачу, если есть свободное место
        cluster->heap[i] = startTime + executionTime;
        cluster->count++;

        // Сохраняем инвариант минимальной кучи
        size_t parent = (i - 1) / 2;
        while (i > 0 && cluster->heap[i] < cluster->heap[parent]) {
            Swap(i, parent, cluster);
            i = parent;
            parent = (parent - 1) / 2;
        }

        // Обновляем время работы кластера, если текущая задача заканчивается позже
        if(cluster->timeOfWork < cluster->heap[i]) {
            cluster->timeOfWork = cluster->heap[i];
        }
    }
}

size_t ClusterGetTimeOfWork(Cluster *cluster) {
    return cluster->timeOfWork;
}

void ClusterFree(Cluster *cluster) {
    free(cluster->heap);
}

int main() {
    size_t N, M;
    scanf("%zu %zu", &N, &M);

    Task *tasks = (Task *)malloc(M * sizeof(Task));
    for (size_t i = 0; i < M; i++) {
        scanf("%zu %zu", &tasks[i].startTime, &tasks[i].executionTime);
    }

    Cluster cluster;
    ClusterInit(N > M ? M : N, &cluster);

    for (size_t i = 0; i < M; i++) {
        ClusterInsert(tasks[i].startTime, tasks[i].executionTime, &cluster);
    }

    size_t resultTime = ClusterGetTimeOfWork(&cluster);
    printf("%zu\n", resultTime);

    free(tasks);
    ClusterFree(&cluster);

    return 0;
}