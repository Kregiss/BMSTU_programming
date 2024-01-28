#include <stdio.h>
#include <stdlib.h>

#define MAX_N 100000
#define HASH_SIZE 65536

typedef struct Node {
    long long value;
    long long count;
    struct Node* next;
} Node;

Node* hashTable[HASH_SIZE];

int hash(long long key) {
    return abs((int)(key % HASH_SIZE));
}

Node* find(Node* head, long long key) {
    Node* current = head;
    while (current != NULL) {
        if (current->value == key) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

void insert(Node** head, long long key, long long value) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (!newNode) {
        exit(EXIT_FAILURE);
    }
    newNode->value = key;
    newNode->count = value;
    newNode->next = *head;
    *head = newNode;
}

int main() {
    int n;
    scanf("%d", &n);

    long long* arr = (long long*)malloc(n * sizeof(long long));
    if (!arr) {
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < n; i++) {
        scanf("%lld", &arr[i]);
    }

    long long result = 0;
    long long currentXOR = 0;

    insert(&hashTable[hash(currentXOR)], currentXOR, 1);

    for (int i = 0; i < n; i++) {
        currentXOR ^= arr[i];

        Node* current = find(hashTable[hash(currentXOR)], currentXOR);
        if (current != NULL) {
            result += current->count;
        }

        insert(&hashTable[hash(currentXOR)], currentXOR, 1 + (current != NULL ? current->count : 0));
    }

    printf("%lld\n", result);

    // Освобождаем память, выделенную под таблицу
    for (int i = 0; i < HASH_SIZE; i++) {
        Node* current = hashTable[i];
        while (current != NULL) {
            Node* temp = current;
            current = current->next;
            free(temp);
        }
    }

    free(arr);

    return 0;
}

// 6 0 14 14 2 2 0
// 10
