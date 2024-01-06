#include <stdio.h>
#include <stdlib.h>

struct Elem {
    struct Elem *prev, *next;
    int v;
};

void InitDoubleLinkedList(struct Elem *sentinel) {
    sentinel->prev = sentinel;
    sentinel->next = sentinel;
}

int ListEmpty(const struct Elem *sentinel) {
    return sentinel->next == sentinel;
}

int ListLength(const struct Elem *sentinel) {
    int len = 0;
    struct Elem *current = sentinel->next;

    while (current != sentinel) {
        len++;
        current = current->next;
    }

    return len;
}

struct Elem *ListSearch(const struct Elem *sentinel, int value) {
    struct Elem *current = sentinel->next;

    while (current != sentinel && current->v != value) {
        current = current->next;
    }

    return current;
}

void InsertAfter(struct Elem *x, struct Elem *y) {
    struct Elem *z = x->next;
    x->next = y;
    y->prev = x;
    y->next = z;
    z->prev = y;
}

void Delete(struct Elem *x) {
    struct Elem *y = x->prev;
    struct Elem *z = x->next;
    y->next = z;
    z->prev = y;
    x->prev = NULL;
    x->next = NULL;
    free(x);
}

void InsertionSort(struct Elem *sentinel) {
    if (ListEmpty(sentinel) || ListLength(sentinel) == 1) {
        return;
    }

    struct Elem *sortedTail = sentinel;
    struct Elem *current = sentinel->next;

    while (current != sentinel) {
        struct Elem *nextNode = current->next;

        while (sortedTail->next != sentinel && sortedTail->next->v < current->v) {
            sortedTail = sortedTail->next;
        }

        current->prev->next = current->next;
        current->next->prev = current->prev;

        current->prev = sortedTail;
        current->next = sortedTail->next;
        sortedTail->next->prev = current;
        sortedTail->next = current;

        current = nextNode;
    }
}

void PrintList(const struct Elem *sentinel) {
    struct Elem *current = sentinel->next;

    while (current != sentinel) {
        printf("%d ", current->v);
        current = current->next;
    }

    printf("\n");
}

int IsIncreasingList(const struct Elem *sentinel) {
    struct Elem *current = sentinel->next;

    while (current->next != sentinel) {
        if (current->v > current->next->v) {
            return 0;
        }
        current = current->next;
    }

    return 1;
}

int main() {
    struct Elem sentinel;
    InitDoubleLinkedList(&sentinel);

    int n;
    scanf("%d", &n);

    for (int i = 0; i < n; i++) {
        struct Elem *newNode = (struct Elem *)malloc(sizeof(struct Elem));
        scanf("%d", &newNode->v);
        InsertAfter(&sentinel, newNode);
    }


    do {
        InsertionSort(&sentinel);
    } while (IsIncreasingList(&sentinel) != 1);

    PrintList(&sentinel);

    struct Elem *current = sentinel.next;
    while (current != &sentinel) {
        struct Elem *nextNode = current->next;
        free(current);
        current = nextNode;
    }

    return 0;
}