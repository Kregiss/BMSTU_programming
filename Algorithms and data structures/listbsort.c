#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Elem {
    struct Elem *next;
    char *word;
};

int compare(struct Elem *a, struct Elem *b) {
    return strlen(a->word) - strlen(b->word);
}

void swap(struct Elem *a, struct Elem *b) {
    if (a && b) {
        char *temp = a->word;
        a->word = b->word;
        b->word = temp;
    }
}

size_t length(struct Elem *head) {
    size_t len = 0;
    while (head) {
        len++;
        head = head->next;
    }
    return len;
}

struct Elem *bsort(struct Elem *head) {
    if (!head) return NULL;

    struct Elem *i, *j;
    int swapped;
    for (i = head; i != NULL; i = i->next) {
        swapped = 0;
        for (j = head; j->next != NULL; j = j->next) {
            if (compare(j, j->next) > 0) {
                swap(j, j->next);
                swapped = 1;
            }
        }
        if (swapped == 0) break;
    }

    return head;
}

void insertAtEnd(struct Elem **list, struct Elem *elem) {
    if (!list || !elem) return;

    elem->next = NULL;

    if (!*list) {
        *list = elem; 
    } else {
        struct Elem *current = *list;
        while (current->next != NULL) {
            current = current->next;
        }
        current->next = elem;
    }
}

void freeList(struct Elem *list) {
    struct Elem *temp;
    while (list != NULL) {
        temp = list;
        list = list->next;
        free(temp->word);
        free(temp);
    }
}

char *myStrdup(const char *src) {
    if (src == NULL) return NULL;

    size_t len = strlen(src) + 1;
    char *dst = (char *)malloc(len);
    if (dst) {
        strcpy(dst, src);
    }
    return dst;
}

int main() {
    struct Elem *list = NULL, *newElem;
    char sentence[1000], *token;

    if (fgets(sentence, sizeof(sentence), stdin) == NULL) {
        exit(EXIT_FAILURE);
    }

    char *p = strchr(sentence, '\n');
    if (p) *p = '\0';

    token = strtok(sentence, " ");
    while (token != NULL) {
        newElem = (struct Elem *)malloc(sizeof(struct Elem));
        if (!newElem) {
            freeList(list);
            exit(EXIT_FAILURE);
        }

        newElem->word = myStrdup(token);
        if (newElem->word == NULL) {
            free(newElem);
            freeList(list);
            exit(EXIT_FAILURE);
        }

        insertAtEnd(&list, newElem);
        token = strtok(NULL, " ");
    }

    list = bsort(list);

    for (struct Elem *current = list; current != NULL; current = current->next) {
        printf("%s ", current->word);
    }
    printf("\n");

    freeList(list);
    return 0;
}


// piomv euaycnyji  u i
// u i piomv euaycnyji

// onewwiho yrocud q vekoe a ah in qi uhe  tyuleot ku
// q a ah in qi ku uhe vekoe yrocud tyuleot onewwiho