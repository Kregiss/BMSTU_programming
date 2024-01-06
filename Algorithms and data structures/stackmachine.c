#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STACK_SIZE 100000

typedef struct {
    int stack[MAX_STACK_SIZE];
    int top;
} Stack;

int isStackEmpty(Stack *s) {
    return s->top == 0;
}

void push(Stack *s, int value) {
    if (s->top < MAX_STACK_SIZE) {
        s->stack[s->top++] = value;
    }
}

int pop(Stack *s) {
    if (s->top > 0) {
        return s->stack[--(s->top)];
    }
}

int main() {
    Stack stack;
    stack.top = 0;

    char operation[10];
    int operand;

    while (1) {
        scanf("%s", operation);

        if (strcmp(operation, "END") == 0) {
            break;
        }

        if (strcmp(operation, "CONST") == 0) {
            scanf("%d", &operand);
            push(&stack, operand);
        } else if (strcmp(operation, "ADD") == 0) {
            push(&stack, pop(&stack) + pop(&stack));
        } else if (strcmp(operation, "SUB") == 0) {
            int a = pop(&stack);
            int b = pop(&stack);
            push(&stack, a - b);
        } else if (strcmp(operation, "MUL") == 0) {
            push(&stack, pop(&stack) * pop(&stack));
        } else if (strcmp(operation, "DIV") == 0) {
            int a = pop(&stack);
            int b = pop(&stack);
            if (b != 0) {
                push(&stack, a / b);
            } else {
                return EXIT_FAILURE;
            }
        } else if (strcmp(operation, "NEG") == 0) {
            push(&stack, -pop(&stack));
        } else if (strcmp(operation, "MAX") == 0) {
            int a = pop(&stack);
            int b = pop(&stack);
            push(&stack, a > b ? a : b);
        } else if (strcmp(operation, "MIN") == 0) {
            int a = pop(&stack);
            int b = pop(&stack);
            push(&stack, a < b ? a : b);
        } else if (strcmp(operation, "DUP") == 0) {
            int top = pop(&stack);
            push(&stack, top);
            push(&stack, top);
        } else if (strcmp(operation, "SWAP") == 0) {
            int a = pop(&stack);
            int b = pop(&stack);
            push(&stack, a);
            push(&stack, b);
        }
    }

    printf("%d", stack.stack[stack.top - 1]);

    return 0;
}