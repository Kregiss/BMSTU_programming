#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *str_duplicate(const char *s) {
    size_t size = strlen(s) + 1;
    char *p = (char*)malloc(size); 
    if (p != NULL) {
        strcpy(p, s);
    }
    return p;
}

char *fibstr(int n) {
    if (n <= 0) {
        return NULL;
    }

    char *a = str_duplicate("a");
    char *b = str_duplicate("b");
    
    if (n == 1) {
        free(b);
        return a;
    } else if (n == 2) {
        free(a);
        return b;
    }

    char *temp;
    for (int i = 3; i <= n; ++i) {
        temp = (char*)malloc(strlen(a) + strlen(b) + 1);
        if (temp == NULL) {
            free(a);
            free(b);
            return NULL;
        }
        strcpy(temp, a);
        strcat(temp, b);
        
        free(a);
        a = b;
        b = temp;
    }

    free(a);
    return b;
}

int main() {
    int n;
    scanf("%d", &n);

    char *fib = fibstr(n);
    if (fib) {
        printf("%s", fib);
        free(fib);
    }

    return 0;
}