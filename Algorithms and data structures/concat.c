#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

char *concat(char **s, int n)
{
    int total_length = 0;
    for (int i = 0; i < n; i++) {
        total_length += strlen(s[i]);
    }
    
    char *result = (char*)malloc((total_length + 1) * sizeof(char));
    result[total_length] = '\0';  // Ensure null-termination

    
    int current_position = 0;
    for (int i = 0; i < n; i++) {
        strcpy(result + current_position, s[i]);
        current_position += strlen(s[i]);
    }
    
    return result;
}

int main()
{
    int n;
    scanf("%d", &n);
    
    char **s = (char**)malloc(n * sizeof(char*));
    
    for (int i = 0; i < n; i++) {
        s[i] = (char*)calloc(1001, sizeof(char));
        scanf("%1000s", s[i]);
    }

    char *result = concat(s, n);
    
    printf("%s\n", result);
    
    for (int i = 0; i < n; i++) { free(s[i]); }
    free(s);
    free(result);
    
    return 0;
}