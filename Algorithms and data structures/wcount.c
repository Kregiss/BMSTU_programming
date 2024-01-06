#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

int wcount(char *s)
{
    int i = 0, n = (int)strlen(s), count = 0, is_last_char = 0;
    while (i < n)
    {
        if ((s[i] != ' ') && (s[i] != '\t') && (s[i] != '\n') && (s[i] != '\0'))
        {
            if (is_last_char != 1) { count++; }
            is_last_char = 1;
        }
        else { is_last_char = 0; }
        i++;
    }

    return count;
}

int main(int argc, char **argv) 
{
    char* s = (char*)malloc(1000 * sizeof(char));
    fgets(s, 1000, stdin);

    printf("%d\n", wcount(s)); 
    free(s);

    return 0; 
}