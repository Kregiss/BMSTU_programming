#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

void csort(const char *src, char *dest) {
    int wordCount = 0;
    const char *word = src;
  
    while (*word != '\0') {
        while (*word == ' ' && *word != '\0') {
            word++;
        }
        if (*word != '\0') {
            wordCount++;
        }
        while (*word != ' ' && *word != '\0') {
            word++;
        }
    }

    int pos = 0;
    char **words = (char **)malloc(wordCount * sizeof(char *));
    const char *ptr = src;
    for (int i = 0; i < wordCount; ++i) {
        while (*ptr == ' ' && *ptr != '\0') {
            ptr++;
        }
        words[i] = (char *)ptr;
        while (*ptr != ' ' && *ptr != '\0') {
            ptr++;
        }
    }

    int *lengths = (int*)malloc(wordCount * sizeof(int));
    for (int i = 0; i < wordCount; ++i) {
        int length = 0;
        const char* word = words[i];
        while (*word != ' ' && *word != '\n' && *word != '\0') {
            length++;
            word++;
        }
        lengths[i] = length; 
    }

    for (int i = 1; i < wordCount; i++) {
        int j = i;
        while (j > 0 && lengths[j - 1] > lengths[j]) {
            
            int tempLength = lengths[j];
            lengths[j] = lengths[j - 1];
            lengths[j - 1] = tempLength;
            
            char *tempWord = words[j];
            words[j] = words[j - 1];
            words[j - 1] = tempWord;
            
            j--;
        }
    }
    
    for (int i = 0; i < wordCount; i++) {
        if (i == 0) {
            strncpy(dest, words[i], lengths[i]);
            dest[lengths[i]] = '\0';
        } else {
            strncat(dest, " ", 1);
            strncat(dest, words[i], lengths[i]);
        }
    }

    free(words);
    free(lengths);
}

int main() {

    char *input = (char*)malloc(1000 * sizeof(char));
  
    fgets(input, 1000, stdin);
  
    char *output = (char*)malloc(1000 * sizeof(char));
  
    csort(input, output);
  
    printf("%s\n", output);
  
    free(input);
    free(output);

    return 0;
}


// qqq  www  t  aa rrr  bb  x y zz
// Answer: t x y aa bb zz qqq www rrr