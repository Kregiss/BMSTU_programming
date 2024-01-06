#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

void revarray(void *base, size_t nel, size_t width)
{
    void *ls = malloc(width);
    for (int i = 0; i < (nel / 2); i++)
    {
        char *lefts = (char*)base + i * width, *rights = (char*)base + (nel - i - 1) * width;
        //int a = *((int*)base + (nel - i - 1) * width);
        //*((int*)base + (nel - i - 1) * width) = *((int*)base + i * width);
        //*((int*)base + i * width) = a;

        memcpy(ls,     lefts,  width);
        memcpy(lefts,  rights, width);
        memcpy(rights, ls,     width);
    }

    free(ls);
}

char array[] = {
	10, 20, 30, 40, 50, 60
};

unsigned long size = 6;

int main(int argc, char **argv)
{
    revarray(array, size, sizeof(char));

    int i;
    for (i = 0; i < size; i++) {
            printf("%i\n", array[i]);
    }

    return 0;
}