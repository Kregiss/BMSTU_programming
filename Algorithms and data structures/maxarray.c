#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

int maxarray(void *base, size_t nel, size_t width, int (*compare)(void *a, void *b))
{
    void *mx = malloc(width);
    memcpy(mx, base, width);
    int index = 0;

    //int *mx = ((int*)base + 0 * width), index = 0;

    for (int i = 1; i < nel; i++)
    {
        void *x = ((char*)base + i * width);
        if (compare(mx, x) < 0)
        {
            memcpy(mx, x, width);
            index = i; 
        }
    }
    free(mx);
    return index;
}

unsigned char array[] = {
	153,
	1,
	15,
	191,
	232,
	251,
	27,
	174,
	26,
	3,
	68,
	48
};

int compare(void *a, void *b)
{
	return (int)(*(unsigned char*)a) - (int)(*(unsigned char*)b);
}

int main(int argc, char **argv)
{
	printf("%d\n", maxarray(array, 12, sizeof(unsigned char), compare));
	return 0;
}