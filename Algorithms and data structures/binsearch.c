#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

unsigned long binsearch(unsigned long nel, int (*compare)(unsigned long i))
{
    unsigned long long right = nel - 1, left = 0, middle = (right + left) / 2;

    while (left <= right)
    {
        if (compare(middle) == 0) { return middle; }
        if (compare(middle) == 1) {  right = middle - 1; }
        else{ left = middle + 1; }
        middle = (right + left) / 2;
    }

    return nel;
}

/*
int array[] = { 1, 2, 30, 45, 50, 51, 55, 60 }; 
const int k = 51; 
 
int compare(unsigned long i) 
{ 
        if (array[i] == k) return 0; 
        if (array[i] < k) return -1; 
        return 1; 
}

int main(int argc, char **argv) 
{ 
        printf("%lu\n", binsearch(8, compare)); 
        return 0; 
}
*/