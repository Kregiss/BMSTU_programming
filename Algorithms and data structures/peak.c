#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

unsigned long peak(unsigned long nel, int (*less)(unsigned long i, unsigned long j))
{
    unsigned long num = 0, right = 1, left;

    if (nel == 1) { return 0; }
    else
    {
        if (less(num, right) == 0) { return num; }
        else
        {
            while (right < nel)
            {
                if ((less(num, right) == 0) && (less(num, left) == 0)) { return num; }
                
                left = num;
                num++;
                right++;
            }
            if (less(num, left) == 0) { return num; }
        }
    }
}

int array[] = {
	210,
	462,
	175,
	169
};

int less(unsigned long i, unsigned long j)
{
	return array[i] < array[j];
}

int main(int argc, char **argv)
{
	int i = peak(4, less);
	if ((i == 0 || array[i] >= array[i-1]) &&
		(i == 3 || array[i] >= array[i+1])) {
		printf("CORRECT, %i\n", i);
	} else {
		/* Если функция peak работает правильно,
		сюда никогда не будет передано
		управление! */
		printf("WRONG\n");
	}
	return 0;
}