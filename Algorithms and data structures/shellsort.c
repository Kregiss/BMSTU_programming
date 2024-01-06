#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

void shellsort(unsigned long nel,
               int (*compare)(unsigned long i, unsigned long j),
               void (*swap)(unsigned long i, unsigned long j)) 
{
    if (nel > 1) {
        unsigned long fib_curr = 1, ln_list = 1;
        unsigned long *list_fib = NULL;
        list_fib = (unsigned long*)malloc(2 * sizeof(unsigned long));
        list_fib[0] = 0;

        while (fib_curr < nel) {
            ln_list++;
            list_fib = (unsigned long*)realloc(list_fib, ln_list * sizeof(unsigned long));
            list_fib[ln_list - 1] = fib_curr;
            fib_curr = list_fib[ln_list - 1] + list_fib[ln_list - 2];
        }
        
        unsigned long k = ln_list - 1;
        while (list_fib[k] > 0) {
            unsigned long step = list_fib[k];
            for (unsigned long i = step; i < nel; ++i) {
                unsigned long j = i;
                
                while ((j >= step) && (compare(j - step, j) > 0)) {
                    swap(j - step, j);
                    if (j < step) { break; }
                    j -= step;
                }
            }
            
            unsigned long temp = fib_curr - step;
            fib_curr = step;
            step = temp;

            k++;
        }

        free(list_fib);
        list_fib = NULL;
    }
}

#include <stdio.h>
#include <stdlib.h>


void shellsort(unsigned long nel,
               int (*compare)(unsigned long i, unsigned long j),
               void (*swap)(unsigned long i, unsigned long j)) 
{
    if (nel > 1) {
        // последовательность Фибоначчи
        unsigned long first = 1, second = 1;
        while (second < nel) { 
            unsigned long tmp = second;
            second = first + second;
            first = tmp;
        }

        while (first > 0) {
            unsigned long step = first;
            for (unsigned long i = step; i < nel; ++i) {
                for (long j = i; j >= step && compare(j - step, j) > 0; j -= step) {
                    swap(j - step, j);
                }
            }

            unsigned long tmp = second - first;
            second = first;
            first = tmp;
        }
    }
}