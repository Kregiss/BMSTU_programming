#include <stdio.h>

void bubblesort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j))
{
    if (nel != 0){
        unsigned long right_border = nel - 1, left_border = 0, last_right_border, last_left_border;
        int fl = 1;
        for (unsigned long i = 0; i < right_border; i++) {
            for (unsigned long j = 0; j < right_border - i; j++) {

                if (fl == -1) {
                    if (compare(j, j + 1) == 1) { swap(j, j + 1); }
                } else {
                    if (compare(j + 1, j) == -1) { swap(j + 1, j); }
                }
            }
            fl *= -1;
        }
    }
}