void bubblesort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j)) 
{
    unsigned long left_border = 0, right_border = nel - 1;
    int swapped;

    if (nel != 0)
    {
        do {
            // Проход слева направо
            swapped = 0;
            for (unsigned long i = left_border; i < right_border; i++) {
                if (compare(i + 1, i) < 0) {
                    swap(i + 1, i);
                    swapped = 1;
                }
            }
            if (!swapped) break;

            right_border--;

            // Проход справа налево
            swapped = 0;
            for (unsigned long i = right_border; i > left_border; i--) {
                if (compare(i, i - 1) < 0) {
                    swap(i, i - 1);
                    swapped = 1;
                }
            }
            
            left_border++;
        } while (swapped);
    }
}
