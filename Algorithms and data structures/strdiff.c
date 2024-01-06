#include <stdio.h>

int strdiff(char *a, char *b) {
    int i = 0;

    while (a[i] != '\0' || b[i] != '\0') {
        
        if (a[i] != b[i]) {
            int bit_position = 0;
            unsigned char diff = (unsigned char)a[i] ^ (unsigned char)b[i];

            while ((diff & (1 << bit_position)) == 0) {
                bit_position++;
                if (bit_position == 8) {
                    break;
                }
            }
            
            return i * 8 + bit_position;
        }
        i++;
    }

    return -1;
}