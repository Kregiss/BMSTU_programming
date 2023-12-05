#include <stdio.h>

int main()
{
    unsigned long long a, b, m, sm = 0;
    scanf("%llu %llu %llu", &a, &b, &m);

    // перевод числа b в двоичную запись
    int b_bin[64] = {0};

    // перевод b в двоичную запись (она будет записана наоборот)
    int k = 0;  // длина двоичной записи числа b
    while (b > 0) {
        b_bin[k] = b % 2;
        //printf("%d", b_bin[i]);
        b /= 2;
        k += 1;
    }
    
    // вычисление решения выражения
    for (int i = 63; i >= 0; i--)
    {
        sm = (((sm % m) * 2) % m) + (((a % m) * b_bin[i]) % m);
    }
    printf("%llu", sm % m);

    return 0;
}