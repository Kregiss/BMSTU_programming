#include <stdio.h>

int main()
{
    long x0, n, sm1 = 0, sm2 = 0;
    //sm1 решение полинома
    //sm2 решение производной полинома
    scanf("%ld", &n);
    scanf("%ld", &x0);
    for (long i = n; i >= 0; i--)
    {
        long ai;
        scanf("%ld", &ai);
        if (i == 0){
            sm1 += ai;
        }
        else{
            sm1 = (sm1 + ai) * x0;

            if (i == 1){
                sm2 += ai * i;
            }
            else{
                sm2 = (sm2 + ai * i) * x0;
            }
        }
    }
    printf("%ld %ld", sm1, sm2);
    return 0;
}