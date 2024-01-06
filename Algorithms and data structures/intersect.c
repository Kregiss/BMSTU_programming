#include <stdio.h>

int main()
{
    int x_a = 0, y_b = 0, A, B;

    scanf("%d", &A);
    for (int i = 0; i < A; i++)
    {
        int x;
        scanf("%d", &x);
        x_a |= (1 << x);
    }

    scanf("%d", &B);
    for (int j = 0; j < B; j++)
    {
        int y;
        scanf("%d", &y);
        y_b |= (1 << y);
    }
    
    //printf("%lld\n%lld\n", x_a, y_b);

    int c = x_a & y_b;

    for (int r = 0; r < 32; r++)
    {
        if (c & (1 << r)){ printf("%d ", r); }
    }

    return 0;
}