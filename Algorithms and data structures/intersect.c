#include <stdio.h>

int main()
{
    int x_a = 0, y_b = 0, mx = 0;
    int A, B;
    
    scanf("%d", &A);
    for (int i = 0; i < A; i++)
    {
        int x;
        scanf("%d", &x);
        if (mx < x) mx = x;
        x_a |= (1 << x);
    }
    
    scanf("%d", &B);
    for (int j = 0; j < B; j++)
    {
        int y;
        scanf("%d", &y);
        if (mx < y) mx = y;
        y_b |= (1 << y);
    }
    
    int c = x_a & y_b;
    for (int r = 0; r <= mx; r++)
    {
        if (c & (1 << r)){
            printf("%d ", r);
        }
    }

    return 0;
}
