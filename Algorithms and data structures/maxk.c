#include <stdio.h>

int main()
{
    int n, k;

    scanf("%d", &n);
    int ls[n];
    for (int i = 0; i < n; i++)
    {
        int x;
        scanf("%d", &x);
        ls[i] = x;
    }

    scanf("%d", &k);

    // самая первая сумма k элементов
    int last_x = ls[0];
    long sm_last = 0;
    for (int j = 0; j < k; j++)
    {
        sm_last += ls[j];
    }
    //printf("%ld", sm_last);

    long mx_sum = sm_last;
    for (int l = k; l < n; l++)
    {
        sm_last = sm_last - last_x + ls[l];
        last_x = ls[l - k + 1];
        if (sm_last > mx_sum){
            mx_sum = sm_last;
        }
    }
    printf("%ld", mx_sum);

    return 0;
}