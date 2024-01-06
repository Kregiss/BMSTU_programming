#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int count_comboinations(int arr[], int n, int sum) 
{
    int count = 0, power2[25];
    for (int i = 0; i <= 24; i++) 
    {
        power2[i] = pow(2, i);
    }

    for (int i = 1; i < (1 << n); i++) 
    {
        int subsetSum = 0;
        for (int j = 0; j < n; j++) 
        {
            if (i & (1 << j)) { subsetSum += arr[j]; }
        }
        for (int j = 0; j <= 24; j++) 
        {
            if (subsetSum == power2[j]) 
            {
                count++;
                break;
            }
        }
    }

    return count;
}

int main() 
{
    int n;
    scanf("%d", &n);
    int arr[24];
    for (int i = 0; i < n; i++) 
    {
        scanf("%d", &arr[i]);
    }

    int result = count_comboinations(arr, n, 0);
    printf("%d", result);

    return 0;
}