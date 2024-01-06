#include <stdio.h>

int main()
{
    int k_str, k_kol;
    scanf("%d %d", &k_str, &k_kol);
    int ls[k_str + 1][k_kol + 1];

    long int mn = 10;

    
    for (int i = 0; i < k_str; i++)
    {
        for (int j = 0; j < k_kol; j++)
        {
            long int n;
            scanf("%ld", &n);

            if (i == 0){ ls[k_str][j] = n; }
            if (j == 0){ ls[i][k_kol] = n; }

            ls[i][j] = n;

            if (ls[i][k_kol] < ls[i][j]){ 
                ls[i][k_kol] = ls[i][j];  //находим максимумы в строках
            }
            if (ls[k_str][j] > ls[i][j]){
                ls[k_str][j] = ls[i][j]; // находим минимумы в столбцах
            }
        }
    }

    //for (int k = 0; k <= k_str; k++){
    //    for (int r = 0; r <= k_kol; r++){
    //        printf("%ld\t", ls[k][r]);
    //    }
    //    printf("\n");
    //}

    // проходимся по крайним строке и столбцу и ищем одинаковые числа
    int fl = 0;
    for (int q = 0; q < k_str; q++) // по строкам (максимумы)
    {
        long int a = ls[q][k_kol];

        for (int w = 0; w < k_kol; w++) // по столбцам (минимумы)
        {
            long int b = ls[k_str][w];

            if (a == b){
                printf("%d %d", q, w);
                fl = 1;
                break;
            }
        }

        if (fl == 1){ break; }
    }

    if (fl == 0){ printf("none"); }

    return 0;
}