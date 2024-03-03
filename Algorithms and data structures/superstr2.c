#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LENGTH 100

int findOverlap(char *str1, char *str2) {
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    for (int overlap = len2; overlap > 0; overlap--) {
        if (len1 >= overlap && strncmp(str1 + len1 - overlap, str2, overlap) == 0) {
            return overlap;
        }
    }
    return 0;
}

char *findShortestSuperstring(char **A, int n) {
    int g[n][n];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            g[i][j] = strlen(A[j]);
            for (int k = 1; k <= (strlen(A[i]) < strlen(A[j]) ? strlen(A[i]) : strlen(A[j])); k++) {
                if (strncmp(A[i] + strlen(A[i]) - k, A[j], k) == 0) {
                    g[i][j] = strlen(A[j]) - k;
                }
            }
        }
    }

    int dp[1 << n][n];
    int parent[1 << n][n];
    memset(dp, 0x3F, sizeof(dp));
    memset(parent, -1, sizeof(parent));

    for (int i = 0; i < n; i++) {
        dp[1 << i][i] = strlen(A[i]);
    }

    for (int s = 1; s < (1 << n); s++) {
        for (int j = 0; j < n; j++) {
            if (!(s & (1 << j))) continue;
            int ps = s & ~(1 << j);
            for (int i = 0; i < n; i++) {
                if (dp[s][j] > dp[ps][i] + g[i][j]) {
                    dp[s][j] = dp[ps][i] + g[i][j];
                    parent[s][j] = i;
                }
            }
        }
    }

    int min_super_len = MAX_LENGTH * n;
    int last_idx = -1;
    for (int j = 0; j < n; j++) {
        if (min_super_len > dp[(1 << n) - 1][j]) {
            min_super_len = dp[(1 << n) - 1][j];
            last_idx = j;
        }
    }

    char *result = (char *)malloc(sizeof(char) * MAX_LENGTH * n);
    result[0] = '\0';
    int cur_idx = last_idx;
    int s = (1 << n) - 1;
    while (s) {
        int pre_idx = parent[s][cur_idx];
        if (pre_idx < 0) {
            strncat(result, A[cur_idx], strlen(A[cur_idx]));
        } else {
            strncat(result, A[cur_idx] + strlen(A[cur_idx]) - g[pre_idx][cur_idx], g[pre_idx][cur_idx]);
        }
        s &= ~(1 << cur_idx);
        cur_idx = pre_idx;
    }

    return result;
}

int main() {
    int n, i;
    char **strings;

    scanf("%d", &n);

    strings = (char **)malloc(sizeof(char *) * n);
    for (i = 0; i < n; i++) {
        strings[i] = (char *)malloc(sizeof(char) * MAX_LENGTH);
        scanf("%s", strings[i]);
    }

    char *shortestSuperstring = findShortestSuperstring(strings, n);
    printf("%d", strlen(shortestSuperstring));

    for (i = 0; i < n; i++)
        free(strings[i]);
    free(strings);
    free(shortestSuperstring);

    return 0;
}

/*
2
odvoqi
vovood
*/
// Ответ: 10
