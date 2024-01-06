#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LENGTH 100

int getOverlap(char *str1, char *str2) {
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    for (int overlap = len2; overlap > 0; overlap--) { 
        if (len1 >= overlap && strncmp(str1 + len1 - overlap, str2, overlap) == 0) {
            return overlap; 
        }
    }
    return 0;
}

int getShortestSuperstringLength(char **strings, int n) {
    int overlap[n][n], masks[n][n], dp[1 << n][n], parent[1 << n][n];
    memset(dp, -1, sizeof(dp));
    memset(parent, -1, sizeof(parent));

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) { 
                overlap[i][j] = getOverlap(strings[i], strings[j]);
                masks[i][j] = (1 << i) | (1 << j);
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        dp[1 << i][i] = strlen(strings[i]);
    }

    for (int mask = 0; mask < (1 << n); mask++) {
        for (int j = 0; j < n; j++) {
            if (mask & (1 << j)) {
                for (int k = 0; k < n; k++) {
                    if (mask & (1 << k)) continue; 
                    int next_mask = mask | (1 << k);
                    int new_length = dp[mask][j] + strlen(strings[k]) - overlap[j][k];
                    if (dp[next_mask][k] == -1 || new_length < dp[next_mask][k]) {
                        dp[next_mask][k] = new_length;
                        parent[next_mask][k] = j;
                    }
                }
            }
        }
    }

    int min_super_len = MAX_LENGTH * n, last_idx = -1;
    for (int j = 0; j < n; j++) {
        if (min_super_len > dp[(1 << n) - 1][j]) {
            min_super_len = dp[(1 << n) - 1][j];
            last_idx = j;
        }
    }
    
    return min_super_len;
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

    int shortestSuperstringLength = getShortestSuperstringLength(strings, n);
    printf("%d", shortestSuperstringLength);

    for (i = 0; i < n; i++)
        free(strings[i]);
    free(strings);

    return 0;
}

// 2
// odvoqi
// vovood
// Ответ: 10