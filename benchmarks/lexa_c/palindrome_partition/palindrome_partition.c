#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>
#include <string.h>

i64 invalid_arg_handler(i64*, i64);
FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*, i64*);

i64 invalid_arg_handler(i64 *env, i64 m) {
    printf("Error: invalid argument (%d)\n", m);
    return 1;
}

#define PSIZE 1024
#define PSIZE2 32768

int isPalindrome(char* s, int i, int j) {
    while (i <= j) {
        if (s[i++] != s[--j]) {
            return 0;
        }
    }
    return 1;
}

char* slice(char* s, int i, int j) {
    char* part = (char*)calloc((j - i + 1), sizeof(char));
    strncpy(part, s + i, j - i);
    return part;
}

char** rowDup(char** s, int n) {
    char** newRow = (char**)malloc(PSIZE * sizeof(char*));
    for (int i = 0; i < n; ++i) {
        newRow[i] = s[i];
    }
    return newRow;
}

void backTrack(char* s, int i, int len, int parts, int row, char*** ans,
               int* ansSize, int* cols, i64* abort_stub) {
    int j, n;
    char** nxt;
    if (i < 0 || len < 0 || parts < 0 || row < 0 || ans < 0 || ansSize < 0 || cols < 0) {
         RAISE_F((i64)abort_stub, (i64)0, (i64)i);
    }
    if (i >= len) {
        cols[row] = parts;
        return;
    }
    ans[row][parts] = slice(s, i, i + 1);
    backTrack(s, i + 1, len, parts + 1, row, ans, ansSize, cols, abort_stub);
    for (j = i + 2; j <= len; ++j) {
        if (!isPalindrome(s, i, j))
            continue;
        nxt = rowDup(ans[row], parts);
        n = *ansSize;
        *ansSize += 1;
        nxt[parts] = slice(s, i, j);
        ans[n] = nxt;
        backTrack(s, j, len, parts + 1, n, ans, ansSize, cols, abort_stub);
    }
}

char*** partition(char* s, int* returnSize, int** returnColumnSizes, i64* abort_stub) {
    char*** ans = (char***)malloc(PSIZE2 * sizeof(char**));
    int* colSizes = (int*)malloc(PSIZE2 * sizeof(int));
    int ansLen = 1, n = strlen(s);
    ans[0] = (char**)malloc(PSIZE * sizeof(char*));
    backTrack(s, 0, n, 0, 0, ans, &ansLen, colSizes, abort_stub);
    *returnSize = ansLen;
    *returnColumnSizes = colSizes;
    return ans;
}

int main(int argc, char *argv[]) {
    char* s = argv[1];
    return HANDLE(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)s)
    );
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env, i64 *abort_stub) {
    int returnSize;
    int* returnColumnSizes;
    char* s = (char*)env[0];
    char*** res = partition(s, &returnSize, &returnColumnSizes, abort_stub);
    for (int i = 0; i < returnSize; i++) {
        for (int j = 0; j < returnColumnSizes[i]; j++) {
            printf("%s ", res[i][j]);
        }
        printf("\n");
    }
    return 0;
}
