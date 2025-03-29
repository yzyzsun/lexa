#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stacktrek.h>

i64 invalid_arg_handler(i64*, i64);
FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*, i64*);

i64 invalid_arg_handler(i64 *env, i64 n) {
    printf("Aborted\n");
    return 1;
}

void iter(int i, char* current, char* next, int n, i64 *abort_stub) {
    if (i >= n) {
        return;
    }
    if (i < 0) {
        RAISE(abort_stub, 0, ((i64)n));
    }
    int len = strlen(current);
    int index = 0;

    for (int j = 0; j < len; j++) {
        int count = 1;
        while (j + 1 < len && current[j] == current[j + 1]) {
            count++;
            j++;
        }
        index += sprintf(next + index, "%d%c", count, current[j]);
    }
    printf("%s\n", next);
    strcpy(current, next);
    iter(i + 1, current, next, n, abort_stub);
}


FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env, i64 *abort_stub) {
    char* current = (char*)env[0];
    char* next = (char*)env[1];
    int n = (int)env[2];
    iter(1, current, next, n, abort_stub);
    return 0;
}

void lookAndSay(int n) {
    
    if (n <= 0) return;
    
    char *current = malloc(9999999999 * sizeof(char));
    char *next = malloc(9999999999 * sizeof(char));
    
    strcpy(current, "1");
    printf("%s\n", current);
    
    HANDLE(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)current, (i64)next, (i64)n)
    );

    // iter(1, current, next, n, (i64*)0);

    free(current);
    free(next);
}



int main() {
    int n = 50; // Number of terms
    lookAndSay(n);
    return 0;
}