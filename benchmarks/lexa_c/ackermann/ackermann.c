#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>

i64 invalid_arg_handler(i64*, i64, i64);
FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*, i64*);

i64 invalid_arg_handler(i64 *env, i64 m, i64 n) {
    printf("Error: invalid argument (%d, %d)\n", m, n);
    return 1;
}

int ackermann(int m, int n, i64 *abort_stub) {
    if(m < 0 || n < 0) {
        RAISE(abort_stub, 0, ((i64)m, (i64)n));
    }

    if(m == 0) {
        return n + 1;
    }
    else if(n == 0) {
        return ackermann(m - 1, 1, abort_stub);
    }
    return ackermann(m - 1, ackermann(m, n - 1, abort_stub), abort_stub);
}

int main(int argc, char *argv[]) {
    int m = atoi(argv[1]);
    int n = atoi(argv[2]);

    return HANDLE(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)m, (i64)n)
    );
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env, i64 *abort_stub) {
    int m = (int)env[0];
    int n = (int)env[1];

    printf("%d\n", ackermann(m, n, abort_stub));
    return 0;
}