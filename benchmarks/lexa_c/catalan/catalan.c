#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>

i64 invalid_arg_handler(i64*, i64);
FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*, i64*);

i64 invalid_arg_handler(i64 *env, i64 n) {
    printf("Error: invalid argument %d\n", n);
    return 1;
}

unsigned long catalan(int n, i64 *abort_stub) {
    if(n < 0) RAISE(abort_stub, 0, ((i64)n));
    else if (n == 0) return 1;

    unsigned long result = 0;
    for(int i = 0; i < n; ++i) {
        result += catalan(i, abort_stub) * catalan(n - 1 - i, abort_stub);
    }

    return result;
}

int main(int argc, char *argv[]) {
    int n = atoi(argv[1]);

    return HANDLE(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)n)
    );
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env, i64 *abort_stub) {
    int n = (int)env[0];

    printf("%lu\n", catalan(n, abort_stub));
    return 0;
}