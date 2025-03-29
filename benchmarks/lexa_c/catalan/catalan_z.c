#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>

i64 invalid_arg_handler(i64*, i64);
FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*);

i64 invalid_arg_handler(i64 *env, i64 n) {
    printf("Error: invalid argument %d\n", n);
    return 1;
}

unsigned long catalan(int n) {
    if(n < 0) {
        RAISEZ(0, 0, 0, ((i64)n));
    }
    else if (n == 0) return 1;

    unsigned long result = 0;
    for(int i = 0; i < n; ++i) {
        result += catalan(i) * catalan(n - 1 - i);
    }

    return result;
}

int main(int argc, char *argv[]) {
    int n = atoi(argv[1]);

    return HANDLEZ(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)n)
    );
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env) {
    int n = (int)env[0];

    printf("%lu\n", catalan(n));
    return 0;
}