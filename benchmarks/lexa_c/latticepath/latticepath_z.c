#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stacktrek.h>

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64*);

i64 invalid_arg_handler(i64 *env) {
    printf("Error: invalid argument\n");
    return 1;
}

unsigned long latticepaths(int x1, int y1, int x2, int y2, int badx, int bady) {
    if(x2 < x1 || y2 < y1) {
        return RAISEZ(0, 0, 0, ());
    }
    else if(x1 == x2 || y1 == y2) return 1;
    else if(x1 == badx && y1 == bady) return 0;

    return latticepaths(x1 + 1, y1, x2, y2, badx, bady) + latticepaths(x1, y1 + 1, x2, y2, badx, bady);
}

int main(int argc, char *argv[]) {
    int n = readInt();
    return HANDLEZ(
        main_handle_body,
        ({ABORT, invalid_arg_handler}),
        ((i64)n)
    );
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env) {
    int n = (int)env[0];
    printf("%lu\n", latticepaths(0, 0, n, n, n/2, n/2));
    return 0;
}