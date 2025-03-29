#include <stdio.h>
#include <stacktrek.h>

FAST_SWITCH_DECORATOR
i64 division_by_zero_handler(i64 *env) {
    printf("Error: Division by zero\n");
    return 1;
}

__attribute((noinline))
uint64_t mod(uint64_t a, uint64_t b) {
    if (b == 0)
        RAISEZ(0, 0, 0, ());
    return a % b;
}

__attribute((noinline))
uint64_t divide(uint64_t a, uint64_t b) {
    if (b == 0)
        RAISEZ(0, 0, 0, ());
    return a / b;
}

// Function to compute gcd and Bézout's coefficients
void extendedEuclid(uint64_t a, uint64_t b, uint64_t *x, uint64_t *y, uint64_t *gcd) {
    if (b == 0) {
        *x = 1;
        *y = 0;
        *gcd = a;
        return;
    }

    uint64_t x1, y1; // To store results of recursive call
    extendedEuclid(b, mod(a, b), &x1, &y1, gcd);
    
    *x = y1;
    *y = x1 - divide(a, b) * y1;
}

FAST_SWITCH_DECORATOR
i64 extendedEuclid_handle_body(i64 *env) {
    uint64_t a = (uint64_t)env[0];
    uint64_t b = (uint64_t)env[1];
    uint64_t *x = (uint64_t*)env[2];
    uint64_t *y = (uint64_t*)env[3];
    uint64_t *gcd = (uint64_t*)env[4];

    extendedEuclid(a, b, x, y, gcd);
    return 0;
}

int main() {
    uint64_t a = 4660046610375530309, b = 7540113804746346429;
    uint64_t x, y, gcd;

    uint64_t acc = 0;

    for (int i = 0; i < 1000000; i++) {
        HANDLEZ(
            extendedEuclid_handle_body,
            ({ABORT, division_by_zero_handler}),
            ((i64)a, (i64)b, (i64)&x, (i64)&y, (i64)&gcd)
        );
        acc += x + y + gcd;
        b += 1;
    }
    printf("%lu\n", acc);
    return 0;
}
