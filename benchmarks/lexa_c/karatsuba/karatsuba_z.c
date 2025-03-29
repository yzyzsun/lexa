#include <math.h>
#include <stacktrek.h>

FAST_SWITCH_DECORATOR
i64 division_by_zero_handler(i64 *env) {
    printf("Error: Division by zero\n");
    return 1;
}

int max(int a, int b)
{
    return a > b ? a : b;
}

unsigned long divide(unsigned long a, unsigned long b) {
    if (b == 0) {
        RAISEZ(0, 0, 0, ());
    }
    return a / b;
}

unsigned long mod(unsigned long a, unsigned long b) {
    if (b == 0) {
        RAISEZ(0, 0, 0, ());
    }
    return a % b;
}

unsigned long numdigits(unsigned long a) {
    if (a <= 1) {
        return 1;
    }
    return 1 + numdigits(a / 2);
}

unsigned long pow2(unsigned long n) {
    if (n == 0) {
        return 1;
    }
    return 2 * pow2(n - 1);
}

unsigned long karatsuba(unsigned long num1, unsigned long num2, int threshold) {
    if(num1 < threshold || num2 < threshold) return num1 * num2;

    int m = max(numdigits(num1), numdigits(num2));
    int m2 = divide(m, 2);
    int powm2 = pow2(m2);

    unsigned long high1 = divide(num1, powm2);
    unsigned long low1 = mod(num1, powm2);
    unsigned long high2 = divide(num2, powm2);
    unsigned long low2 = mod(num2, powm2);

    // printf("%lu, %lu, %lu, %lu\n", high1, low1, high2, low2);

    unsigned long z0 = karatsuba(low1, low2, threshold);
    unsigned long z1 = karatsuba(low1 + high1, low2 + high2, threshold);
    unsigned long z2 = karatsuba(high1, high2, threshold);

    return (z2 * powm2 * powm2 + ((z1 - z2 - z0) * powm2)) + z0;
}

FAST_SWITCH_DECORATOR
i64 main_handle_body(i64 *env) {
    int n = (int)env[0];
    unsigned long acc = 0;
    for (int i = 0; i < 1000000; i++) {
        acc += karatsuba(n, n, 2);
    }
    printf("%lu\n", acc);
    return 0;
}

int main(int argc, char *argv[]) {
    int n = readInt();
    return HANDLEZ(
        main_handle_body,
        ({ABORT, division_by_zero_handler}),
        ((i64)n)
    );
}
