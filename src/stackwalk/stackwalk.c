#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

// See the use of __start_SECTION_NAME: https://stackoverflow.com/a/48550485
long __attribute__((section("clue_table"), used)) dummyVar;
extern __attribute__((weak)) intptr_t __start_clue_table;
static const intptr_t CLUE_TABLE_WIDTH = 3;

static const intptr_t CODE_START = 0x555555554000;

#define DEBUG_STACKWALKER 0
#if DEBUG_STACKWALKER
static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 56;
#else
static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 8;
#endif


intptr_t* stackwalk(int clue_sig, int clue_dist) {
    intptr_t *CLUE_TABLE = &__start_clue_table;
    int table_size = CLUE_TABLE[0];
    CLUE_TABLE = CLUE_TABLE + 1;

    int count = 0;

    intptr_t stack_iter;
    __asm__("mov %%rsp, %0" : "=r"(stack_iter));
    stack_iter = stack_iter + STACKWALKER_IMMEDIATE_OFFSET;

    // At the beginning of each iteration,
    // stack_iter points to the return address on the stack
    while (1) {
        intptr_t ret_addr = *((intptr_t*)stack_iter);
        ret_addr = ret_addr - CODE_START;

#if DEBUG_STACKWALKER
        printf("=================\n");
        printf("stack_iter:0x%lx\n", stack_iter);
        printf("return address: 0x%lx\n", ret_addr);
        printf("Depth: %d\n", count);
        printf("clue_sig: %d\n", clue_sig);
        printf("clue_dist: %d\n", clue_dist);
#endif

        // Find the framesize of the current function
        // and check if the current function is a handler
        long frame_size = -1;
        bool is_handler = false;
        for (int i = 0; i < table_size; i++) {
            if (CLUE_TABLE[CLUE_TABLE_WIDTH*i] == ret_addr) {
                frame_size = CLUE_TABLE[CLUE_TABLE_WIDTH*i+1];
                is_handler = CLUE_TABLE[CLUE_TABLE_WIDTH*i+2];
                break;
            }
        }
        if (is_handler) {
            if (clue_dist == 0) {
                // +1 is because: https://git.uwaterloo.ca/z33ge/sstal/-/issues/77
                intptr_t *header_ptr = (intptr_t*)*((intptr_t*)stack_iter + 1);
                // -2 is because the pointer read off the stack is the location of the exchanger,
                // while this function promised to return the location of header
                return header_ptr-2;
            } else {
                clue_dist--;
            }
        }
        if (frame_size == -1) {
            printf("Frame size not found!\n\n");
            exit(1);
        }
        
        stack_iter = stack_iter + frame_size;

        count++;
    }
}