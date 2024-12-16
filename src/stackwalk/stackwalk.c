#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

// See the use of __start_SECTION_NAME: https://stackoverflow.com/a/48550485
// NB: we assume that clue_table has max 4096 bytes
__attribute__((section("clue_table"), used)) char dummyArray[4096];
extern __attribute__((weak)) intptr_t __start_clue_table;
static const intptr_t CLUE_TABLE_WIDTH = 4;

static const intptr_t CODE_START = 0x555555554000;

#define DEBUG_STACKWALKER 0
#if DEBUG_STACKWALKER
static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 56;

// Turn this on when -O1 is used
// static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 88;
#else
static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 8;
#endif


intptr_t* stackwalk(int clue_sig, int clue_dist) {
    intptr_t *CLUE_TABLE = &__start_clue_table;
    int table_size = CLUE_TABLE[0];
    CLUE_TABLE = CLUE_TABLE + 1;

    intptr_t stack_iter;
    __asm__("mov %%rsp, %0" : "=r"(stack_iter));
    stack_iter = stack_iter + STACKWALKER_IMMEDIATE_OFFSET;


    bool is_in_handler = false;
    // At the beginning of each iteration,
    // stack_iter points to the return address on the stack
    while (1) {
        intptr_t ret_addr = *((intptr_t*)stack_iter);
        ret_addr = ret_addr - CODE_START;

#if DEBUG_STACKWALKER
        printf("=================\n");
        printf("stack_iter:0x%lx\n", stack_iter);
        printf("return address: 0x%lx\n", ret_addr);
        printf("clue_sig: %d\n", clue_sig);
        printf("clue_dist: %d\n", clue_dist);
#endif

        // Find the framesize of the current function
        // and check if the current function is a handler
        long frame_size = 0;
        bool is_handler = false;
        bool found = false;
        for (int i = 0; i < table_size; i++) {
            if (CLUE_TABLE[CLUE_TABLE_WIDTH*i] == ret_addr) {
                frame_size = CLUE_TABLE[CLUE_TABLE_WIDTH*i+1];
                is_handler = CLUE_TABLE[CLUE_TABLE_WIDTH*i+2];
                is_in_handler = CLUE_TABLE[CLUE_TABLE_WIDTH*i+3];
                found = true;
                break;
            }
        }
        if (!found) {
            printf("No clue found for address 0x%lx\n", ret_addr);
            exit(1);
        }

        if (is_handler) {
            // +1 is because: https://git.uwaterloo.ca/z33ge/sstal/-/issues/77
            intptr_t *exc_ptr = (intptr_t*)*((intptr_t*)stack_iter + 1);

            if (clue_dist == 0) {
                // -2 is because the pointer read off the stack is the location of the exchanger,
                // while this function promised to return the location of header
                #if DEBUG_STACKWALKER
                printf("found handler\n");
                #endif
                intptr_t *header_ptr = exc_ptr-2;
                return header_ptr;
            } else {
                clue_dist--;
            }

            // We use the value of the exchanger to determine the type of handler
            intptr_t ctx_sp = *exc_ptr;
            if ((intptr_t)stack_iter - (intptr_t)ctx_sp == 0) {
                #if DEBUG_STACKWALKER
                printf("walk over in-stack handler\n");
                #endif
                stack_iter = stack_iter + frame_size;
            } else {
                #if DEBUG_STACKWALKER
                printf("walk over general handler\n");
                #endif
                stack_iter = ctx_sp;
            }
        } else {
            if (is_in_handler) {
                // If an effect is coming out of a handler,
                // we need to walk all the way up to the installment of the handler
                // For abortive handlers and tail handlers, we find the saved exchanger
                // behind the return address on the stack.
                // For abortive handlers, it was pushed when the handler is installed.
                // For tail handlers, it was pushed when the handler was called
                // TODO: For general handlers it is not clear how to carry on
                #if DEBUG_STACKWALKER
                printf("jump over\n");
                #endif
                stack_iter = stack_iter + frame_size;
                intptr_t *exc_ptr = (intptr_t*)*((intptr_t*)stack_iter + 1);
                intptr_t ctx_sp = *exc_ptr;
                stack_iter = ctx_sp;

                clue_dist += 1;
            } else {
                #if DEBUG_STACKWALKER
                printf("skipping function\n");
                #endif
                stack_iter = stack_iter + frame_size;
            }

            // We now has skipped all handlers within a function and
            // is going to the caller. If we were in a handler, 
            // we are not anymore.
            // (actually, we may still be in a handler, but that will be determined in the next iteration)
            is_in_handler = false;
        }
    }
}