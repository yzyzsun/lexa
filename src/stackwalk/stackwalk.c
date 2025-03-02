#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include "../stacktrek/common.h"

static const intptr_t CODE_START = 0x555555554000;


// Programs are compiled twice. In the first time, 
// NO_OFFSET is set, and the resulting binary is used for analysis
// by annotate_binary.py. In the second time, NO_OFFSET is not set,
// and the resulting binary is used for execution.
#if __has_include("offset_functions.h") && !NO_OFFSET
#include "offset_functions.h"
#else
    #define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) 0
    #define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 0
    #define GET_FRAME_INFO(ret_addr)
    static const intptr_t STACKWALKER_IMMEDIATE_OFFSET;
#endif


int error_int(const char* msg, long val) {
    fprintf(stderr, "Error: %s 0x%lx\n", msg, val);
    exit(1);
}

// We assume that zero cost effect is going to be raised rarely, so
// we don't inline it
__attribute__((noinline))
// HACK: This is to prevent stackwalk being placed before
// other functions. If that happens, as we change the offset_functions.h,
// the address of other functions which is described by offset_functions.h will change too,
// causing an egg-chicken problem.
__attribute__((section (".text.stackwalk"))) 
header_t* stackwalk(int clue_sig, int clue_dist) {
#if DEBUG_STACKWALKER
        printf("=================\n");
#endif

    intptr_t stack_iter;
    __asm__("mov %%rsp, %0" : "=r"(stack_iter));
    stack_iter = stack_iter + STACKWALKER_IMMEDIATE_OFFSET;

    bool to_jump = false;
    // At the beginning of each iteration,
    // stack_iter points to the return address on the stack
    while (1) {
        intptr_t ret_addr = *((intptr_t*)stack_iter);
        ret_addr = ret_addr - CODE_START;

        if (to_jump) {
            int stub_pointer_offset = 0;
            GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr);
            header_t *header = (header_t*)*((intptr_t*)(stack_iter + stub_pointer_offset));
            intptr_t *exc = header->exchanger;
            stack_iter = (intptr_t)exc;
            
            to_jump = false;

            clue_dist += 1; // skip over the immediate handler, which we just jumped at, and is not in scope
            continue;
        }

        long frame_size = 0;
        long ip_offset = 0;
        bool is_handler = false;
        bool is_in_handler = false;
        GET_FRAME_INFO(ret_addr);
        // printf("ret_addr: %x, frame_size: %d, is_handler: %d, is_in_handler: %d\n", ret_addr, frame_size, is_handler, is_in_handler);

        if (is_handler) {

            int header_offset = 0;
            GET_HEADER_OFFSET_IN_HANDLER(ret_addr);

            intptr_t *header_ptr = (intptr_t *)(stack_iter + header_offset);

            if (clue_dist == 0) {
                #if DEBUG_STACKWALKER
                printf("found handler\n");
                #endif
                return header_ptr;
            } else {
                clue_dist--;
            }

            // We use the value of the exchanger to determine the type of handler
            intptr_t mode = *header_ptr;
            if (mode == TAIL || mode == ABORT) {
                #if DEBUG_STACKWALKER
                printf("walk over in-stack handler\n");
                #endif
                stack_iter = stack_iter + frame_size;
            } else {
                #if DEBUG_STACKWALKER
                printf("walk over general handler\n");
                #endif
                stack_iter = (intptr_t)((header_t*)header_ptr)->exchanger;
            }
        } else {
            if (is_in_handler) {
                #if DEBUG_STACKWALKER
                printf("jump over\n");
                #endif
                to_jump = true;
            } else {
                #if DEBUG_STACKWALKER
                printf("skipping function\n");
                #endif
            }
            stack_iter = stack_iter + frame_size;
        }
    }
}