#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include "../stacktrek/common.h"

static const intptr_t CODE_START = 0x555555554000;

long __attribute__((section("clue_table"), used)) CLUE_TABLE[4096];
extern __attribute__((weak)) intptr_t __start_clue_table;
static const intptr_t CLUE_TABLE_WIDTH = 41; // 7 parts + 34 for hopper

// Programs are compiled twice. In the first time, 
// NO_OFFSET is set, and the resulting binary is used for analysis
// by annotate_binary.py. In the second time, NO_OFFSET is not set,
// and the resulting binary is used for execution.
#if __has_include("offset_functions.h") && !NO_OFFSET
#include "offset_functions.h"
#else
    #define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) 0
#endif

struct clue_frame {
    uint8_t sig;
    uint8_t type;
    uint8_t index;
    struct clue_frame *next;
};

int error_int(const char* msg, long val) {
    fprintf(stderr, "Error: %s 0x%lx\n", msg, val);
    exit(1);
}

#define UPDATE_CLUE \
    if(!is_const) { \
        int index = clue_sig * (1 + cap_num + label_num); \
        if(clue_type == 0) { \
            index += 1 + cap_num + clue_index; \
        } \
        else if(clue_type == 1)  { \
            index += 1 + clue_index; \
        } \
        uint8_t updated_clue = hopper_clue[index]; \
        \
        clue_type = updated_clue >> 4; \
        clue_index = (updated_clue & 0xF); \
    } \

// We assume that zero cost effect is going to be raised rarely, so
// we don't inline it
__attribute__((noinline))
// HACK: This is to prevent stackwalk being placed before
// other functions. If that happens, as we change the offset_functions.h,
// the address of other functions which is described by offset_functions.h will change too,
// causing an egg-chicken problem.
__attribute__((section (".text.stackwalk"))) 
header_t* stackwalk(int clue_sig, int clue_type, int clue_index) {
    int table_size = __start_clue_table;

    // the stack of clues from walking through call-resumptive call/install sites
    struct clue_frame *clue_stack = NULL;

#if DEBUG_STACKWALKER
        printf("=================\n");
        printf("initial clue: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
#endif

    intptr_t stack_iter = (intptr_t) __builtin_frame_address(0) + 8;
    bool to_jump = false;

    while (1) {
        intptr_t ret_addr = *((intptr_t*)stack_iter);
        ret_addr = ret_addr - CODE_START;

        #if DEBUG_STACKWALKER
        printf("-----------------\n");
        printf("current clue: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
        #endif

        long frame_size = 0;
        long ip_offset = 0;
        bool is_handler = false;
        bool is_in_tail_handler = false;
        bool is_const = false;
        int cap_num = 0;
        int label_num = 0;
        uint8_t *hopper_clue = NULL;

        for (int i = 0; i < table_size; i++) {
            intptr_t *entry = &CLUE_TABLE[CLUE_TABLE_WIDTH * i + 1];
            if (entry[0] == ret_addr) {
                frame_size = entry[1];
                is_handler = entry[2];
                is_in_tail_handler = entry[3];
                is_const = entry[4];
                cap_num = entry[5];
                label_num = entry[6];
                hopper_clue = (uint8_t *) (entry + 7);
                break;
            }
        }

        #if DEBUG_STACKWALKER
        printf("ret_addr: %x, frame_size: %d, is_handler: %d, is_in_tail_handler: %d, cap_num: %d, label_num: %d, hopper_clue: %x\n", ret_addr, frame_size, is_handler, is_in_tail_handler, cap_num, label_num, hopper_clue);
        int index = clue_sig * (1 + cap_num + label_num); 
        printf("\nindex: %d", index);
        if(clue_type == 0) {
            index += 1 + cap_num + clue_index;
        } \
        else if(clue_type == 1)  {
            index += 1 + clue_index;
        }
        printf("\nindex: %d, hopper[index]: %x\n", index, hopper_clue[index]);
        #endif

        // If we're inside a tail-resumptive handler, we need to jump over the clue 
        // that was pushed onto the stack.
        if (to_jump) {
            stack_iter += 48;
            to_jump = false;
        }

        if (is_handler) {

            int header_offset = 0;
            GET_HEADER_OFFSET_IN_HANDLER(ret_addr);

            intptr_t *header_ptr = (intptr_t *)(stack_iter + header_offset);

            if (clue_type == 0 && clue_index == 0) {
                // no more clues in the stack: return the header in the current frame
                if (clue_stack == NULL) {
                    #if DEBUG_STACKWALKER
                    printf("found handler\n");
                    #endif
                    return (header_t *) header_ptr;
                }

                // pop the clue from stack
                clue_sig = clue_stack->sig;
                clue_type = clue_stack->type;
                clue_index = clue_stack->index;
                #if DEBUG_STACKWALKER
                printf("jumping over tail-resumptive install site\n");
                printf("old clue: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
                #endif

                clue_stack = clue_stack->next;
            } 
            // update the clue based on hopper
            UPDATE_CLUE
            #if DEBUG_STACKWALKER 
                printf("updated clue: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
            #endif

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
            stack_iter = stack_iter + frame_size;
            if (is_in_tail_handler) {
                #if DEBUG_STACKWALKER
                printf("jump over tail-resumptive call site\n");
                #endif

                // push the old clue into the clue stack
                struct clue_frame *old_clue = (struct clue_frame*)((intptr_t*)stack_iter + 5);
                old_clue->sig = clue_sig;
                old_clue->type = clue_type;
                old_clue->index = clue_index;
                old_clue->next = clue_stack;
                clue_stack = old_clue;

                clue_sig = (intptr_t) *((intptr_t*)stack_iter + 3);
                clue_type = (intptr_t) *((intptr_t*)stack_iter + 2);
                clue_index = (intptr_t) *((intptr_t*)stack_iter + 1);

                #if DEBUG_STACKWALKER
                printf("new clue from stack: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
                #endif

                to_jump = true;
            } else {
                UPDATE_CLUE
                #if DEBUG_STACKWALKER
                    printf("updated clue: sig %d, type %d, index %d\n", clue_sig, clue_type, clue_index);
                #endif
            }
        }
    }
}