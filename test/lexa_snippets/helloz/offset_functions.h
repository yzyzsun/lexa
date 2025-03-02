// THIS IS A TEMPLATE FOR OFFSET FUNCTIONS

#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 8;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    switch (ret_addr) { \
        case 0x3628: \
            header_offset = 16; \
            break; \
        default: \
            error_int("GET_HEADER_OFFSET_IN_HANDLER: invalid ret_addr: ", ret_addr); \
            break; \
    }

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) \
    switch (ret_addr) { \
        default: \
            error_int("GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER: invalid ret_addr: ", ret_addr); \
            break; \
    }

#define GET_FRAME_INFO(ret_addr) \
    switch (ret_addr) { \
        case 0x37a7: \
            frame_size = 80; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3628: \
            frame_size = 160; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        default: \
            error_int("GET_FRAME_INFO: invalid ret_addr: ", ret_addr); \
            break; \
    }

