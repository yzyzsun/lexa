// THIS IS A TEMPLATE FOR OFFSET FUNCTIONS

#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 8;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    switch (ret_addr) { \
        case 0x34fe: \
            header_offset = 24; \
            break; \
        default: \
            error_int("GET_HEADER_OFFSET_IN_HANDLER: invalid ret_addr: ", ret_addr); \
            break; \
    }

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) \
    switch (ret_addr) { \
        case 0x37f5: \
            stub_pointer_offset = 48; \
            break; \
        default: \
            error_int("GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER: invalid ret_addr: ", ret_addr); \
            break; \
    }

#define GET_FRAME_INFO(ret_addr) \
    switch (ret_addr) { \
        case 0x3838: \
            frame_size = 96; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3821: \
            frame_size = 96; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3aa9: \
            frame_size = 16; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x34fe: \
            frame_size = 8; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        default: \
            error_int("GET_FRAME_INFO: invalid ret_addr: ", ret_addr); \
            break; \
    }

