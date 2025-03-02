#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    header_offset = 8;

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 

#define GET_FRAME_INFO(ret_addr) \
    if (ret_addr == 0x369b) { \
        frame_size = 176; \
        is_handler = true; \
        is_in_handler = false; \
    } else { \
        frame_size = 96; \
        is_handler = false; \
        is_in_handler = false; \
    }