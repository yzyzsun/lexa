#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    header_offset = 32;

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 

#define GET_FRAME_INFO(ret_addr) \
    switch (ret_addr) { \
        case 0x3c2f: \
            frame_size = 32; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3745: \
            frame_size = 224; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
    }

