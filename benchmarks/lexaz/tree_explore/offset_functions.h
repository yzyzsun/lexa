#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    header_offset = 24;

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 

#define GET_FRAME_INFO(ret_addr) \
    switch (ret_addr) { \
        case 0x39ed: \
            frame_size = 112; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3e1e: \
            frame_size = 16; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x34fe: \
            frame_size = 8; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        case 0x3ab6: \
            frame_size = 112; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
    }

