#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    header_offset = 24;

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 

#define GET_FRAME_INFO(ret_addr) \
    if (ret_addr <= 0x34fe) { \
        frame_size = 8; \
        is_handler = true; \
        is_in_handler = false; \
    } else if (ret_addr <= 0x386b) { \
        frame_size = 80; \
        is_handler = false; \
        is_in_handler = false; \
    } else if (ret_addr <= 0x395d) { \
        frame_size = 96; \
        is_handler = false; \
        is_in_handler = false; \
    } else { \
        frame_size = 16; \
        is_handler = false; \
        is_in_handler = false; \
    }

    // switch (ret_addr) { \
    //     case 0x24fe: \
    //         frame_size = 8; \
    //         is_handler = true; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x27f6: \
    //         frame_size = 80; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x2802: \
    //         frame_size = 80; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x280e: \
    //         frame_size = 80; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x286b: \
    //         frame_size = 80; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x28e9: \
    //         frame_size = 96; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x295d: \
    //         frame_size = 96; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    //     case 0x2b1d: \
    //         frame_size = 16; \
    //         is_handler = false; \
    //         is_in_handler = false; \
    //         break; \
    // }

