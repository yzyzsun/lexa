#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
            header_offset = 24;

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 

#define GET_FRAME_INFO(ret_addr) \
    if (__builtin_expect(ret_addr <= 0x34fe, true)) { \
        frame_size = 8; \
        is_handler = true; \
        is_in_handler = false; \
    } else if (__builtin_expect(ret_addr <= 0x3971, true)) { \
        frame_size = 80; \
        is_handler = false; \
        is_in_handler = false; \
    } else { \
        frame_size = 16; \
        is_handler = false; \
        is_in_handler = false; \
    }

// #define GET_FRAME_INFO(ret_addr) \
//     switch (ret_addr) { \
//         case 0x2971: \
//             frame_size = 80; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x2968: \
//             frame_size = 80; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x2a62: \
//             frame_size = 16; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x2a7a: \
//             frame_size = 16; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x24fe: \
//             frame_size = 8; \
//             is_handler = true; \
//             is_in_handler = false; \
//             break; \
//     }

