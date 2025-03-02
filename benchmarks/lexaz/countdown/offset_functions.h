#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
            header_offset = 16; 

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) 


#define GET_FRAME_INFO(ret_addr) \
    if (__builtin_expect(ret_addr <= 0x3687, false)) { \
        frame_size = 176; \
        is_handler = true; \
        is_in_handler = false; \
    } else { \
        frame_size = 80; \
        is_handler = false; \
        is_in_handler = false; \
    }

// #define GET_FRAME_INFO(ret_addr) \
//     switch (ret_addr) { \
//         case 0x37d8: \
//             frame_size = 80; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x3687: \
//             frame_size = 176; \
//             is_handler = true; \
//             is_in_handler = false; \
//             break; \
//         case 0x3839: \
//             frame_size = 80; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//     }

