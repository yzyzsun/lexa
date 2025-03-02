#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 0;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    if (__builtin_expect(ret_addr == 0x3678, true)) { \
        header_offset = 16; \
    } else { \
        header_offset = 48; \
    }

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) \
    if (__builtin_expect(ret_addr == 0x37f5, false)) { \
        stub_pointer_offset = 48; \
    } else { \
        stub_pointer_offset = 16; \
    }

#define GET_FRAME_INFO(ret_addr) \
    if (__builtin_expect(ret_addr <= 0x3678, true)) { \
        frame_size = 176; \
        is_handler = true; \
        is_in_handler = false; \
    } else if (__builtin_expect(ret_addr <= 0x37d9, false)) { \
        frame_size = 240; \
        is_handler = false; \
        is_in_handler = false; \
    } else if (ret_addr <= 0x38ef) { \
        frame_size = 240; \
        is_handler = true; \
        is_in_handler = false; \
    } else { \
        frame_size = 80; \
        is_handler = false; \
        is_in_handler = true; \
    }

// #define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
//     switch (ret_addr) { \
//         case 0x3678: \
//             header_offset = 16; \
//             break; \
//         case 0x38ef: \
//             header_offset = 48; \
//             break; \
//         default: \
//             error_int("GET_HEADER_OFFSET_IN_HANDLER: invalid ret_addr: ", ret_addr); \
//             break; \
//     }

// #define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) \
//     switch (ret_addr) { \
//         case 0x37f5: \
//             stub_pointer_offset = 48; \
//             break; \
//         case 0x3973: \
//             stub_pointer_offset = 16; \
//             break; \
//         default: \
//             error_int("GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER: invalid ret_addr: ", ret_addr); \
//             break; \
//     }

// #define GET_FRAME_INFO(ret_addr) \
//     switch (ret_addr) { \
//         case 0x37d9: \
//             frame_size = 240; \
//             is_handler = false; \
//             is_in_handler = false; \
//             break; \
//         case 0x3678: \
//             frame_size = 176; \
//             is_handler = true; \
//             is_in_handler = false; \
//             break; \
//         case 0x38ef: \
//             frame_size = 240; \
//             is_handler = true; \
//             is_in_handler = false; \
//             break; \
//         case 0x3957: \
//             frame_size = 80; \
//             is_handler = false; \
//             is_in_handler = true; \
//             break; \
//         default: \
//             error_int("GET_FRAME_INFO: invalid ret_addr: ", ret_addr); \
//             break; \
//     }

