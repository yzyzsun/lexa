#include <stdint.h>

static const intptr_t STACKWALKER_IMMEDIATE_OFFSET = 48;

#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
    switch (ret_addr) { \
        case 0x3ab0: \
            header_offset = 8; \
            break; \
        case 0x3e51: \
            header_offset = 24; \
            break; \
        case 0x3870: \
            header_offset = 16; \
            break; \
    }

#define GET_STUB_POINTER_OFFSET_IN_HANDLER_INVOKER(ret_addr) \
            stub_pointer_offset = 8;

#define GET_FRAME_INFO(ret_addr) \
    switch (ret_addr) { \
        case 0x38da: \
            frame_size = 96; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3ab0: \
            frame_size = 176; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        case 0x3944: \
            frame_size = 96; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3f1a: \
            frame_size = 16; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
        case 0x3e51: \
            frame_size = 224; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        case 0x3870: \
            frame_size = 176; \
            is_handler = true; \
            is_in_handler = false; \
            break; \
        case 0x3cd7: \
            frame_size = 80; \
            is_handler = false; \
            is_in_handler = true; \
            break; \
        case 0x3989: \
            frame_size = 96; \
            is_handler = false; \
            is_in_handler = false; \
            break; \
    }

