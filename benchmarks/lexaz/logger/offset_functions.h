#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \
switch(ret_addr) { \
	case 0x34fe: header_offset = 24; break; \
	case 0x3689: header_offset = 16; break; \
	case 0x3752: header_offset = 16; break; \
	default: error_int("GET_HEADER_OFFSET_IN_HANDLER: invalid ret_addr: ", ret_addr); break; \
}