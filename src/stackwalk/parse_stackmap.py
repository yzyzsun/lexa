from elftools.dwarf.descriptions import _REG_NAMES_x64
import struct

def parse_stackmaps(elf):
    stackmaps_section = elf.get_section_by_name('.llvm_stackmaps')
    data = stackmaps_section.data()

    # Parse the header
    header_fmt = 'BBH'
    header_size = struct.calcsize(header_fmt)
    stack_map_version, reserved1, reserved2 = struct.unpack(header_fmt, data[:header_size])
    print(f"Stack Map Version: {stack_map_version}")
    print(f"Reserved1: {reserved1}")
    print(f"Reserved2: {reserved2}")

    # Set the initial offset past the header
    offset = header_size
    
    # Parse NumFunctions, NumConstants, NumRecords
    num_fmt = 'III'
    num_size = struct.calcsize(num_fmt)
    num_functions, num_constants, num_records = struct.unpack_from(num_fmt, data, offset)
    offset += num_size
    
    print(f"NumFunctions: {num_functions}, NumConstants: {num_constants}, NumRecords: {num_records}")

    # Parse StkSizeRecord
    stk_size_fmt = 'QQQ'
    stk_size_len = struct.calcsize(stk_size_fmt)
    print("Function Stack Sizes:")
    for _ in range(num_functions):
        address, stack_size, record_count = struct.unpack_from(stk_size_fmt, data, offset)
        offset += stk_size_len
        print(f"  Address: {address:#x}, Stack Size: {stack_size}, Record Count: {record_count}")

    # Parse Constants
    const_fmt = 'Q'
    const_size = struct.calcsize(const_fmt)
    print("Constants:")
    for _ in range(num_constants):
        large_constant = struct.unpack_from(const_fmt, data, offset)
        offset += const_size
        print(f"  Large Constant: {large_constant[0]}")

    # Parse StkMapRecords
    print("Stack Map Records:")
    for _ in range(num_records):
        rec_header_fmt = 'QIH'
        rec_header_size = struct.calcsize(rec_header_fmt)
        patch_point_id, instruction_offset, reserved = struct.unpack_from(rec_header_fmt, data, offset)
        offset += rec_header_size
        
        num_locations, = struct.unpack_from('H', data, offset)
        offset += 2

        print(f"  Patch Point ID: {patch_point_id}, Instruction Offset: {instruction_offset}, NumLocations: {num_locations}")

        # Loop through each location
        loc_fmt = 'BBHHHi'
        loc_size = struct.calcsize(loc_fmt)
        for _ in range(num_locations):
            type, reserved_loc, location_size, dwarf_regnum, reserved_dwarf, offset_smallconst = struct.unpack_from(loc_fmt, data, offset)
            assert(reserved_loc == 0)
            assert(reserved_dwarf == 0)
            offset += loc_size
            print(f"    Type: {type}, Location Size: {location_size}, Dwarf RegNum: {_REG_NAMES_x64[dwarf_regnum]}, Offset/SmallConstant: {offset_smallconst}")

        # Padding for alignment to 8 bytes
        alignment = offset % 8
        if alignment != 0:
            offset += 8 - alignment

        num_live_outs, = struct.unpack_from('H', data, offset)
        offset += 2
        print(f"  NumLiveOuts: {num_live_outs}")

        live_out_fmt = 'HBB'
        live_out_size = struct.calcsize(live_out_fmt)
        for _ in range(num_live_outs):
            dwarf_regnum, reserved_live, size_in_bytes = struct.unpack_from(live_out_fmt, data, offset)
            offset += live_out_size
            print(f"    LiveOut - Dwarf RegNum: {dwarf_regnum}, Size in Bytes: {size_in_bytes}")

        # Additional padding for alignment to 8 bytes, if required
        alignment = offset % 8
        if alignment != 0:
            offset += 8 - alignment

parse_stackmaps(data)