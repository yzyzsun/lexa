#!/usr/bin/env python3

import argparse
import os
import tempfile
from capstone import Cs, CS_ARCH_X86, CS_MODE_64
from elftools.elf.elffile import ELFFile
from elftools.dwarf.callframe import *
from elftools.dwarf.dwarf_expr import DWARFExprParser
import subprocess
import parse_dwarf

def add_section_to_elf(input_elf, output_elf, section_name, data):
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file:
        tmp_file.write(data)  # Write the data to the temporary file
        tmp_file_path = tmp_file.name

    try:
        # Step 2: Copy ELF and add the section with the contents of the temporary file
        objcopy_add_section_cmd = [
            "objcopy",
            f"--update-section", f"{section_name}={tmp_file_path}",
            input_elf, output_elf
        ]
        subprocess.run(objcopy_add_section_cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    finally:
        os.remove(tmp_file_path)  # Clean up temporary file

def get_function_map(elf):
    function_map = {}
    for section in elf.iter_sections():
        if section.header.sh_type in ['SHT_SYMTAB', 'SHT_DYNSYM']:
            for symbol in section.iter_symbols():
                if symbol.name and symbol['st_value'] != 0 and symbol['st_size'] > 0:
                    # Map the address range to the function name
                    address_range = (symbol['st_value'], symbol['st_value'] + symbol['st_size'])
                    function_map[address_range] = symbol.name
    return function_map
    

def find_function_and_start_addr_by_address(function_map, address):
    for address_range, name in function_map.items():
        if address_range[0] <= address < address_range[1]:
            return address_range[0], name
    return None, None

# Get the return address to tags mapping from the binary
def get_address_to_metadata_map(binary_path):
    output = subprocess.check_output(["nm", "-n", binary_path], text=True)

    addr_to_metadata = {}
    for line in output.strip().splitlines():
        parts = line.strip().split()
        if len(parts) >= 3:
            addr_str, _type, label = parts
            try:
                addr = int(addr_str, 16)
                if label.startswith("__callsite_metadata_"):
                    addr_to_metadata[addr] = label.split("___")[1]
            except ValueError:
                continue
    return addr_to_metadata

# Creates the header offset mapping based on the clue table and the debug information from the binary
def get_header_offset_in_handler(input, clue_table):
    with open(input, 'rb') as f:
        elf = ELFFile(f)
        dwarfinfo = elf.get_dwarf_info()
        expr_parser = DWARFExprParser(dwarfinfo.structs)

        locations = parse_dwarf.parse_variable_locations(elf)
        offset_map = {}

        for (addr, cfa_offset, ip_offset, function_name, handler_indicator, in_tail_handler, is_const, cap_num, label_num, hopper_md) in clue_table:
            if not handler_indicator:
                continue
            
            for var in locations[function_name]:
                if not 'stub' in var['variable']:
                    continue

                # There is a chance that multiple stubs are present in same stack frame due to inlining.
                # This can happen only under the following conditions:
                #   1. There are multiple tail-resumptive stock lexa stubs in the stack frame, and
                #   2. There is at most 1 tail-resumptive or abortive lexaz stubs in the frame.
                # If there is a lexaz stub, we must pick its offset in offset_functions.h in case it is raised.
                # Otherwise, any stub's offset in the stack frame will suffice, since walking through tail-resumptive
                # and abortive handlers both involve simply skipping the current frame.
                stub_found = False

                for loc in var['locations']:
                    if loc['start'] <= addr < loc['end']:
                        ops = expr_parser.parse_expr(loc['expr'])
                        for op in ops:
                        # Offset from the stack pointer
                            if op.op_name == 'DW_OP_breg7' or op.op_name == 'DW_OP_fbreg':
                                offset_map[addr] = op.args[0] + 8
                                stub_found = True
                                break
                        
                # If the variable found is stub_z, break
                if stub_found and var['variable'] == 'stub_z':
                    break

        return offset_map

def get_clue_table(input):
    with open(input, 'rb') as f:
        elf = ELFFile(f)
        exec_segments = [seg for seg in elf.iter_segments() if seg['p_type'] == 'PT_LOAD' and (seg['p_flags'] & 0x1)]
        assert(len(exec_segments) == 1)
        segment = exec_segments[0]
        function_map = get_function_map(elf)
        addr_to_metadata = get_address_to_metadata_map(input)

        # Create a Capstone disassembler
        md = Cs(CS_ARCH_X86, CS_MODE_64)

        clue_table = {}

        section = elf.get_section_by_name(".text")
        prev_inst_is_call = False
        handler_indicator = False
        sh_addr = elf.get_section_by_name(".text").header.sh_addr

        # Disassemble the binary and find call instructions
        for i in md.disasm(section.data(), 0):
            if prev_inst_is_call:
                start_addr, function_name = find_function_and_start_addr_by_address(function_map, i.address+sh_addr)
                if function_name is None or function_name == "_start":
                    continue
                clue_table[i.address+sh_addr] = {"function_name": function_name, "start_addr": start_addr, "handler_indicator": handler_indicator}

            prev_inst_is_call = False
            handler_indicator = False
            if i.mnemonic == 'call':
                if i.op_str[0].isdigit():
                    target_address = int(i.op_str, 16)
                    _, target_function_name = find_function_and_start_addr_by_address(function_map, target_address+sh_addr)
                    # print(f"{i.address+sh_addr:#x} {i.mnemonic} {i.op_str} {target_function_name}")
                    # HACK: we assume the body function's name contains "body"
                    # this is justifiable because the Lexac compiler generates
                    # body's function names
                    if "body" in str(target_function_name):
                        handler_indicator = True
                prev_inst_is_call = True

        cfi_entries = elf.get_dwarf_info().EH_CFI_entries()
        cfi_entries = [entry for entry in cfi_entries if type(entry) is not ZERO]
        # sort the cfi entries(each entry represents a function) by pc
        cfi_entries.sort(key=lambda x: x.get_decoded().table[0]["pc"])
        for addr, meta in clue_table.items():
            cfa = None
            done = False
            for cfi in cfi_entries:
                table = cfi.get_decoded().table
                table.sort(key=lambda x: x["pc"])
                for entry in table:
                    if addr < entry["pc"]:
                        done = True
                        break
                    else:
                        cfa = entry["cfa"].offset
                if done:
                    break
            assert(cfa is not None)
            meta["cfa_offset"] = cfa
            meta["ip_offset"] = addr - meta["start_addr"]

            meta["is_const"] = 0
            meta["cap_num"] = 0
            meta["label_num"] = 0
            meta["hopper_md"] = ""
        
        for addr, metadata in addr_to_metadata.items():
            # HACK: The label might not be inserted right after the function call when LLVM IR is translated to asm.
            #       Here we assume there are less than 4-byte offset between the call and the label.
            #       This may assign a metadata to a callsite that shouldn't have a metadata, but if it is never
            #       accessed by the stackwalker it should be fine.
            for i in range(1, 32):
                if addr + i in clue_table:
                    metadata_nums = metadata.split("_")
                    meta = clue_table[addr + i]
                    meta["is_const"] = int(metadata_nums[0])
                    meta["cap_num"] = int(metadata_nums[1])
                    meta["label_num"] = int(metadata_nums[2])
                    meta["hopper_md"] = metadata_nums[3]
                    break


        for addr, meta in clue_table.items():
            _, function_name = find_function_and_start_addr_by_address(function_map, addr)
            if function_name.startswith("__tail__handler"):
                meta["in_tail_handler"] = True
            else:
                meta["in_tail_handler"] = False

    # find the return address to ENTER and mark handler indicator
    for addr, meta in clue_table.items():
        if meta["function_name"] == "ENTER":
            meta["handler_indicator"] = True

    clue_table = [(addr, meta["cfa_offset"], meta["ip_offset"], meta["function_name"], meta["handler_indicator"], meta["in_tail_handler"], meta["is_const"], meta["cap_num"], meta["label_num"], meta["hopper_md"]) for addr, meta in clue_table.items()]
    
    return clue_table

# Remove calls that are pure. Most of them come from the old c stdlib
def clean_clue_table(clue_table):
    irrelevant_functions =  ["error", "check", "printInt", "printChar", "listNode", "listRange", "listPrint", "listAppend", "treeNode", "queueMake", "queueEnq", "stringMake", "stringSubStr", "printFloat", "floatPow", "floatExp", "floatRand", "floatCos", "floatSin", "floatSqrt", "floatLog", "arrayMake", "arrayMakeInit", "arrayPush", "arrayPrint", "arrayPrintChars", "strPrint", "strConcat", "strEq", "init_stack_pool", "get_stack", "dup_stack", "RAISE_M", "RAISE_M_2", "RAISE_M_3", "RAISE_M_0"]
    return [(addr, cfa_offset, ip_offset, function_name, handler_indicator, in_tail_handler, is_const, cap_num, label_num, hopper_md) 
            for addr, cfa_offset, ip_offset, function_name, handler_indicator, in_tail_handler, is_const, cap_num, label_num, hopper_md in clue_table 
            if function_name not in irrelevant_functions 
            and hopper_md
            # If all characters of metadata is F, stackwalker will never walk through this address
            and (hopper_md != len(hopper_md) * 'F' or handler_indicator)
            ]

def main():
    parser = argparse.ArgumentParser(description="Find CALL instructions and their function names in a binary file.")
    parser.add_argument("input", type=str, help="The path to the binary file to analyze.")
    parser.add_argument("--gen-offset", action="store_true", help="Generate offset_functions.h")
    parser.add_argument("--path", "--output", type=str, default="", help="The path to generate offset_function.h at")
    args = parser.parse_args()

    clue_table = get_clue_table(args.input)
    clue_table = clean_clue_table(clue_table)

    flat_clue_table = [len(clue_table)]

    for (addr, cfa_offset, ip_offset, function_name, handler_indicator, in_tail_handler, is_const, cap_num, label_num, hopper_md) in clue_table:
        flat_clue_table.append(addr)
        flat_clue_table.append(cfa_offset)
        flat_clue_table.append(handler_indicator)
        flat_clue_table.append(in_tail_handler)
        flat_clue_table.append(is_const)
        flat_clue_table.append(cap_num)
        flat_clue_table.append(label_num)

        parts = []

        for _ in range(34):
            # Reverse two digits at a time since the byte order will be reveresed later
            parts.append(0 if hopper_md == '' else int("".join(reversed([hopper_md[i:i+2] for i in range(0, 16, 2)])), 16))
            hopper_md = hopper_md[16:]
        flat_clue_table.extend(parts) 

    flat_clue_table = [x.to_bytes(8, byteorder='little') for x in flat_clue_table]
    flat_clue_table = [byte for b in flat_clue_table for byte in b]

    add_section_to_elf(args.input, args.input, "clue_table", bytes(flat_clue_table))

    with open("clue_table.txt", "w") as file:
        
        for addr, cfa_offset, ip_offset, function_name, handler_indicator, in_tail_handler, is_const, cap_num, label_num, hopper_md in clue_table:
            file.write(f"{addr:#x} {cfa_offset} {ip_offset} {function_name} {handler_indicator} {in_tail_handler} {is_const} {cap_num} {label_num} {hopper_md}\n")

    if args.gen_offset:
        # Create offset_functions.h
        with open(args.path + "/offset_functions.h", "w") as file:
            file.write("#define GET_HEADER_OFFSET_IN_HANDLER(ret_addr) \\\nswitch(ret_addr) { \\\n")

            header_offsets = get_header_offset_in_handler(args.input, clue_table)
            for addr in header_offsets:
                file.write(f"\tcase 0x{addr:x}: header_offset = {header_offsets[addr]}; break; \\\n")

            file.write('\tdefault: header_offset = 24; break; \\\n}')

if __name__ == "__main__":
    main()
