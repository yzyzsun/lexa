#!/usr/bin/env python3

import argparse
import os
import tempfile
from capstone import Cs, CS_ARCH_X86, CS_MODE_64
from elftools.elf.elffile import ELFFile
from elftools.dwarf.callframe import *
import subprocess

def add_section_to_elf(input_elf, output_elf, section_name, data):
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file:
        tmp_file.write(data)  # Write the data to the temporary file
        tmp_file_path = tmp_file.name

    try:
        # Step 2: Copy ELF and add the section with the contents of the temporary file
        objcopy_add_section_cmd = [
            "objcopy",
            f"--update-section", f"clue_table={tmp_file_path}",
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
    

def find_function_by_address(function_map, address):
    for address_range, name in function_map.items():
        if address_range[0] <= address < address_range[1]:
            return name
    return None

def get_clue_table(input):
    with open(input, 'rb') as f:
        elf = ELFFile(f)
        exec_segments = [seg for seg in elf.iter_segments() if seg['p_type'] == 'PT_LOAD' and (seg['p_flags'] & 0x1)]
        assert(len(exec_segments) == 1)
        segment = exec_segments[0]
        function_map = get_function_map(elf)

        # Create a Capstone disassembler
        md = Cs(CS_ARCH_X86, CS_MODE_64)

        clue_table = {}

        section = elf.get_section_by_name(".text")
        found = False
        handler_indicator = False
        sh_addr = elf.get_section_by_name(".text").header.sh_addr

        # Disassemble the binary and find call instructions
        for i in md.disasm(section.data(), 0):
            if found:
                function_name = find_function_by_address(function_map, i.address+sh_addr)
                if function_name is None or function_name == "_start":
                    continue
                clue_table[i.address+sh_addr] = {"function_name": function_name, "handler_indicator": handler_indicator}

            found = False
            handler_indicator = False
            if i.mnemonic == 'call':
                if i.op_str[0].isdigit():
                    target_address = int(i.op_str, 16)
                    target_function_name = find_function_by_address(function_map, target_address+sh_addr)
                    # print(f"{i.address+sh_addr:#x} {i.mnemonic} {i.op_str} {target_function_name}")
                    # HACK: we assume the body function's name contains "body"
                    # this is justifiable because the Lexac compiler generates
                    # body's function names
                    if "body" in str(target_function_name):
                        handler_indicator = True
                found = True

        cfi_entries = elf.get_dwarf_info().EH_CFI_entries()
        for addr, meta in clue_table.items():
            offset = None
            done = False
            for cfi in cfi_entries:
                if type(cfi) is ZERO:
                    continue
                table = cfi.get_decoded().table
                table.sort(key=lambda x: x["pc"])
                for entry in table:
                    if addr < entry["pc"]:
                        done = True
                        break
                    else:
                        offset = entry["cfa"].offset
                if done:
                    break
            assert(offset is not None)
            # HACK: https://git.uwaterloo.ca/z33ge/sstal/-/issues/77
            if meta["handler_indicator"]:
                offset += 16
            meta["rsp_offset"] = offset

    # find the return address to ENTER and mark handler indicator
    for addr, meta in clue_table.items():
        if meta["function_name"] == "ENTER":
            meta["handler_indicator"] = True
    
    return clue_table

# Remove calls that are pure. Most of them come from the old c stdlib
def clean_clue_table(clue_table):
    irrelevant_functions =  ["error", "check", "printInt", "printChar", "listNode", "listRange", "listPrint", "listAppend", "treeNode", "queueMake", "queueEnq", "stringMake", "stringSubStr", "printFloat", "floatPow", "floatExp", "floatRand", "floatCos", "floatSin", "floatSqrt", "floatLog", "arrayMake", "arrayMakeInit", "arrayPush", "arrayPrint", "arrayPrintChars", "strPrint", "strConcat", "strEq", "init_stack_pool", "get_stack", "dup_stack", "RAISE_M", "RAISE_M_2", "RAISE_M_3", "RAISE_M_0", "stackwalk"]
    return {addr: meta for addr, meta in clue_table.items() if meta["function_name"] not in irrelevant_functions}

def main():
    parser = argparse.ArgumentParser(description="Find CALL instructions and their function names in a binary file.")
    parser.add_argument("input", type=str, help="The path to the binary file to analyze.")
    args = parser.parse_args()
    
    clue_table = get_clue_table(args.input)
    clue_table = clean_clue_table(clue_table)
    flat_clue_table = [len(clue_table)]
    for addr, meta in clue_table.items():
        flat_clue_table.append(addr)
        flat_clue_table.append(meta["rsp_offset"])
        flat_clue_table.append(meta["handler_indicator"])

    flat_clue_table = [x.to_bytes(8, byteorder='little') for x in flat_clue_table]
    flat_clue_table = [byte for b in flat_clue_table for byte in b]
    add_section_to_elf(args.input, args.input, "clue_table", bytes(flat_clue_table))

    with open("clue_table.txt", "w") as file:
        for addr, meta in clue_table.items():
            file.write(f"{addr:#x} {meta['rsp_offset']} {meta['function_name']} {meta['handler_indicator']}\n")

if __name__ == "__main__":
    main()
