#!/usr/bin/env python3

from elftools.dwarf.locationlists import LocationEntry
from collections import defaultdict

def collect_variable_DIEs(die):
    stack = [die]
    while stack:
        current = stack.pop()
        if current.tag in ('DW_TAG_variable'):
            yield current
        stack.extend(current.iter_children())

def parse_variable_locations(elf):
    dwarfinfo = elf.get_dwarf_info()
    variable_locations = defaultdict(list)

    for CU in dwarfinfo.iter_CUs():
        top_DIE = CU.get_top_DIE()
        
        low_pc = top_DIE.attributes.get('DW_AT_low_pc')
        base_addr = low_pc.value if low_pc else 0

        for DIE in top_DIE.iter_children():
            if DIE.tag == 'DW_TAG_subprogram':
                function_name = DIE.attributes.get('DW_AT_name', None)
                if not function_name:
                    continue
                function_name = function_name.value.decode('utf-8')

                for child in collect_variable_DIEs(DIE):
                    var_name_attr = child.attributes.get('DW_AT_name')
                    loc_attr = child.attributes.get('DW_AT_location')
                    if not loc_attr:
                        continue
                    elif not var_name_attr:
                        # If the variable is inside an inlined function,
                        # the DIE of original variable can be referred by the "DW_AT_abstract_origin" attribute.
                        abs_origin_attr = child.attributes.get('DW_AT_abstract_origin')
                        if abs_origin_attr:
                            abs_child = dwarfinfo.get_DIE_from_refaddr(abs_origin_attr.value)
                            var_name_attr = abs_child.attributes.get('DW_AT_name')

                        if not var_name_attr:
                            continue
                            
                    var_name = var_name_attr.value.decode('utf-8')

                    form = loc_attr.form
                    loc_value = loc_attr.value

                    # List of location expressions of the variable based on the current address
                    if form == 'DW_FORM_loclistx':
                        loclist = dwarfinfo.location_lists().get_location_list_at_offset(loc_value, DIE)
                        entries = []
                        for entry in loclist:
                            if isinstance(entry, LocationEntry):
                                entries.append({
                                    'start': base_addr + entry.begin_offset,
                                    'end': base_addr + entry.end_offset,
                                    'expr': entry.loc_expr
                                })
                        variable_locations[function_name].append({
                            'variable': var_name,
                            'locations': entries
                        })

                    # A single location expression
                    elif form == 'DW_FORM_exprloc':
                        entries = [{
                            'start': 0,
                            'end': float('inf'),
                            'expr': loc_value
                        }]
                        variable_locations[function_name].append({
                            'variable': var_name,
                            'locations': entries
                        })

    return variable_locations