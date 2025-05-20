open Syntax__Common

type hopper_cell =
  | Label of label_index
  | Capability of cap_index
  | NA

type hopper_res = 
  | Empty
  | Const of string
  | Regular of string
let effect_num = 16

let hopper_res_to_str res =
  match res with
  | Label index -> (
    match index with
    | LabelIndex i -> Printf.sprintf "0%X" i
    | LabelInfty -> Printf.sprintf "20"
  )
  | Capability index -> (
    match index with
    | CapIndex i -> Printf.sprintf "1%X" i
    | CapInfty -> Printf.sprintf "20"
  )
  | NA -> "FF"

let hopper_is_const (hopper: int * int * hopper_cell list) =
  let cap_num, label_num, table = hopper in
  let is_const = ref true in
  let table = ref table in

  for _ = 0 to effect_num - 1 do
    (match !table with
    | (Label LabelInfty)::rest | (Capability CapInfty)::rest | NA::rest -> table := rest;
    | _ -> is_const := false);

    for cap_index = 0 to cap_num - 1 do
      (match !table with
      | (Capability CapIndex cap_index')::rest -> 
        if cap_index = cap_index' then table := rest else is_const := false
      | NA::rest -> table := rest;
      | _ -> is_const := false);
    done;

    for label_index = 0 to label_num - 1 do
      (match !table with
      | (Label LabelIndex label_index')::rest -> 
        if label_index = label_index' then table := rest else is_const := false
      | NA::rest -> table := rest;
      | _ -> is_const := false);
    done
  done;
  !is_const

let hopper (metadata: metadata) = 
  let (captured_set, cap_insts, label_insts) = metadata in

  let res = ref [] in
  for effect_index = 0 to effect_num - 1 do
    let captured_set_res =
      (
        let (cap, labels) = captured_set in
        match List.find_opt (fun (_, ei) -> ei = effect_index) labels with
        | Some (li, _) -> Label li
        | None -> (
          match cap with
          | Some ci -> Capability ci
          | None -> NA
      )) in

    let label_insts_res = List.fold_right (fun li ls -> (Label li)::ls) label_insts [] in

    let cap_insts_res = List.fold_right (fun (cap, labels) ls ->
      match List.find_opt (fun (_, ei) -> ei = effect_index) labels with
      | Some (li, _) -> (Label li)::ls
      | None -> (
        match cap with
        | Some ci -> (Capability ci)::ls
        | None -> (NA)::ls
      )) cap_insts label_insts_res in

    res := (!res)@(captured_set_res::cap_insts_res)
  done;
  (List.length cap_insts), (List.length label_insts), !res

let parse_hopper (hopper: int * int * hopper_cell list) = 
  let cap_num, label_num, table = hopper in
  let table_flattened = List.fold_right (fun res str -> (hopper_res_to_str res)^str) table "" in
  let is_const = if (hopper_is_const hopper) then 1 else 0 in
  Printf.sprintf "%d_%d_%d_%s" is_const cap_num label_num table_flattened


let hopper_parsed metadata = parse_hopper (hopper metadata)

(* Returns the parsed metadata if it is non-consant. *)
let hopper_parsed_with_res (metadata: metadata) =
  let is_capability_empty (cap: capability) = 
    match cap with
    | Some _, _ -> false
    | None, labels -> List.is_empty labels
  in

  let (captured_set, cap_insts, label_insts) = metadata in
  if (is_capability_empty captured_set)
    && (List.for_all is_capability_empty cap_insts)
    && (List.is_empty label_insts)
  then Empty
  else 
    let hopper_result = hopper metadata in
    if hopper_is_const hopper_result
      then Const (parse_hopper hopper_result)
      else Regular (parse_hopper hopper_result)