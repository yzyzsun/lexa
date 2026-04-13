open Translation

let usage_msg = "lexa <file1>"

let input_file = ref ""

let speclist = []

let rec get_open_filenames filename =
  let toplevels = SLParser.Main.parseFile filename in
  let cur_opens =
    let get_cur_open_filenames = function
      | SLsyntax.TLOpen x -> Some ((Filename.dirname filename) ^ "/" ^ x)
      | _ -> None
    in
    List.filter_map get_cur_open_filenames toplevels
  in
    
  let all_open_filenames = List.concat_map get_open_filenames cur_opens in
  all_open_filenames @ [filename]

let rec dedup l acc =
  match l with
  | h :: t ->
    if List.mem h acc then
      dedup t acc
    else
      h :: (dedup t (h :: acc))
  | [] -> []

let typecheck_file filename =
  let open_filenames = dedup (get_open_filenames filename) [] in
  let sl_toplevels = List.concat_map
    (fun fn -> SLParser.Main.parseFile fn)
    open_filenames
  in
  typecheck_toplevels sl_toplevels

let () = 
  Arg.parse speclist (fun file -> input_file := file) usage_msg;
  typecheck_file !input_file;
  Printf.printf "Typechecking passed!\n"
