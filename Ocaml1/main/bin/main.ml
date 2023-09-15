let rec last lst = match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
in
  match last [1; 2; 3] with 
  | None -> prerr_endline "Nothing"
  | Some x -> print_int x; print_endline ""
