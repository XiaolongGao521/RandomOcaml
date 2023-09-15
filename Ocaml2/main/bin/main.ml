let rec last lst = match lst with
  | [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: tl -> last tl
in
  match last [3] with 
  | None -> prerr_endline "Nothing"
  | Some (x, y) -> print_int x; print_string " "; print_int y ; print_newline ()
