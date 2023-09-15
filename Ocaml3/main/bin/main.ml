let rec at k lst = match lst with
  | [] -> None
  | hd :: _ when k = 1 -> Some hd
  | _ :: tl ->  at (k-1) tl
in match at 3 ["a"] with
| None -> print_endline "None"
| Some x -> prerr_string x; prerr_newline ()
