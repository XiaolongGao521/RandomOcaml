let split lst n = 
  let rec split_aux lst n acc = match lst with
  | [] as l -> (acc, l)
  | hd :: tl -> if n == 0 then (hd :: acc |> List.rev, tl) else split_aux tl (n - 1) (hd :: acc)
  in split_aux lst n []
in let print_tuple tuple =
  let (x, _) = tuple in 
  List.iter prerr_endline x in 
split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 |> print_tuple
