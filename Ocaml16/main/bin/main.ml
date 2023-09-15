let drop lst n = 
  let rec drop_aux lst k n acc = match lst with
  | [] -> acc
  | hd :: tl -> if k == n then drop_aux tl 1 n acc else drop_aux tl (k + 1) n (hd :: acc)
  in drop_aux lst 1 n [] |> List.rev
in drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 |> List.iter print_endline