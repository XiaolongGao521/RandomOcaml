let remove_at lst k = 
  let rec remove_at_aux lst k acc = match lst with
  | [] -> acc
  | hd :: tl -> if k = 0 then remove_at_aux tl (k - 1) acc else remove_at_aux tl (k - 1) (hd :: acc)
in remove_at_aux lst k [] |> List.rev
in remove_at ["a"; "b"; "c"; "d"] 1 |> List.iter print_endline
