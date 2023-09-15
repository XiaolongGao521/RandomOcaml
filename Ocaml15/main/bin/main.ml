let replicate lst n =
  let rec prepend_aux c n acc = if n == 0 then acc else prepend_aux c (n - 1) (c :: acc) in
  let rec replicate_aux lst n acc = match lst with
  | [] -> acc
  | hd :: tl -> replicate_aux tl n (prepend_aux hd n acc)
  in replicate_aux lst n [] |> List.rev
in replicate ["a"; "b"; "c"] 3 |> List.iter print_endline