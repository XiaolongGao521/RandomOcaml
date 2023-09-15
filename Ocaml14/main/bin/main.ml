let duplicate lst = 
  let rec duplicate_aux lst acc = match lst with
    | [] -> acc
    | hd :: tl -> duplicate_aux tl (hd :: hd :: acc)
  in duplicate_aux lst [] |> List.rev
in duplicate ["a"; "b"; "c"; "c"; "d"] |> List.iter print_endline