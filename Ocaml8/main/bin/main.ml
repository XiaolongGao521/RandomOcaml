let compress lst = 
  let rec compress_aux lst acc = match lst with
    | [] -> acc
    | hd :: [] -> hd :: acc
    | hd1 :: (hd2 :: _ as t) -> if hd1 = hd2 then compress_aux t acc else compress_aux t (hd1 :: acc)
  in compress_aux lst [] |> List.rev
in compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter print_endline