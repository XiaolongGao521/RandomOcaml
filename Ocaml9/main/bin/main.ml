let pack lst = 
  let rec pack_aux lst acc1 acc2 = match lst with
    | [] -> acc2
    | hd :: [] -> (hd :: acc1) :: acc2
    | hd1 :: (hd2 :: _ as t) -> if hd1 == hd2 then pack_aux t (hd1 :: acc1) acc2 else pack_aux t [] ((hd1 :: acc1) :: acc2)
  in pack_aux lst [] [] |> List.rev
in pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] |> List.iter (List.iter print_endline)
