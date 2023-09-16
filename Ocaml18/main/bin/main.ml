let slice lst i k = 
  let rec slice_aux lst i k acc = match lst with
  | [] -> acc
  | hd :: tl -> if i = 0 then if k = 0 then hd :: acc else slice_aux tl i (k - 1) (hd :: acc) else slice_aux tl (i - 1) (k - 1) acc
  in slice_aux lst i k [] |> List.rev
in slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 |> List.iter print_endline