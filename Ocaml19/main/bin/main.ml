let rotate lst n = 
  let rec rotate_aux lst n acc = match lst with
  | [] -> List.rev acc
  | (hd :: tl as l) -> if n = 0 then List.append l (List.rev acc) else rotate_aux tl (n - 1) (hd :: acc)
in let modulo x y =
    let result = x mod y in
    if result >= 0 then result
    else result + y
  in let length = List.length lst
  in rotate_aux lst (modulo n length) []
in rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 |> List.iter print_endline
