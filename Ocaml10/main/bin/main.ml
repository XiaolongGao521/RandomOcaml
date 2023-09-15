let print_tuple tup = 
  let x, y = tup in x |> string_of_int |> print_string; y |> print_endline;;

let encode lst = 
  let rec encode_aux lst count acc = match lst with
    | [] -> acc
    | hd :: [] -> (count, hd) :: acc
    | hd1 :: (hd2 :: _ as t) -> if hd1 == hd2 then encode_aux t (count + 1) acc else encode_aux t 0 ((count + 1, hd1) :: acc)
  in encode_aux lst 0 [] |> List.rev
in encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter print_tuple