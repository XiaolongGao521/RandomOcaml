type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let print_rle encoding = match encoding with
  | One x -> x |> print_endline; 
  | Many (x, y) -> x |> string_of_int |> print_string; y |> print_endline;;

let encode lst = 
  let rec encode_aux lst count acc = match lst with
    | [] -> acc
    | hd :: [] -> (if count = 0 then One hd else Many (count + 1, hd)) :: acc
    | hd1 :: (hd2 :: _ as t) -> 
      if hd1 == hd2 
      then encode_aux t (count + 1) acc 
      else encode_aux t 0 ((if count = 0 then One hd1 else Many (count + 1, hd1)) :: acc)
  in encode_aux lst 0 [] |> List.rev
in encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter print_rle