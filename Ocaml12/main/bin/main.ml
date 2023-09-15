type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode lst = 
  let rec many_aux k c acc = if k == 0 then acc else many_aux (k - 1) c (c :: acc) in
  let rec decode_aux lst acc = match lst with
   | [] -> acc
   | One c :: tl -> decode_aux tl (c :: acc)
   | Many (k, c) :: tl -> decode_aux tl (many_aux k c acc)
  in decode_aux lst [] |> List.rev
in decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |> List.iter print_endline
  
