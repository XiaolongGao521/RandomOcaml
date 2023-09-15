type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten lst =
  let rec flatten_aux lst acc = match lst with
    | [] -> acc
    | One x :: tl -> flatten_aux tl (x :: acc)
    | Many node_lst :: tl -> flatten_aux tl (flatten_aux node_lst acc)
  in flatten_aux lst [] |> List.rev
in flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] |> List.iter print_endline