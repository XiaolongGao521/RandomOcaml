let length lst = 
  let rec length_helper lst acc = match lst with
    | [] -> acc
    | _ :: tl -> length_helper tl (acc + 1)
  in length_helper lst 0
in length [] |> string_of_int |> print_endline
