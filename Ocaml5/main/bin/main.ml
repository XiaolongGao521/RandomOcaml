let rev lst = 
  let rec rev_helper lst acc = match lst with
    | [] -> acc
    | hd :: tl -> rev_helper tl (hd :: acc)
  in rev_helper lst []
in List.iter print_endline (rev ["a"; "b"; "c"; "d"])
