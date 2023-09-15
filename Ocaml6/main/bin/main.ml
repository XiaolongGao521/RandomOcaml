let is_palindrome lst = lst = List.rev lst in
is_palindrome ["a"; "b"] |> string_of_bool |> print_endline
