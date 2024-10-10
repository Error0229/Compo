let l = List.init 1000001 (fun x -> x)

let rev l =
  let rec aux res l =
    match l with
    | [] -> res
    | x :: xs -> aux (x :: res) xs
  in
  aux [] l

let print_list l = print_endline (String.concat " " (List.map string_of_int l))

let () = print_list (rev l)

let map f l =
  let rec aux res l =
    match l with
    | [] -> rev res
    | x :: xs -> aux (f x :: res) xs
  in
  aux [] l

let () = print_list (map (fun x -> x * x * x) l)
