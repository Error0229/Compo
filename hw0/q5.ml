let rec_square_sum l =
  let rec aux res l =
    match l with
    | [] -> res
    | x :: xs -> aux (res + (x * x)) xs
  in
  aux 0 l

let square_sum l = l |> List.map (fun x -> x * x) |> List.fold_left ( + ) 0

let () = Printf.printf "%d\n" (rec_square_sum [ 10; 2; 3 ])

let rec_find_opt x l =
  let rec aux id l =
    match l with
    | [] -> None
    | x' :: xs -> if x' = x then Some id else aux (id + 1) xs
  in
  aux 0 l

let find_opt x l = List.find_index (fun x' -> x = x') l

let () =
  match find_opt 4 [ 10; 2; 3; 4; 5; 7 ] with
  | Some index -> Printf.printf "Found at index: %d\n" index
  | None -> Printf.printf "Not found\n"
