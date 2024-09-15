let merge_sort l =
  let split l =
    let rec aux left right l =
      match l with
      | [] -> (List.rev left, List.rev right)
      | [ x ] -> (List.rev (x :: left), List.rev right)
      | x1 :: x2 :: xs -> aux (x1 :: left) (x2 :: right) xs
    in
    aux [] [] l
  in
  let merge l1 l2 =
    let rec aux res l1 l2 =
      match (l1, l2) with
      | [], [] -> List.rev res
      | l1' :: ls, [] -> aux (l1' :: res) ls l2
      | [], l2' :: ls -> aux (l2' :: res) l1 ls
      | l1' :: ls1, l2' :: ls2 ->
        if l1' > l2' then aux (l2' :: res) l1 ls2 else aux (l1' :: res) ls1 l2
    in
    aux [] l1 l2
  in

  let rec sort l =
    match l with
    | [] -> []
    | [ x ] -> [ x ]
    | l ->
      let left, right = split l in
      merge (sort left) (sort right)
  in
  sort l

let rec print_list = function
  | [] -> ()
  | [ x ] -> Printf.printf "%d\n" x
  | x :: xs ->
    Printf.printf "%d; " x;
    print_list xs
;;

print_list (merge_sort [ 3; 6; 10; 2; 7; 2; 7; 9 ])
