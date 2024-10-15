let merge_sort l =
  let split l =
    let rec aux left right l =
      match l with
      | [] -> (left, right)
      | [ x ] -> (left @ [ x ], right)
      | x1 :: x2 :: xs -> aux (left @ [ x1 ]) (right @ [ x2 ]) xs
    in
    aux [] [] l
  and merge l1 l2 =
    let rec aux res l1 l2 =
      match (l1, l2) with
      | [], [] -> res
      | l1' :: ls, [] -> aux (res @ [ l1' ]) ls l2
      | [], l2' :: ls -> aux (res @ [ l2' ]) l1 ls
      | l1' :: ls1, l2' :: ls2 ->
        if l1' < l2' then aux (res @ [ l1' ]) ls1 l2
        else aux (res @ [ l2' ]) l1 ls2
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
