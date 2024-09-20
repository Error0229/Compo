type 'a seq =
  | Elt of 'a
  | Seq of 'a seq * 'a seq

let ( @@ ) x y = Seq (x, y)

let rec hd sq =
  match sq with
  | Elt x -> Elt x
  | Seq (x, y) -> hd x

let rec tl sq =
  match sq with
  | Elt x -> sq
  | Seq (a, b) -> (
    match a with
    | Elt x' -> b
    | Seq (x', y') -> tl a @@ b)

let rec mem q sq =
  match sq with
  | Elt x' -> q = x'
  | Seq (x', y') -> mem q x' || mem q y'

let rec print_listn l =
  match l with
  | [] -> print_newline ()
  | x :: xs ->
    Printf.printf "%d " x;
    print_listn xs

let rec print_seq sq =
  match sq with
  | Elt x -> Printf.printf "%d " x
  | Seq (x, y) ->
    print_seq x;
    print_seq y

let print_seqn sq =
  print_seq sq;
  print_newline ()

let rec rev sq =
  match sq with
  | Elt x -> Elt x
  | Seq (x, y) -> rev y @@ rev x

let rec map f sq =
  match sq with
  | Elt x -> Elt (f x)
  | Seq (x, y) -> map f x @@ map f y

let rec fold_left f init sq =
  match sq with
  | Elt x -> f init x
  | Seq (x, y) -> fold_left f (fold_left f init x) y

let rec fold_right f sq init =
  match sq with
  | Elt x -> f x init
  | Seq (x, y) -> fold_right f x (fold_right f y init)

let rec seq2list s = fold_right (fun x l -> x :: l) s []

let find_opt x l =
  let res, _ =
    fold_left
      (fun (res, id) y ->
        if res = None && x = y then (Some id, id + 1) else (res, id + 1))
      (None, 0)
      l
  in
  res

let nth s n =
  match
    fold_left
      (fun (res, id) y -> if id = n then (Some y, id + 1) else (res, id + 1))
      (None, 0)
      s
  with
  | None, _ -> failwith "No such object"
  | Some x, _ -> x

let s = Seq (Seq (Seq (Elt 1, Elt 2), Elt 3), Seq (Seq (Elt 4, Elt 5), Elt 6))

let () = print_seqn (tl s)

let () = Printf.printf "%b\n" (mem 4 s)

let () = print_seq (rev s)

let () = print_newline ()

let () = print_seqn (map (fun x -> x * x) s)

let () = print_int (fold_left (fun x y -> x - y) 0 s)

let () = print_newline ()

let () = print_int (fold_right (fun x y -> x - y) s 0)

let () = print_newline ()

let () = print_listn (seq2list s)

let option_to_string opt =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %d" x

let () = print_endline (option_to_string (find_opt 3 s))

let () =
  print_int (nth s 3);
  print_newline ()
