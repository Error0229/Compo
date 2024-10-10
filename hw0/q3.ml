(* a *)
let reverse s =
  let ls = String.length s in
  let rec aux result i =
    if i = ls then result
    else aux (result ^ String.make 1 (String.get s i)) (i + 1)
  in
  aux "" 0

let o_o_palindrome s = String.equal s (reverse s);;

let s = "madam" in
Printf.printf
  "%s is %sa palindrome\n"
  s
  (if o_o_palindrome "madam" then "" else "not ")
;;

let s = "madam" in
let rec palindrome s : bool =
  match String.length s with
  | 0 | 1 -> true
  | len when String.get s 0 = String.get s (len - 1) ->
    palindrome (String.sub s 1 (len - 2))
  | _ -> false
in
Printf.printf
  "%s is %sa palindrome\n"
  s
  (if palindrome "madam" then "" else "not ")

(* b *)

let a = "cake"

let b = "coke"

let compare a b : bool =
  let len1 = String.length a in
  let len2 = String.length b in
  let rec for' i =
    if i >= len1 then true
    else if i >= len2 then false
    else if String.get a i = String.get b i then for' (i + 1)
    else String.get a i < String.get b i
  in
  for' 0

let () =
  Printf.printf
    "%s is %s then %s\n"
    a
    (if compare a b then "greater" else "smaller")
    b

let cake = "Cake"

let cheese_cake = "Cheese Cake"

(* c *)
let rec factor m1 m2 =
  let l1 = String.length m1 in
  let l2 = String.length m2 in
  if l1 > l2 then false
  else
    let rec for' i =
      if i + l1 > l2 then false
      else if m1 = String.sub m2 i l1 then true
      else for' (i + 1)
    in
    for' 0

let () =
  Printf.printf
    "%s is %sa factor of %s\n"
    cake
    (if factor cake cheese_cake then "" else "not ")
    cheese_cake
