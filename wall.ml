(* the code is adapted from Jean-Christophe Filliâtre *)

let width = 32

let height = 10

let add2 r = (r lsl 2) lor 0b10

let add3 r = (r lsl 3) lor 0b100

let rows = ref []

let rec fill r w =
  if w = width then rows := r :: !rows
  else if w < width then (
    fill (add2 r) (w + 2);
    fill (add3 r) (w + 3))

let () =
  fill 0 2;
  fill 0 3

let () = Printf.printf "%d arrangements\n" (List.length !rows)

let rec sum f l =
  match l with
  | [] -> 0
  | x :: s -> f x + sum f s

(* let table = Hashtbl.create 5003 *)
let size = 1 lsl 16

let table = Array.make size []

let hash (r, h) = ((r lxor (r lsr 16)) + (5003 * h)) land (size - 1)

let add k v =
  let i = hash k in
  table.(i) <- (k, v) :: table.(i)

let find k =
  let rec look_up = function
    | [] -> raise Not_found
    | (k', v) :: s -> if k' = k then v else look_up s
  in
  look_up table.(hash k)

let rec count r h =
  if h = 1 then 1
  else sum (fun r' -> if r' land r = 0 then memo_count r' (h - 1) else 0) !rows

and memo_count r h =
  try find (r, h)
  with Not_found ->
    let v = count r h in
    add (r, h) v;
    v

let sol = sum (fun r -> count r height) !rows

let () = Printf.printf "solution = %d\n" sol
