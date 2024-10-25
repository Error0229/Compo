type ichar = char * int

type regexp =
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

let rec null : regexp -> bool = function
  | Epsilon | Star _ -> true
  | Character _ -> false
  | Concat (r1, r2) -> null r1 && null r2
  | Union (r1, r2) -> null r1 || null r2

module Cset = struct
  include Set.Make (struct
    type t = ichar

    let compare = Stdlib.compare
  end)

  let union3 (s1 : t) (s2 : t) (s3 : t) : t = union s1 (union s2 s3)
end

type state = Cset.t (* a state is a set of characters *)

module Cmap = Map.Make (Char) (* dictionary whose keys are characters *)
module Smap = Map.Make (Cset) (* dictionary whose keys are states *)

type autom =
  { start : state
  ; trans : state Cmap.t Smap.t
        (* state dictionary -> (character dictionary -> state) *)
  }

let eof = ('#', -1)

let rec first : regexp -> Cset.t = function
  | Epsilon -> Cset.empty
  | Character r -> Cset.singleton r
  | Concat (r1, r2) ->
    if null r1 then Cset.union (first r1) (first r2) else first r1
  | Union (r1, r2) -> Cset.union (first r1) (first r2)
  | Star r -> first r

let rec last : regexp -> Cset.t = function
  | Epsilon -> Cset.empty
  | Character r -> Cset.singleton r
  | Concat (r1, r2) ->
    if null r2 then Cset.union (last r1) (last r2) else last r2
  | Union (r1, r2) -> Cset.union (last r1) (last r2)
  | Star r -> last r

let union3 (s1 : Cset.t) (s2 : Cset.t) (s3 : Cset.t) : Cset.t =
  Cset.union s1 (Cset.union s2 s3)

let rec follow : ichar -> regexp -> Cset.t =
 fun c -> function
  | Epsilon | Character _ -> Cset.empty
  | Concat (r1, r2) ->
    if Cset.mem c (last r1) then
      Cset.union3 (follow c r1) (follow c r2) (first r2)
    else Cset.union (follow c r1) (follow c r2)
  | Union (r1, r2) -> Cset.union (follow c r1) (follow c r2)
  | Star r ->
    if Cset.mem c (last r) then Cset.union (follow c r) (first r)
    else follow c r

let next_state : regexp -> Cset.t -> char -> Cset.t =
 fun r q c ->
  let fr = first r in
  List.fold_left
    (fun acc ic -> if fst ic = c then Cset.union (follow ic r) acc else acc)
    q
    (Cset.to_list fr)

let make_dfa : regexp -> autom = 
  fun r -> 
let r = Concat (r, Character eof) in
(* transitions under construction *)
let trans = ref Smap.empty in
let rec transitions q =
(* the transitions function constructs all the transitions of the state q,
if this is the first time q is visited *)
in 
let q0 = first r in
transitions q0;
{ start = q0; trans = !trans }
(* Ex 1 *)
let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))));
  print_endline "Excerise 1 passed🎉"

(* Ex 2 *)
let () =
  let ca = ('a', 0)
  and cb = ('b', 0) in
  let a = Character ca
  and b = Character cb in
  let ab = Concat (a, b) in
  let eq = Cset.equal in
  assert (eq (first a) (Cset.singleton ca));
  assert (eq (first ab) (Cset.singleton ca));
  assert (eq (first (Star ab)) (Cset.singleton ca));
  assert (eq (last b) (Cset.singleton cb));
  assert (eq (last ab) (Cset.singleton cb));
  assert (Cset.cardinal (first (Union (a, b))) = 2);
  assert (Cset.cardinal (first (Concat (Star a, b))) = 2);
  assert (Cset.cardinal (last (Concat (a, Star b))) = 2);
  print_endline "Excerise 2 passed🎉"

(* Ex 3 *)
let () =
  let ca = ('a', 0)
  and cb = ('b', 0) in
  let a = Character ca
  and b = Character cb in
  let ab = Concat (a, b) in
  assert (Cset.equal (follow ca ab) (Cset.singleton cb));
  assert (Cset.is_empty (follow cb ab));
  let r = Star (Union (a, b)) in
  assert (Cset.cardinal (follow ca r) = 2);
  assert (Cset.cardinal (follow cb r) = 2);
  let r2 = Star (Concat (a, Star b)) in
  assert (Cset.cardinal (follow cb r2) = 2);
  let r3 = Concat (Star a, b) in
  assert (Cset.cardinal (follow ca r3) = 2);
  print_endline "Excerise 3 passed🎉"
