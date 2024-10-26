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

let print_Cmap cm = Cset.iter (fun (c, i) -> Printf.printf "%c%i " c i) cm

let print_Smap sm =
  Smap.iter
    (fun q i ->
      Printf.printf "state %d:" i;
      print_Cmap q;
      print_newline ())
    sm

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

let rec alphabets : regexp -> Cset.t = function
  | Epsilon | Star _ -> Cset.empty
  (* | Character c -> if c <> eof then Cset.singleton c else Cset.empty *)
  | Character c -> Cset.singleton c
  | Concat (r1, r2) | Union (r1, r2) -> Cset.union (alphabets r1) (alphabets r2)

let next_state : regexp -> Cset.t -> char -> Cset.t =
 fun r q c ->
  Cset.fold
    (fun ci acc -> if fst ci = c then Cset.union (follow ci r) acc else acc)
    q
    Cset.empty

let make_dfa : regexp -> autom =
 fun r ->
  let r = Concat (r, Character eof) in
  (* let w = alphabets r in *)
  (* transitions under construction *)
  let trans = ref Smap.empty in
  let rec transitions q =
    if not (Smap.mem q !trans) then (
      let delta =
        Cset.fold
          (fun ic acc ->
            let c = fst ic in
            let q' = next_state r q c in
            Cmap.add c q' acc)
          (* w *)
          q
          Cmap.empty
      in
      trans := Smap.add q delta !trans;
      Cmap.iter (fun _ q' -> transitions q') delta)
    (* the transitions function constructs all the transitions of the state q,
       if this is the first time q is visited *)
  in

  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans }

let is_final state = Cset.mem eof state

let recognize (aut : autom) (s : string) : bool =
  let q0 = aut.start in
  try
    is_final
      (String.fold_left
         (fun q_now ch ->
           let q's = Smap.find q_now aut.trans in
           try Cmap.find ch q's
           with Not_found -> failwith "No transition for current character")
         q0
         s)
  with _ -> false

let number_states (aut : autom) =
  let numbering = ref Smap.empty in
  let counter = ref 0 in
  Smap.iter
    (fun q v ->
      (* print_Cmap q; *)
      if not (Smap.mem q !numbering) then (
        numbering := Smap.add q !counter !numbering;
        counter := !counter + 1;
        Cmap.iter
          (fun _ q' ->
            if not (Smap.mem q' !numbering) then (
              numbering := Smap.add q' !counter !numbering;
              counter := !counter + 1))
          v))
    aut.trans;
  !numbering

let generate_state (fmt : Format.formatter) (state : state) (state_num : int)
    (numbering : int Smap.t) (aut : autom) =
  Format.fprintf fmt "state%d b = @\n" state_num;

  if is_final state then (
    Format.fprintf fmt "  b.last <- b.current;@\n";
    Format.fprintf fmt "  failwith \"token founded\"")
  else (
    Format.fprintf fmt "  try@\n";
    Format.fprintf fmt "  let nc = next_char b in @\n";
    let trans = try Smap.find state aut.trans with Not_found -> Cmap.empty in
    if Cmap.is_empty trans then
      Format.fprintf fmt "    failwith \"lexical error\"@\n"
    else (
      Format.fprintf fmt "    match nc with@\n";
      Cmap.iter
        (fun ch next_state ->
          let next_state_num = Smap.find next_state numbering in
          Format.fprintf fmt "    | '%c' -> state%d b@\n" ch next_state_num)
        trans;
      Format.fprintf fmt "    | _ -> failwith \"lexical error\"@\n");
    Format.fprintf fmt "  with End_of_file -> raise End_of_file@\n")

let generate (filename : string) (aut : autom) =
  let ch = open_out filename in
  let fmt = Format.formatter_of_out_channel ch in
  Format.fprintf
    fmt
    "type buffer = { text: string; mutable current: int; mutable last: int }@\n";
  Format.fprintf fmt "let next_char b =@\n";
  Format.fprintf
    fmt
    "  if b.current = String.length b.text then raise End_of_file;@\n";
  Format.fprintf fmt "  let c = b.text.[b.current] in@\n";
  Format.fprintf fmt "  b.current <- b.current + 1;@\n";
  Format.fprintf fmt "  c@\n@\n";
  let numbering = number_states aut in
  (* print_Smap numbering; *)
  let first = ref true in
  Smap.iter
    (fun s i ->
      if !first then (
        Format.fprintf fmt "let rec ";
        first := false)
      else Format.fprintf fmt "and ";
      generate_state fmt s i numbering aut;
      Format.fprintf fmt "@\n")
    numbering;
  Format.fprintf
    fmt
    "let start = state%d"
    (try Smap.find aut.start numbering
     with Not_found -> failwith "No start state?");
  close_out ch

(* Ex 1 *)
let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))));
  print_endline "Excerise 1 passed 🎉"

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
  print_endline "Excerise 2 passed 🎉"

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
  print_endline "Excerise 3 passed 🎉"

(* Ex 4 *)
let fprint_state fmt q =
  Cset.iter
    (fun (c, i) ->
      if c = '#' then Format.fprintf fmt "# "
      else Format.fprintf fmt "%c%i " c i)
    q

let fprint_transition fmt q c q' =
  Format.fprintf
    fmt
    "\"%a\" -> \"%a\" [label=\"%c\"];@\n"
    fprint_state
    q
    fprint_state
    q'
    c

let fprint_autom fmt a =
  Format.fprintf fmt "digraph A {@\n";
  Format.fprintf fmt " @[\"%a\" [ shape = \"rect\"];@\n" fprint_state a.start;
  Smap.iter
    (fun q t -> Cmap.iter (fun c q' -> fprint_transition fmt q c q') t)
    a.trans;
  Format.fprintf fmt "@]@\n}@."

let save_autom file a =
  let ch = open_out file in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprint_autom a;
  close_out ch

(* Ex 4 *)
(* (a|b)*a(a|b) *)
let r =
  Concat
    ( Star (Union (Character ('a', 1), Character ('b', 1)))
    , Concat (Character ('a', 2), Union (Character ('a', 3), Character ('b', 2)))
    )

let a = make_dfa r

let () =
  save_autom "autom.dot" a;
  print_endline "Exercise 4 passed 🎉 (kinda)"

(* Ex 5 *)
let () = assert (recognize a "aa")

let () = assert (recognize a "ab")

let () = assert (recognize a "abababaab")

let () = assert (recognize a "babababab")

let () = assert (recognize a (String.make 1000 'b' ^ "ab"))

let () = assert (not (recognize a ""))

let () = assert (not (recognize a "a"))

let () = assert (not (recognize a "b"))

let () = assert (not (recognize a "ba"))

let () = assert (not (recognize a "aba"))

let () = assert (not (recognize a "abababaaba"))

let r =
  Star
    (Union
       ( Star (Character ('a', 1))
       , Concat
           ( Character ('b', 1)
           , Concat (Star (Character ('a', 2)), Character ('b', 2)) ) ))

let a = make_dfa r

let () = save_autom "autom2.dot" a

let () = assert (recognize a "")

let () = assert (recognize a "bb")

let () = assert (recognize a "aaa")

let () = assert (recognize a "aaabbaaababaaa")

let () = assert (recognize a "bbbbbbbbbbbbbb")

let () = assert (recognize a "bbbbabbbbabbbabbb")

let () = assert (not (recognize a "b"))

let () = assert (not (recognize a "ba"))

let () = assert (not (recognize a "ab"))

let () = assert (not (recognize a "aaabbaaaaabaaa"))

let () = assert (not (recognize a "bbbbbbbbbbbbb"))

let () = assert (not (recognize a "bbbbabbbbabbbabbbb"))

let () = print_endline "Exercise 5 passed 🎉 "

(* Ex 6 *)

(* a*b *)
let r3 = Concat (Star (Character ('a', 1)), Character ('b', 1))

let a = make_dfa r3

let () =
  generate "a.ml" a;
  print_endline "Exercise 6 passed 🎉 maybe?"

let r4 =
  Concat
    ( Concat
        ( Union (Character ('b', 1), Epsilon)
        , Star (Concat (Character ('a', 1), Character ('b', 2))) )
    , Union (Character ('a', 2), Epsilon) )

let () = generate "b.ml" (make_dfa r4)
