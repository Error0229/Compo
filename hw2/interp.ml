open Ast
open Format

(* Exception raised to signal a runtime error *)
exception Error of string

let error s = raise (Error s)

(* Values of Mini-Python.

   Two main differences wrt Python:

   - We use here machine integers (OCaml type `int`) while Python integers are
   arbitrary-precision integers (we could use an OCaml library for big integers,
   such as zarith, but we opt for simplicity here).

   - What Python calls a ``list'' is a resizeable array. In Mini-Python, there
   is no way to modify the length, so a mere OCaml array can be used. *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Print a value on standard output *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n - 1 do
      print_value a.(i);
      if i < n - 1 then printf ", "
    done;
    printf "]"

(* Boolean interpretation of a value

   In Python, any value can be used as a Boolean: None, the integer 0, the empty
   string, and the empty list are all considered to be False, and any other
   value to be True. *)
let is_false v =
  match v with
  | Vnone -> true
  | Vint n -> n = 0
  | Vstring s -> s = ""
  | Vlist l -> Array.length l = 0
  | Vbool b -> not b

let is_true v = not (is_false v)
(* We only have global functions in Mini-Python *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* The following exception is used to interpret `return` *)

exception Return of value

(* Local variables (function parameters and local variables introduced by
   assignments) are stored in a hash table that is passed to the following OCaml
   functions as parameter `ctx`. *)

type ctx = (string, value) Hashtbl.t

let rec compare_list l1 l2 =
  let l1l = Array.length l1 in
  let l2l = Array.length l2 in
  let rec aux i =
    if i = l1l && i = l2l then 0
    else if i = l1l then -1
    else if i = l2l then 1
    else
      let c = compare l1.(i) l2.(i) in
      if c <> 0 then c else aux (i + 1)
  in
  aux 0

let rec compare_value v1 v2 =
  match (v1, v2) with
  | Vlist l1, Vlist l2 -> compare_list l1 l2
  | _ -> compare v1 v2

(* Interpreting an expression (returns a value) *)

let rec expr ctx = function
  | Ecst Cnone -> Vnone
  | Ecst (Cstring s) -> Vstring s
  (* arithmetic *)
  | Ecst (Cint n) -> Vint (Int64.to_int n)
  | Ebinop
      ( ((Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge)
         as op)
      , e1
      , e2 ) -> (
    let v1 = expr ctx e1 in
    let v2 = expr ctx e2 in
    match (op, v1, v2) with
    | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
    | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
    | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
    | Bdiv, Vint n1, Vint n2 -> (
      match n2 with
      | 0 -> error "division zero error"
      | _ -> Vint (n1 / n2))
    | Bmod, Vint n1, Vint n2 -> (
      match n2 with
      | 0 -> error "division zero error"
      | _ -> Vint (n1 mod n2))
    | Beq, _, _ -> Vbool (compare_value v1 v2 = 0)
    | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
    | Blt, _, _ -> Vbool (compare_value v1 v2 < 0)
    | Ble, _, _ -> Vbool (compare_value v1 v2 <= 0)
    | Bgt, _, _ -> Vbool (compare_value v1 v2 > 0)
    | Bge, _, _ -> Vbool (compare_value v1 v2 >= 0)
    | Badd, Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
    | Badd, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
    | _ -> error "unsupported operand types🗿")
  | Eunop (Uneg, e1) -> (
    match expr ctx e1 with
    | Vint n -> Vint (-n)
    | _ -> error "Unsupported operand types🗿")
  (* Boolean *)
  | Ecst (Cbool b) -> Vbool b
  | Ebinop (Band, e1, e2) ->
    Vbool (if is_false (expr ctx e1) then false else is_true (expr ctx e2))
  | Ebinop (Bor, e1, e2) ->
    Vbool (if is_true (expr ctx e1) then true else is_true (expr ctx e2))
  | Eunop (Unot, e1) -> Vbool (is_false (expr ctx e1))
  | Eident { id } -> (
    try Hashtbl.find ctx id
    with Not_found -> error ("identifier not found: " ^ id))
  (* function call *)
  | Ecall ({ id = "len" }, [ e1 ]) -> (
    match expr ctx e1 with
    | Vlist l -> Vint (Array.length l)
    | _ -> error "Not an array 🗿")
  | Ecall ({ id = "list" }, [ Ecall ({ id = "range" }, [ e1 ]) ]) -> (
    match expr ctx e1 with
    | Vint n -> Vlist (Array.init (max 0 n) (fun i -> Vint i))
    | _ -> error "range must be a int")
  | Ecall ({ id = f }, el) -> (
    let ctx' = Hashtbl.create 16 in
    let fps, body = Hashtbl.find functions f in
    List.iter2 (fun pe { id = fp } -> Hashtbl.add ctx' fp (expr ctx pe)) el fps;
    try
      stmt ctx' body;
      Vnone
    with Return v -> v)
  | Elist el -> Vlist (Array.of_list (List.map (fun e' -> expr ctx e') el))
  | Eget (e1, e2) -> (
    match expr ctx e2 with
    | Vint v -> (
      match expr ctx e1 with
      | Vlist vl -> ( try vl.(v) with _ -> error "index out of bound")
      | _ -> error "[] is only for list")
    | _ -> error "index must be a integer")
(* Interpreting a statement returns nothing but may raise exception `Return` *)

and stmt ctx = function
  | Seval e -> ignore (expr ctx e)
  | Sprint e ->
    print_value (expr ctx e);
    printf "@."
  | Sblock bl -> block ctx bl
  | Sif (e, s1, s2) -> if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Sassign ({ id }, e1) ->
    if Hashtbl.mem ctx id then Hashtbl.replace ctx id (expr ctx e1)
    else Hashtbl.add ctx id (expr ctx e1)
  | Sreturn e -> raise (Return (expr ctx e))
  | Sfor ({ id }, e, s) -> (
    match expr ctx e with
    | Vlist vl ->
      Array.iter
        (fun v ->
          Hashtbl.replace ctx id v;
          stmt ctx s)
        vl
    | _ -> error "can't iterate such type")
  | Sset (e1, e2, e3) -> (
    match expr ctx e2 with
    | Vint v -> (
      match expr ctx e1 with
      | Vlist vl -> (
        try vl.(v) <- expr ctx e3 with _ -> error "index out of bound")
      | _ -> error "[] is only for list")
    | _ -> error "index must be a integer")
(* Interpreting a block (a sequence of statements) *)

and block ctx = function
  | [] -> ()
  | s :: sl ->
    stmt ctx s;
    block ctx sl

(* Interpreting a file - `dl` is a list of function definitions (see type `def`
   in ast.ml) - `s` is a statement (the toplevel code) *)

let file (dl, s) =
  List.iter
    (fun (ident, param, body) -> Hashtbl.add functions ident.id (param, body))
    dl;
  stmt (Hashtbl.create 16) s
