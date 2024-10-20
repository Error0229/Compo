(* Programming exercises from Chapter 2 *)

(* The abstract syntax of mini-ML expressions (as in Chapter 1) *)

type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression

(* Substitution of a variable x by an expression b in an expression a (as in
   Chapter 1) *)
(* Function to print an expression *)
(* Function to print an expression with indentation *)
let rec print_expression ?(indent = 0) e =
  let indent_str = String.make indent ' ' in
  match e with
  | Var v -> print_string (indent_str ^ v)
  | Const n ->
    print_string indent_str;
    print_int n
  | Op op -> print_string (indent_str ^ op)
  | Fun (x, e1) ->
    print_string (indent_str ^ "Fun " ^ x ^ " ->\n");
    print_expression ~indent:(indent + 2) e1
  | App (e1, e2) ->
    print_string (indent_str ^ "App (\n");
    print_expression ~indent:(indent + 2) e1;
    print_string ",\n";
    print_expression ~indent:(indent + 2) e2;
    print_string ("\n" ^ indent_str ^ ")")
  | Pair (e1, e2) ->
    print_string (indent_str ^ "Pair (\n");
    print_expression ~indent:(indent + 2) e1;
    print_string ",\n";
    print_expression ~indent:(indent + 2) e2;
    print_string ("\n" ^ indent_str ^ ")")
  | Let (x, e1, e2) ->
    print_string (indent_str ^ "Let " ^ x ^ " =\n");
    print_expression ~indent:(indent + 2) e1;
    print_string ("\n" ^ indent_str ^ "in\n");
    print_expression ~indent:(indent + 2) e2

let rec subst e x v =
  match e with
  | Var y -> if y = x then v else e
  | Const _ | Op _ -> e
  | Fun (y, e1) -> if y = x then e else Fun (y, subst e1 x v)
  | App (e1, e2) -> App (subst e1 x v, subst e2 x v)
  | Pair (e1, e2) -> Pair (subst e1 x v, subst e2 x v)
  | Let (y, e1, e2) -> Let (y, subst e1 x v, if y = x then e2 else subst e2 x v)

(* Tests if an expression is a value *)

let rec is_value e =
  match e with
  | Var v -> false
  | Const n -> true
  | Op o -> true
  | Fun (x, e1) -> true
  | App (e1, e2) -> false
  | Pair (e1, e2) -> is_value e1 && is_value e2
  | Let (v, e1, e2) -> false

(* One step of head reduction of the term *)

exception Head_reduction_impossible

let head_reduction e =
  match e with
  | App (Fun (x, e1), e2) when is_value e2 ->
    (* beta_fun *)
    subst e1 x e2
  | Let (x, e1, e2) when is_value e1 ->
    (* beta_let *)
    subst e2 x e1
  | App (Op "+", Pair (Const n1, Const n2)) ->
    (* delta_plus *)
    Const (n1 + n2)
  | App (Op "-", Pair (Const n1, Const n2)) ->
    (* delta_minus *)
    Const (n1 - n2)
  | App (Op "*", Pair (Const n1, Const n2)) ->
    (* delta_times *)
    Const (n1 * n2)
  | App (Op "fst", Pair (e1, e2)) when is_value e1 && is_value e2 ->
    (* delta_fst *)
    e1
  | App (Op "snd", Pair (e1, e2)) when is_value e1 && is_value e2 ->
    (* delta_snd *)
    e2
  | App (Op "fix", Fun (x, e2)) ->
    (* delta_fix *)
    subst e2 x e
  | App (Op "ifzero", Pair (Const 0, Pair (e1, e2))) ->
    (* delta_if *)
    e1
  | App (Op "ifzero", Pair (Const n, Pair (e1, e2))) ->
    (* delta_if' *)
    e2
  | _ -> raise Head_reduction_impossible

(* Representation of contexts using Caml functions *)

let hole (e : expression) = e (* the context reduced to [] *)

let app_left ctx e2 (* the context (gamma e2) *) (e : expression) =
  App (ctx e, e2)

let app_right v1 ctx (* the context (v1 gamma) *) (e : expression) =
  App (v1, ctx e)

let pair_left ctx e2 (* the context (gamma, e2) *) (e : expression) =
  Pair (ctx e, e2)

let pair_right v1 ctx (* the context (v1, gamma) *) (e : expression) =
  Pair (v1, ctx e)

let let_left x ctx e2 (* the context (let x = gamma in e2) *) (e : expression) =
  Let (x, ctx e, e2)

(* Decomposes an expression into a context and a subexpression to evaluate *)

exception Unclosed_expression of string

exception Normal_form

let rec decompose e =
  match e with
  | Var v -> raise (Unclosed_expression v)
  | Const _ | Op _ | Fun (_, _) -> raise Normal_form
  (* Here we recognize the cases where we can reduce at the head. We then return
     the trivial context [] and the expression a itself. *)
  | App (Fun (x, e1), e2) when is_value e2 -> (hole, e)
  | Let (x, e1, e2) when is_value e1 -> (hole, e)
  | App (Op ("+" | "-" | "*"), Pair (Const n1, Const n2)) -> (hole, e)
  | App (Op ("fst" | "snd"), Pair (e1, e2)) when is_value e1 && is_value e2 ->
    (hole, e)
  | App (Op "fix", Fun (x, e2)) -> (hole, e)
  | App (Op "ifzero", Pair (Const n, Pair (e1, e2))) -> (hole, e)
  (* Now, we see if we can reduce within the subexpressions *)
  | App (e1, e2) ->
    if is_value e1 then
      (* e1 is already evaluated, we need to delve into e2 *)
      let ctx, rd = decompose e2 in
      (app_right e1 ctx, rd)
    else
      (* e1 is not yet evaluated, that's where we need to delve *)
      let ctx, rd = decompose e1 in
      (app_left ctx e2, rd)
  | Pair (e1, e2) ->
    if is_value e1 then
      if is_value e2 then raise Normal_form
      else
        (* e1 is already evaluated, we need to delve into e2 *)
        let ctx, rd = decompose e2 in
        (pair_right e1 ctx, rd)
    else
      (* e1 is not yet evaluated, that's where we need to delve *)
      let ctx, rd = decompose e1 in
      (pair_left ctx e2, rd)
  | Let (x, e1, e2) ->
    (* We know that e1 is not a value, otherwise the beta-let case above would
       apply. So we will delve into e1. *)
    let ctx, rd = decompose e1 in
    (let_left x ctx e2, rd)

(* Performs one reduction step a -> a' and returns Some(a') or returns None if a
   is in normal form *)

let reduction e =
  try
    let ctx, e' = decompose e in
    Some (ctx (head_reduction e'))
  with Normal_form -> None

(* Reduces until a normal form is obtained *)

let rec normal_form e =
  match reduction e with
  | None -> e
  | Some e' -> normal_form e'
;;

(* For testing *)
normal_form (App (Op "+", Pair (Const 1, Const 2)));;

normal_form (Let ("x", Op "+", App (Var "x", Pair (Const 1, Const 2))));;

normal_form
  (Pair
     ( App (Op "+", Pair (Const 1, Const 2))
     , App (Op "+", Pair (Const 3, Const 4)) ))

let fact =
  App
    ( Op "fix"
    , Fun
        ( "fact"
        , Fun
            ( "n"
            , App
                ( Op "ifzero"
                , Pair
                    ( Var "n"
                    , Pair
                        ( Const 1
                        , App
                            ( Op "*"
                            , Pair
                                ( Var "n"
                                , App
                                    ( Var "fact"
                                    , App (Op "-", Pair (Var "n", Const 1)) ) )
                            ) ) ) ) ) ) )
;;

print_expression fact;;

print_newline ();;

print_expression (normal_form (App (fact, Const 0)));;

print_newline ();;

print_expression (normal_form (App (fact, Const 5)));;

print_newline ()
(* normal_form (App (fact, Const (-1))) *)
