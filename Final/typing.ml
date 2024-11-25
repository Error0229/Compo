open Ast

(* Debug flag *)
let debug = ref false

(* Unique location for dummy errors *)
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

(* Enhanced type system with polymorphic support *)
type typ =
  | TNone
  | TBool
  | TString
  | TInt
  | TList of typ
  | TAny
  | TFunc of typ list * typ
  | TVar of int (* Polymorphic type variable *)

(* Type variable generation *)
let next_type_var = ref 0

let fresh_type_var () =
  incr next_type_var;
  TVar !next_type_var

(* Exception for type errors *)
exception Error of Ast.location * string

(* Substitution and unification context *)
module SMap = Map.Make (String)

type context =
  { vars : typ SMap.t
  ; funcs : (typ list * typ) SMap.t
  ; constraints : (typ * typ) list
  }

(* Native environment with polymorphic support *)
let native_env =
  { vars = SMap.empty
  ; funcs =
      SMap.of_list
        [ ("len", ([ TList (fresh_type_var ()) ], TInt))
        ; ("range", ([ TInt ], TList TInt))
        ; ("list", ([ TList (fresh_type_var ()) ], TList (fresh_type_var ())))
        ]
  ; constraints = []
  }

(* Error reporting utility *)
let error ?(loc = dummy_loc) fmt =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ fmt ^^ "@]")

(* Unification algorithm *)
let rec unify t1 t2 =
  match (t1, t2) with
  | TVar v1, TVar v2 when v1 = v2 -> t1
  | TVar _, _ -> t2 (* Flexible type variable assignment *)
  | TList elem1, TList elem2 -> TList (unify elem1 elem2)
  | t1, t2 when t1 = t2 -> t1
  | _ -> error "Type mismatch during unification"

(* Polymorphic expression type checking *)
let rec type_expr ctx expr : texpr * typ =
  match expr with
  | Ecst c -> (
    match c with
    | Cnone -> (TEcst c, TNone)
    | Cbool _ -> (TEcst c, TBool)
    | Cint _ -> (TEcst c, TInt)
    | Cstring _ -> (TEcst c, TString))
  | Eident id -> (
    match SMap.find_opt id.id ctx.vars with
    | Some t -> (TEvar { v_name = id.id; v_ofs = 0 }, t)
    | None -> error ~loc:id.loc "Unbound variable %s" id.id)
  | Ebinop (op, e1, e2) -> (
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in
    let unified_type = unify t1 t2 in

    match op with
    | Badd -> (
      match unified_type with
      | TString | TInt | TList _ -> (TEbinop (op, te1, te2), unified_type)
      | _ -> error "Unsupported operand type for +")
    | Bsub | Bmul | Bdiv | Bmod -> (
      match unified_type with
      | TInt -> (TEbinop (op, te1, te2), TInt)
      | _ -> error "Arithmetic operations require integers")
    | Beq | Bneq -> (TEbinop (op, te1, te2), TBool)
    | Blt | Ble | Bgt | Bge -> (
      match unified_type with
      | TInt -> (TEbinop (op, te1, te2), TBool)
      | _ -> error "Comparison requires integers")
    | Band | Bor -> (
      match unified_type with
      | TBool -> (TEbinop (op, te1, te2), TBool)
      | _ -> error "Logical operations require booleans"))
  | Eunop (op, e) -> (
    let te, t = type_expr ctx e in
    match op with
    | Uneg -> (
      match t with
      | TInt -> (TEunop (op, te), TInt)
      | _ -> error "Negation requires an integer")
    | Unot -> (
      match t with
      | TBool -> (TEunop (op, te), TBool)
      | _ -> error "Logical NOT requires a boolean"))
  | Ecall (id, args) -> (
    let fname = id.id in
    match SMap.find_opt fname ctx.funcs with
    | Some (param_types, ret_type) ->
      if List.length param_types <> List.length args then
        error
          ~loc:id.loc
          "Function %s expects %d arguments"
          fname
          (List.length param_types);

      let typed_args =
        List.map2
          (fun param_type arg ->
            let t_arg, arg_type = type_expr ctx arg in
            unify param_type arg_type;
            t_arg)
          param_types
          args
      in
      (TEcall ({ fn_name = fname; fn_params = [] }, typed_args), ret_type)
    | _ -> error "Unbound function %s" fname)
  | Elist el ->
    let elem_type = fresh_type_var () in
    let typed_list =
      List.map
        (fun e ->
          let te, t = type_expr ctx e in
          unify elem_type t;
          te)
        el
    in
    (TElist typed_list, TList elem_type)
  | Eget (e1, e2) -> (
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in

    match t1 with
    | TList elem_type ->
      if t2 = TInt then (TEget (te1, te2), elem_type)
      else error "List index must be an integer"
    | _ -> error "Can only index into lists")

(* Type checking for statements *)
let rec type_stmt ctx stmt : tstmt * context =
  match stmt with
  (* Similar implementation with polymorphic typing *)
  | Sif (cond, s1, s2) ->
    let typ_cond, t = type_expr ctx cond in
    if t = TBool then
      let ts1, ctx1 = type_stmt ctx s1 in
      let ts2, ctx2 = type_stmt ctx s2 in
      (TSif (typ_cond, ts1, ts2), ctx)
    else error "Condition must be boolean"
  (* Other statement type checking remains similar *)
  | _ -> failwith "TODO: Complete polymorphic type checking"

(* Function definition type checking *)
let rec type_def ctx (id, idl, stmt) : tdef * context =
  let fname = id.id in
  if SMap.mem fname ctx.funcs then
    error ~loc:id.loc "Function %s already defined" fname;

  let params = List.map (fun p -> p.id) idl in
  let unique_params = List.sort_uniq String.compare params in
  if List.length params <> List.length unique_params then
    error "Function parameters must be unique";

  let ts, _ = type_stmt ctx stmt in
  let fn_params = List.map (fun _ -> fresh_type_var ()) idl in
  let fn = { fn_name = fname; fn_params = [] } in

  ( (fn, ts)
  , { vars = ctx.vars
    ; funcs = SMap.add fname (fn_params, fresh_type_var ()) ctx.funcs
    ; constraints = []
    } )

(* Top-level file type checking *)
let rec type_file ctx (dl, s) : tdef list =
  let ts, ctx' = type_stmt ctx s in
  let main = [ ({ fn_name = "main"; fn_params = [] }, ts) ] in

  let rec type_defs ctx defs =
    match defs with
    | [] -> ([], ctx)
    | def :: rest ->
      let t_def, ctx' = type_def ctx def in
      let t_rest, ctx'' = type_defs ctx' rest in
      (t_def :: t_rest, ctx'')
  in

  let tds, _ = type_defs ctx' dl in
  main @ tds

(* Main entry point *)
let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  type_file native_env p
