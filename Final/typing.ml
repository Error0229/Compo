open Ast

let debug = ref false

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string

type typ =
  | TNone
  | TBool
  | TString
  | TInt
  | TList
  | TAny
  | TFunc of typ list * typ (* parameters and return value's type*)

module SMap = Map.Make (String)

type context =
  { vars : typ SMap.t (* Variable environment *)
  ; funcs : (typ list * typ) SMap.t
        (* Function environment: param types and return type *)
  }

let native_env =
  { vars = SMap.empty
  ; funcs =
      SMap.of_list
        [ ("len", ([ TList ], TInt))
        ; ("range", ([ TInt ], TList))
        ; ("list", ([ TList ], TList))
        ]
  }

(* use the following function to signal typing errors, e.g. error ~loc "unbound
   variable %s" id *)
let error ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  failwith "TODO"

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
    | Some t -> (TEvar { v_name = id.id; v_ofs = 0 (* Placeholder *) }, t)
    | None -> error ~loc:id.loc "Unbound variable %s" id.id)
  | Ebinop (op, e1, e2) -> (
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in
    if t1 <> t2 then error "TypeError: you can only do binop to same type"
    else
      match op with
      | Badd ->
        if t1 = t2 then
          match t1 with
          | TString | TInt | TList -> (TEbinop (op, te1, te2), t1)
          | _ -> error "TypeError: unsupported operand type(s) for +"
        else error "TypeError: unsupported type to use + operand"
      | Bsub | Bmul | Bdiv | Bmod ->
        if t1 = TInt && t2 = TInt then (TEbinop (op, te1, te2), TInt)
        else error "You can only use - * / mod for int"
      | Beq | Bneq -> (TEbinop (op, te1, te2), TBool)
      | Blt | Ble | Bgt | Bge ->
        if t1 = TInt && t2 = TInt then (TEbinop (op, te1, te2), TBool)
        else error "can only do <= >= < > on int comparison"
      | Band | Bor ->
        if t1 = TBool && t2 = TBool then (TEbinop (op, te1, te2), TBool)
        else error "can not do 'and' and 'or' on not bool value")
  | Eunop (op, e) -> (
    let te, t = type_expr ctx e in
    match op with
    | Uneg ->
      if t = TInt then (TEunop (op, te), TInt)
      else error "connot use - as a negative operator for a value is not int"
    | Unot ->
      if t = TBool then (TEunop (op, te), TBool)
      else error "the 'not' operator can only apply on boolean variable")
  | Ecall (id, args) -> (
    let fname = id.id in
    match SMap.find_opt fname ctx.funcs with
    | Some (param_types, ret_type) ->
      if List.length param_types <> List.length args then
        error
          ~loc:id.loc
          "Function %s expects %d arguments but got %d"
          fname
          (List.length param_types)
          (List.length args);
      let typed_args =
        List.map2
          (fun param_type arg ->
            let t_arg, arg_type = type_expr ctx arg in
            if arg_type = param_type then t_arg
            else error "Type mismatch in function arguments")
          param_types
          args
      in
      (TEcall ({ fn_name = fname; fn_params = [] }, typed_args), ret_type)
    | _ -> error "Unbound function %s" fname)
  | Elist el ->
    let typed_list =
      List.map
        (fun e ->
          let te, t = type_expr ctx e in
          te)
        el
    in
    (TElist typed_list, TList)
    (* Should I check thier type? to force them have only type for a list*)
  | Eget (e1, e2) ->
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in
    if t1 = TList && t2 = TInt then (TEget (te1, te2), TAny)
    else
      error
        "The first argument is not a list, or the second argument is not an int"

(* Continue handling other cases *)
