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
    | None ->
      error "Unbounded identifier %s" id.id
      (* If variables must be declared before use, you might raise an error *)
      (* Alternatively, you can assign TAny to undeclared variables *)
      (* let t = TAny in let ctx = { ctx with vars = SMap.add id.id t ctx.vars }
         in (TEvar { v_name = id.id; v_ofs = 0 }, t) *))
  | Ebinop (op, e1, e2) -> (
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in
    match op with
    | Bsub | Bmul | Bdiv | Bmod -> (
      match (t1, t2) with
      | TInt, TInt -> (TEbinop (op, te1, te2), TInt)
      | TAny, _ | _, TAny -> (TEbinop (op, te1, te2), TAny)
      | _, _ ->
        error "TypeError: unsupported operand types for arithmetic operation")
    | Badd -> (
      (* Handle overloaded '+' operator *)
      match (t1, t2) with
      | TString, TString -> (TEbinop (op, te1, te2), TString)
      | TList, TList -> (TEbinop (op, te1, te2), TList)
      | TInt, TInt -> (TEbinop (op, te1, te2), TInt)
      | TAny, _ | _, TAny -> (TEbinop (op, te1, te2), TAny)
      | _, _ -> error "TypeError: unsupported operand types for '+'")
    | Beq | Bneq ->
      (* Equality checks can be between any types *)
      (TEbinop (op, te1, te2), TBool)
    | Blt | Ble | Bgt | Bge -> (
      match (t1, t2) with
      | TInt, TInt -> (TEbinop (op, te1, te2), TBool)
      | TAny, _ | _, TAny -> (TEbinop (op, te1, te2), TBool)
      | _, _ -> error "TypeError: unsupported operand types for comparison")
    | Band | Bor -> (
      match (t1, t2) with
      | TBool, TBool -> (TEbinop (op, te1, te2), TBool)
      | TAny, _ | _, TAny -> (TEbinop (op, te1, te2), TBool)
      | _, _ -> error "TypeError: logical operators require boolean operands"))
  | Eunop (op, e) -> (
    let te, t = type_expr ctx e in
    match op with
    | Uneg -> (
      match t with
      | TInt -> (TEunop (op, te), TInt)
      | TAny -> (TEunop (op, te), TAny)
      | _ -> error "TypeError: unary '-' requires an integer operand")
    | Unot -> (
      match t with
      | TBool -> (TEunop (op, te), TBool)
      | TAny -> (TEunop (op, te), TBool)
      | _ -> error "TypeError: 'not' requires a boolean operand"))
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
            match param_type with
            | TAny -> t_arg (* Accept any type *)
            | _ ->
              if arg_type = param_type || arg_type = TAny then t_arg
              else error "Type mismatch in function arguments")
          param_types
          args
      in
      (TEcall ({ fn_name = fname; fn_params = [] }, typed_args), ret_type)
    | None -> error ~loc:id.loc "Unbound function %s" fname)
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
  | Eget (e1, e2) -> (
    let te1, t1 = type_expr ctx e1 in
    let te2, t2 = type_expr ctx e2 in
    match (t1, t2) with
    | TList, TInt -> (TEget (te1, te2), TAny) (* Result can be any type *)
    | TList, TAny | TAny, TInt | TAny, TAny -> (TEget (te1, te2), TAny)
    | _, _ -> error "TypeError: invalid types for list indexing")

let rec type_stmt ctx stmt : tstmt * context =
  match stmt with
  | Sif (cond, s1, s2) ->
    let typ_cond, t = type_expr ctx cond in
    if t = TBool then
      let ts1, ctx1 = type_stmt ctx s1 in
      let ts2, ctx2 = type_stmt ctx s2 in
      (* both ctx1, ctx2 are local context right? *)
      (TSif (typ_cond, ts1, ts2), ctx)
    else error "the condition did not return a bool"
  | Sreturn e ->
    let typ_e, _ = type_expr ctx e in
    (TSreturn typ_e, ctx)
  | Sassign (id, e) -> (
    let v_name = id.id in
    let tye, t = type_expr ctx e in
    match SMap.find_opt v_name ctx.vars with
    | Some vt ->
      if t = vt then (TSassign ({ v_name; v_ofs = 0 }, tye), ctx)
      else error "the variable type does not match with the value"
    | None ->
      let new_ctx =
        { vars = SMap.add v_name t ctx.vars
        ; funcs =
            (if SMap.mem v_name ctx.funcs then SMap.remove v_name ctx.funcs
             else ctx.funcs)
        }
      in
      (TSassign ({ v_name; v_ofs = 0 }, tye), new_ctx))
  | Sprint e -> (
    match type_expr ctx e with
    | _, TAny | _, TFunc _ -> error "Func and Any type cannot be print"
    | tye, _ -> (TSprint tye, ctx))
  | Sblock sl ->
    let rec type_stmts ctx stmts =
      match stmts with
      | [] -> ([], ctx)
      | stmt :: rest ->
        let t_stmt, ctx' = type_stmt ctx stmt in
        let t_rest, ctx'' = type_stmts ctx' rest in
        (t_stmt :: t_rest, ctx'')
    in
    let t_stmts, ctx' = type_stmts ctx sl in
    (TSblock t_stmts, ctx')
  | Sfor (it, lst, s) ->
    let tye, t = type_expr ctx lst in
    let tys, ctx' =
      type_stmt { vars = SMap.add it.id t ctx.vars; funcs = ctx.funcs } s
    in
    if t = TList then (TSfor ({ v_name = it.id; v_ofs = 0 }, tye, tys), ctx')
    else error "can only iterate list"
  | Seval e ->
    let tye, _ = type_expr ctx e in
    (TSeval tye, ctx)
  | Sset (e1, e2, e3) ->
    let te1, t1 = type_expr ctx e1 in
    if t1 <> TList then error "the variable must be a list"
    else
      let te2, t2 = type_expr ctx e2 in
      if t2 <> TInt then error "the index of an array must be an int"
      else
        let te3, t3 = type_expr ctx e3 in
        (TSset (te1, te2, te3), ctx)

let rec type_def ctx (id, idl, stmt) : tdef * context =
  let fname = id.id in
  if SMap.mem fname ctx.funcs then
    error ~loc:id.loc "Function %s is already defined" fname;
  if List.mem fname [ "range"; "len"; "list" ] then
    error ~loc:id.loc "Function %s cannot shadow a builtin function" fname;
  let params = List.map (fun p -> p.id) idl in
  let unique_params = List.sort_uniq String.compare params in
  if List.length params <> List.length unique_params then
    error "Function paramteres must be unique";
  let ts, _ = type_stmt ctx stmt in
  let fn_params = List.map (fun id -> { v_name = id.id; v_ofs = 0 }) idl in
  let fn = { fn_name = fname; fn_params } in
  ( (fn, ts)
  , { vars = ctx.vars
    ; funcs = SMap.add fname (List.map (fun _ -> TAny) idl, TAny) ctx.funcs
    } )

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

let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  type_file { vars = SMap.empty; funcs = SMap.empty } p
