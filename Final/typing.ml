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
  | TRange

module SMap = Map.Make (String)

type context =
  { vars : typ SMap.t (* Variable environment *)
  ; funcs : (typ list * typ) SMap.t
        (* Function environment: param types and return type *)
  }

let native_ctx =
  { vars = SMap.empty
  ; funcs =
      SMap.of_list
        [ ("len", ([ TAny ], TInt))
        ; ("range", ([ TAny ], TRange))
        ; ("list", ([ TRange ], TList))
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
    | None -> error ~loc:id.loc "Unbounded identifier %s" id.id)
  | Ebinop (op, e1, e2) -> (
    if op = Band || op = Bor then (TEbinop (op, TEcst Cnone, TEcst Cnone), TBool)
    else
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
        | _, _ ->
          (TEbinop (op, te1, te2), TAny)
          (* since the binop is runtime evaluated*)
          (* | _, _ -> error "TypeError: unsupported operand types for '+'") *))
      | Beq | Bneq ->
        (* Equality checks can be between any types *)
        (TEbinop (op, te1, te2), TBool)
      | Blt | Ble | Bgt | Bge -> (
        match (t1, t2) with
        | TInt, TInt
        | TBool, TBool
        | TBool, TInt
        | TInt, TBool
        | TString, TString
        | TList, TList
        | TAny, _
        | _, TAny -> (TEbinop (op, te1, te2), TBool)
        | _, _ -> error "TypeError: unsupported operand types for comparison")
      | Band | Bor -> (TEbinop (op, te1, te2), TBool))
  | Eunop (op, e) -> (
    let te, t = type_expr ctx e in
    match op with
    | Uneg -> (
      match t with
      | TInt -> (TEunop (op, te), TInt)
      | TAny -> (TEunop (op, te), TAny)
      | _ -> error "TypeError: unary '-' requires an integer operand")
    | Unot -> (TEunop (op, te), TBool))
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
              else error ~loc:id.loc "Type mismatch in function arguments")
          param_types
          args
      in
      (* List.map (fun a -> let t_arg, arg_type = type_expr ctx a in t_arg) args
         in *)
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
    let ts1, ctx1 = type_stmt ctx s1 in
    let ts2, ctx2 = type_stmt ctx1 s2 in
    (* both ctx1, ctx2 are local context right? *)
    (TSif (typ_cond, ts1, ts2), ctx2)
  | Sreturn e ->
    let typ_e, _ = type_expr ctx e in
    (TSreturn typ_e, ctx)
  | Sassign (id, e) ->
    let v_name = id.id in
    let tye, t = type_expr ctx e in
    let ctx' = { ctx with vars = SMap.add v_name t ctx.vars } in
    (TSassign ({ v_name; v_ofs = 0 }, tye), ctx')
  | Sprint e -> (
    match type_expr ctx e with
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
      type_stmt { vars = SMap.add it.id TAny ctx.vars; funcs = ctx.funcs } s
    in
    (* if t = TList || t = TAny then *)
    (TSfor ({ v_name = it.id; v_ofs = 0 }, tye, tys), ctx')
    (* else error "can only iterate list" *)
  | Seval e ->
    let tye, _ = type_expr ctx e in
    (TSeval tye, ctx)
  | Sset (e1, e2, e3) ->
    let te1, t1 = type_expr ctx e1 in
    if t1 <> TList && t1 <> TAny then error "the variable must be a list"
    else
      let te2, t2 = type_expr ctx e2 in
      if t2 <> TInt && t2 <> TAny then
        error "the index of an array must be an int"
      else
        let te3, t3 = type_expr ctx e3 in
        (TSset (te1, te2, te3), ctx)

let type_def ctx (id, params, body) : tdef * context =
  let fname = id.id in
  if SMap.mem fname ctx.funcs then
    error ~loc:id.loc "Function %s is already defined" fname;
  if List.mem fname [ "len"; "range"; "list" ] then
    error ~loc:id.loc "Function %s cannot shadow a built-in function" fname;
  List.iter
    (fun p ->
      if List.length (List.filter (fun p' -> p'.id = p.id) params) > 1 then
        error ~loc:p.loc "Function parameter %s is duplicated" p.id)
    params;
  (* Assign TAny to all parameters *)
  let param_types = List.map (fun _ -> TAny) params in
  let func_type = (param_types, TAny) in
  let ctx = { ctx with funcs = SMap.add fname func_type ctx.funcs } in
  let env_body =
    { vars =
        List.fold_left2
          (fun vars param param_type -> SMap.add param.id param_type vars)
          ctx.vars
          params
          param_types
    ; funcs = ctx.funcs
    }
  in
  let t_body, _ = type_stmt env_body body in
  let fn_params = List.map (fun id -> { v_name = id.id; v_ofs = 0 }) params in
  let fn = { fn_name = fname; fn_params } in
  ((fn, t_body), ctx)

let rec type_file ctx (dl, s) : tdef list =
  let rec type_defs ctx defs =
    match defs with
    | [] -> ([], ctx)
    | def :: rest ->
      let t_def, ctx' = type_def ctx def in
      let t_rest, ctx'' = type_defs ctx' rest in
      (t_def :: t_rest, ctx'')
  in
  let tds, ctx' = type_defs ctx dl in
  let ts, _ = type_stmt ctx' s in
  let main = [ ({ fn_name = "main"; fn_params = [] }, ts) ] in
  main @ tds

let file ?debug:(b = false) (p : Ast.file) : Ast.tfile =
  debug := b;
  type_file native_ctx p
