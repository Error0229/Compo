open Format
open X86_64
open Ast
open Util

let debug = ref false

type env =
  { locals : (string * int) list (* Variable name and offset from %rbp *)
  ; next_offset : int (* Next available offset (negative) *)
  ; data : (string * label) list (* List of data items and their labels *)
  ; functions : (string * string) list (* Function names and their labels *)
  ; current_function : string (* Name of the current function *)
  }
  let rec compile_tstmt env (stmt : Ast.tstmt) : X86_64.text * env =
  match stmt with
  | TSassign (var, expr) ->
      (* Compile the expression *)
      let expr_code= compile_texpr env expr in
      (* Store the result into the variable's location *)
      (* Update the environment with the variable offset *)
      let offset, env = allocate_variable env var.v_name in
      let code =
        expr_code ++
        movq !%rax (ind ~ofs:offset rbp)
      in
      (code, env)
  | TSreturn expr ->
      let expr_code= compile_texpr env expr in
      let code =
        expr_code ++
        jmp ("end_" ^ env.current_function)
      in
      (code, env)
   | TSif (cond_expr, then_stmt, else_stmt) ->
    let cond_code = compile_texpr env cond_expr in
    let then_code, _ = compile_tstmt env then_stmt in
    let else_code, _ = compile_tstmt env else_stmt in
    (* Generate labels *)
    let label_else = Util.genid "else"in
    let label_end = Util.genid "endif" in
    (cond_code ++
    (* Evaluate condition and jump to else if false *)
    (* Assuming condition result in %rax *)
    cmpq (imm 0) !%rax ++
    je label_else ++
    (* Then branch *)
    then_code ++
    jmp label_end ++
    (* Else branch *)
    label label_else ++
    else_code ++
    label label_end, env)
  | TSblock stmts ->
    (* Compile each statement in the block *)
( 
    List.fold_left (fun acc s ->
      let  cstmt, _ = compile_tstmt env s in
       acc ++ cstmt) nop stmts
 , env)
  | TSprint expr ->
    (* Compile the expression *)
    let expr_code = compile_texpr env expr in
    (* Generate code to print the value *)
    expr_code, env
    (* Placeholder: Need to implement print logic *)
  | _ ->
    failwith "Statement not yet implemented" 
  (* ... Handle other cases ... *)

and compile_texpr env (expr : Ast.texpr) : X86_64.text =
  match expr with
  | TEcst cst ->
      let code = compile_constant cst in
      (code)
  | TEvar var ->
      let offset = find_variable env var.v_name in
      let code = movq (ind ~ofs:offset rbp) !%rax in
      (code)
  | _ -> failwith "TODO"
  (* ... Handle other cases ... *)
  and compile_constant (cst : Ast.constant) : X86_64.text =
  match cst with
  | Cint n ->
    (* Allocate a heap block for the integer value *)
    (* Type tag: 2, Value: n *)
    let code =
      movq (imm 16) !%rdi ++      (* Size of the block *)
      call "my_malloc" ++       (* %rax has pointer to new block *)
      movq (imm 2) (ind rax) ++ (* Type tag at offset 0 *)
      movq (imm64 n) (ind ~ofs:8 rax)  (* Value at offset 8 *)
    in
    code
  | Cbool b ->
    (* Similar to integer, with type tag 1 and value 0 or 1 *)
    let value = if b then 1 else 0 in
    let code =
      movq (imm 16) !%rdi ++
      call "my_malloc" ++
      movq (imm 1) (ind rax) ++  (* Type tag 1 *)
      movq (imm value) (ind ~ofs:8 rax)
    in
    code
  | Cstring s ->
    (* Allocate block with type tag 3 and store the string *)
    let len = String.length s in
    let total_size = 16 + len + 1 in  (* Type tag + length + string + null terminator *)
    let code =
      movq (imm total_size) !%rdi ++
      call "my_malloc" ++
      movq (imm 3) (ind rax) ++           (* Type tag 3 *)
      movq (imm len) (ind ~ofs:8 rax) ++  (* Length *)
      (* Copy the string to memory *)
      (* Implement string copying, possibly by calling strcpy *)
      nop  (* Placeholder *)
    in
    code
  | Cnone ->
    (* None value, type tag 0 *)
    let code =
      movq (imm 8) !%rdi ++
      call "my_malloc" ++
      movq (imm 0) (ind rax)  (* Type tag 0 *)
    in
    code


and compile_tdef env ((fn, body) : Ast.tdef) : X86_64.text * env =
  let fn_label = fn.fn_name in
  let env = { env with current_function = fn_label } in
  (* Set up initial environment for the function *)
  let env, param_setup = setup_parameters env fn.fn_params in
  let prologue = 
    label fn_label ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    param_setup
  in
  (* Compile the function body *)
  let body_code, env = compile_tstmt env body in
  (* Epilogue label for returns *)
  let epilogue_label = "end_" ^ fn_label in
  let epilogue = 
    label epilogue_label ++
    popq rbp ++
    ret
  in
  (prologue ++ body_code ++ epilogue, env)

  (* Function to generate the data section from env.data *)
and generate_data_section (data_items : (string * label) list) : X86_64.data =
  List.fold_left (fun acc (s, lbl) ->
    acc ++
    label lbl ++
    string s
  ) nop data_items

and compile_tfile (tdefs : Ast.tfile) : X86_64.program =
  let text_sections = ref [] in
  let data_items = ref [] in
  let functions = List.map (fun (fn, _) -> (fn.fn_name, fn.fn_name)) tdefs in
  let env = {
    locals = [];
    next_offset = 0;
    data = [];
    functions = functions;
    current_function = "";
  } in

  (* Compile each function *)
  List.iter (fun tdef ->
    let fn_code, env = compile_tdef env tdef in
    text_sections := !text_sections @ [fn_code];
    data_items := env.data @ !data_items;
  ) tdefs;

  (* Remove duplicates *)
  let unique_data_items = List.sort_uniq (fun (_, l1) (_, l2) -> String.compare l1 l2) !data_items in
  (* Generate data section *)
  let data_section = generate_data_section unique_data_items in

  {
    text = List.fold_left (++) nop !text_sections;
    data = data_section;
  }


  (* ... rest remains the same ... *)
and compile_function_call env (fn : Ast.fn) (args : Ast.texpr list) : X86_64.text * env =
  (* Evaluate arguments in reverse order and push onto stack *)
  let arg_code, env = List.fold_right (fun arg (code, env) ->
    let arg_code = compile_texpr env arg in
    (code ++ arg_code ++ pushq !%rax, env)
  ) args (nop, env) in
  (* Find the function label *)
  let fn_label =
    try List.assoc fn.fn_name env.functions
    with Not_found -> failwith ("Function not found: " ^ fn.fn_name)
  in
  (* Call the function *)
  let code =
    arg_code ++
    call fn_label ++
    (* Adjust the stack pointer if needed *)
    addq (imm (8 * List.length args)) !%rsp
  in
  (code, env)

and allocate_variable env var_name =
  (* Check if variable already exists *)
  if List.exists (fun (name, _) -> name = var_name) env.locals then
    let offset = List.assoc var_name env.locals in
    (offset, env)
  else
    let offset = env.next_offset - 8 in
    let env = { env with
      locals = (var_name, offset) :: env.locals;
      next_offset = offset;
    } in
    (offset, env)

  and find_variable env var_name =
  try List.assoc var_name env.locals
  with Not_found -> failwith ("Variable not found: " ^ var_name)
and 
 setup_parameters env (params : Ast.var list) : env * X86_64.text =
  let env = { env with locals = []; next_offset = -8;} in  (* Start at -8(%rbp) *)
  let code, env = List.fold_left (fun (code, env) param ->
    let offset = env.next_offset in
    let var_name = param.v_name in
    (* Copy parameter from stack to local variable slot *)
    let param_offset = (* Calculate offset based on call convention *)
      16 + ((List.length params - 1) * 8)  (* Adjust as needed *)
    in
    let code = 
      code ++
      movq (ind ~ofs:param_offset rbp) (ind ~ofs:offset rbp)
    in
    let env = { env with locals = (var_name, offset) :: env.locals; next_offset = offset - 8 } in
    (code, env)
  ) (nop, env) params in
  (env, code)



(* let setup_parameters (params : Ast.var list) : env * X86_64.text = *)
(* let rec compile_texpr (ctx : env) (expr : Ast.texpr) : X86_64.text = *)
(* let rec compile_tstmt (ctx : env) (stmt : Ast.tstmt) : X86_64.text = *)
(* let compile_tdef (ctx : env) ((fn, body) : Ast.tdef) : X86_64.text = *)
(* let compile_tfile (ctx : env) (tdefs : Ast.tfile) : X86_64.program = *)
and file ?debug:(b = false) (p : Ast.tfile) : X86_64.program =
  debug := b;
  { text = globl "main" ++ label "main" ++ ret; (* TODO *)
                                                data = nop }
(* TODO *)
