open Format
open X86_64
open Ast

let debug = ref false

type env =
  { locals : (string * int) list (* Variable name and offset from %rbp *)
  ; next_offset : int (* Next available offset (negative) *)
  }

(* let setup_parameters (params : Ast.var list) : env * X86_64.text = *)
(* let rec compile_texpr (ctx : env) (expr : Ast.texpr) : X86_64.text = *)
(* let rec compile_tstmt (ctx : env) (stmt : Ast.tstmt) : X86_64.text = *)
(* let compile_tdef (ctx : env) ((fn, body) : Ast.tdef) : X86_64.text = *)
(* let compile_tfile (ctx : env) (tdefs : Ast.tfile) : X86_64.program = *)
let file ?debug:(b = false) (p : Ast.tfile) : X86_64.program =
  debug := b;
  { text = globl "main" ++ label "main" ++ ret; (* TODO *)
                                                data = nop }
(* TODO *)
