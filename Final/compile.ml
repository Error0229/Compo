open Format
open X86_64
open Ast

let debug = ref false

type env =
  { locals : (string * int) list (* Variable name and offset from %rbp *)
  ; next_offset : int (* Next available offset (negative) *)
  }

(* let rec compile_texpr (expr : Ast.texpr) : X86_64.text = *)
(* let rec compile_tstmt (stmt : Ast.tstmt) : X86_64.text = *)
(* let compile_tdef ((fn, body) : Ast.tdef) : X86_64.text = *)
(* let compile_tfile (tdefs : Ast.tfile) : X86_64.program = *)

let file ?debug:(b = false) (p : Ast.tfile) : X86_64.program =
  debug := b;
  { text = globl "main" ++ label "main" ++ ret; (* TODO *)
                                                data = nop }
(* TODO *)
