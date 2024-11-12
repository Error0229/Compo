
(* Lexical analyser for mini-Turtle *)

{
  open Lexing
  open Parser

  (* raise exception to report a lexical error *)
  exception Lexing_error of string

  (* note : remember to call the Lexing.new_line function
at each carriage return ('\n' character) *)

}
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let decimals = '.' digit*
let exponent = ['e' 'E'] ['+' '-']? digit+
let space = [' ' '\t' '\r']
let comment = "//" [^'\n']*

rule token = parse
  | '\n' {new_line lexbuf; token lexbuf}
  | (space|comment)+ { token lexbuf }
  | "(*" { comment lexbuf }
  | eof { EOF } 
and comment = parse 
  | "*)" { token lexbuf }
  | _ { comment lexbuf }
  | eof { failwith "non-closed comment"}
