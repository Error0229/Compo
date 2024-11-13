
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
let ident = (letter | '_') (letter | digit | '_')*
let space = [' ' '\t' '\r']
let comment = "//" [^'\n']*
let integer = '0' | ['1'-'9'] digit *


rule token = parse
  | '\n' {new_line lexbuf; token lexbuf}
  | (space|comment)+ { token lexbuf }
  | "(*" { comment lexbuf }
  | integer as n { INT (int_of_string n)}
  | ident as id {
    match id with
    | "def" -> DEF
    | "penup" -> PU
    | "pendown" -> PD
    | "forward" -> FORWARD
    | "turnleft" -> TL
    | "turnright" -> TR
    | "color" -> CLR
    | "black" -> BLACK
    | "white" -> WHITE
    | "red" -> RED
    | "green" -> GREEN
    | "blue" -> BLUE
    | "if" -> IF
    | "else" -> ELSE
    | "repeat" -> REP
    | _ -> IDENT id
  }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LP }
  | ')' { RP }
  | '{' { LBR }
  | '}' { RBR }
  | ',' { COMMA }
  | eof { EOF } 
and comment = parse 
  | "*)" { token lexbuf }
  | _ { comment lexbuf }
  | eof { failwith "non-closed comment"}
