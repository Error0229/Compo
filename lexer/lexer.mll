{
type token =
    | Tident of string
    | Tconst of int
    | Tfun
    | Tarrow
    | Tplus
    | Teof
}
rule token = parse
    | [' ' '\t' '\n']+ { token lexbuf }
    | "(*" { comment lexbuf }
    | "fun" { Tfun }
    | ['a'-'z']+ as s { Tident s }
    | ['0'-'9']+ as s { Tconst (int_of_string s) }
    | "+" { Tplus }
    | "->" { Tarrow }
    | _ as c { failwith ("illegal character : " ^ String.make 1 c) }
    | eof { Teof }
and comment = parse
    | "*)" { token lexbuf }
    | _ { comment lexbuf }
    | eof { failwith "non-closed comment" }
