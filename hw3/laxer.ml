open A

let buffer_of_string s = { text = s; current = 0; last = -1 }

let rec tokenize b cs =
  if cs = String.length b.text then ignore ()
  else (
    b.last <- -1;
    try start b with
    | End_of_file -> print_endline "exception End_of_file"
    | Failure msg ->
      if b.last = -1 then print_endline msg
      else
        let token = String.sub b.text cs (b.last - cs) in
        print_endline ("--> \"" ^ token ^ "\"");
        b.current <- b.last;
        tokenize b b.current)
(* print_endline ("Lexer error: " ^ msg) *)

let () =
  let b = buffer_of_string "abbaaab" in
  tokenize b 0

let () =
  let b = buffer_of_string "aba" in
  tokenize b 0

let () =
  let b = buffer_of_string "aac" in
  tokenize b 0

let () =
  let b = buffer_of_string "abbac" in
  tokenize b 0
