let counter = ref 0

let genid s =
  let id = !counter in
  if id = -1 then failwith "Counter reached max value.";
  incr counter;
  s ^ "_" ^ string_of_int id
