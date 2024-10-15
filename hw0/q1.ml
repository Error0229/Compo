let rec frac x = x * if x > 1 then frac (x - 1) else 1;;

print_int (frac 5);
print_newline ()

let rec frac_m x =
  match x with
  | 1 -> 1
  | _ -> x * frac_m (x - 1)

let frac_tr x =
  let rec aux res i = if i > 1 then aux (res * i) (i - 1) else res in
  aux 1 x

let () =
  print_int (frac_tr 6);
  print_newline ()

let rec nb_bit_pos x = if x > 0 then (x land 1) + nb_bit_pos (x lsr 1) else 0

let rec nb_bit_pos_tr x =
  let rec aux res x = if x > 0 then aux (res + (x land 1)) (x lsr 1) else res in
  aux 0 x

let () =
  print_int (nb_bit_pos 10);
  print_newline ()

let () =
  print_int (nb_bit_pos_tr 10);
  print_newline ()
