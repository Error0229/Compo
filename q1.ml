let rec frac x = x * if x > 1 then frac (x - 1) else 1;;

print_int (frac 5);
print_newline ()

let rec frac_m x =
  match x with
  | 1 -> 1
  | _ -> x * frac_m (x - 1)
;;

print_int (frac_m 6);
print_newline ()

let rec nb_bit_pos x = if x > 0 then (x land 1) + nb_bit_pos (x lsr 1) else 0;;

print_int (nb_bit_pos 10)
