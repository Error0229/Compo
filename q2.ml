let rec fibo n =
  (* precondition : n >= 0 *)
  if n <= 1 then n else fibo (n - 1) + fibo (n - 2)

let linear_fibo n =
  let f : int array = Array.make (n + 1) (-1) in
  let rec lin_fib n =
    if f.(n) <> -1 then f.(n)
    else
      let x = if n < 2 then 1 else lin_fib (n - 1) + lin_fib (n - 2) in
      f.(n) <- x;
      x
  in
  lin_fib n

let accum_fibo n =
  let rec f p1 p2 i = if i <> n then f p2 (p1 + p2) (i + 1) else p2 in
  f 1 1 1

let () =
  print_int (linear_fibo 10);
  print_newline ();
  print_int (accum_fibo 10)
