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

let () = print_int (linear_fibo 10)