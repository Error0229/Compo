let rec palindrome s : bool =
  match String.length s with
  | 0 | 1 -> true
  | len when String.get s 0 = String.get s (len - 1) ->
    palindrome (String.sub s 1 (len - 2))
  | _ -> false
