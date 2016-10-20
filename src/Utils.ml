let find: 'a array -> 'a -> int  = fun a x ->
  let rec find' a x n =
    if a.(n) = x
      then n
      else find' a x (n + 1)
  in find' a x 0

let default: 'a -> 'a option -> 'a = fun d v ->
  match v with
  | Some x -> x
  | None   -> d
