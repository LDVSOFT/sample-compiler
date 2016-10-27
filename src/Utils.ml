let find (a: 'a array) (x: 'a): int =
  let len = Array.length a in
  let rec find' a x n =
    if a.(n) = x
      then n
      else if n == len then failwith "I failed to find!" else find' a x (n + 1)
  in find' a x 0

let default: 'a -> 'a option -> 'a = fun d v ->
  match v with
  | Some x -> x
  | None   -> d

let collect_args: ('a * 'a list) option -> 'a list = fun x ->
  match x with
  | None              -> []
  | Some (head, tail) -> head::tail
