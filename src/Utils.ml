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

let cut_head (l: 'a list): ('a * 'a list) =
  match l with
  | x::xs -> (x, xs)
  | _ -> failwith "List depleted"

let cut_tail (l: 'a list): ('a * 'a list) =
  let (x, xs) = cut_head (List.rev l) in
  (x, List.rev xs)

let list_cut (l: 'a list) (n: int): ('a list * 'a list) =
  let rec f src dst n =
    if n = 0
    then (dst, src)
    else
      let (x, xs) = cut_head src in
      f xs (dst @ [x]) (n - 1)
  in f l [] n

let rec m_map (f: 'a -> 'c -> ('b * 'c)) (l: 'a list) (init: 'c): ('b list * 'c) =
  match l with
  | [] -> ([], init)
  | x::xs ->
    let (y, init') = f x init in
    let (ys, init'') = m_map f xs init' in
    (y::ys, init'')
