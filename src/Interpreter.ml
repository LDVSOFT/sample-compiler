module Expr =
  struct
    open Language.Expr

    let eval_op op l r =
      let toi x = if x then 1 else 0 in
      match op with
      | "+" -> l + r
      | "-" -> l - r
      | "*" -> l * r
      | "/" -> l / r
      | "%" -> l mod r
      | "&&" -> toi ((l != 0) && (r != 0))
      | "||" -> toi ((l != 0) || (r != 0))
      | "<"  -> toi (l < r)
      | ">"  -> toi (l > r)
      | "<=" -> toi (l <= r)
      | ">=" -> toi (l >= r)
      | "==" -> toi (l == r)
      | "!=" -> toi (l != r)
      | _ -> failwith "wrong op"

    let rec eval state expr =
      match expr with
      | Const n          -> n
      | Var   x          -> state x
      | Op (binop, l, r) -> eval_op binop (eval state l) (eval state r)
  end

module Stmt =
  struct
    open Language.Stmt

    let eval input stmt =
      let rec run' ((state, input, output) as c) stmt =
        let state' x = List.assoc x state in
        match stmt with
        | Skip                -> c
        | Seq    (l, r)       -> run' (run' c l) r
        | Assign (x, e)       -> ((x, Expr.eval state' e) :: state, input, output)
        | If     (cond, code) -> if Expr.eval state' cond != 0 then run' c code else c
        | While  (cond, code) -> if Expr.eval state' cond != 0 then run' c (Seq (code, stmt)) else c
        | Write  e            -> (state, input, output @ [Expr.eval state' e])
        | Read   x            ->
           let y::input' = input in
           ((x, y) :: state, input', output)
      in
      let (_, _, result) = run' ([], input, []) stmt in
      result
  end
