type io_state_t = {input: int list; output: int list}
type expr_state_t = {io: io_state_t; vars: (string * int) list}
type stmt_state_t = {expr: expr_state_t; result: int option}
type invoke_t = string -> int list -> io_state_t -> (int * io_state_t)

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
      | "!!" -> toi ((l != 0) || (r != 0))
      | "<"  -> toi (l < r)
      | ">"  -> toi (l > r)
      | "<=" -> toi (l <= r)
      | ">=" -> toi (l >= r)
      | "==" -> toi (l =  r)
      | "!=" -> toi (l <> r)
      | _ -> failwith "wrong op"

    let rec eval (state: expr_state_t) (invoke: invoke_t) (expr: t): (int * expr_state_t) =
      let rec eval_args (args: t list) (state: expr_state_t): (int list * expr_state_t) =
        match args with
        | []    -> ([], state)
        | x::xs ->
          let (y , state' ) = eval state invoke x in
          let (ys, state'') = eval_args xs state' in
          (y::ys, state'')
      in
      let get x = List.assoc x state.vars in
      match expr with
      | Const n          -> (n, state)
      | Var   x          -> (get x, state)
      | Op (binop, l, r) ->
        let (lr, state' ) = eval state  invoke l in
        let (rr, state'') = eval state' invoke r in
        (eval_op binop lr rr, state'')
      | Call (f, args)   ->
        let (args', state') = eval_args args state in
        let (res, io'') = invoke f args' state'.io in
        (res, {state' with io = io''})
  end

module Stmt =
  struct
    open Language.Expr
    open Language.Stmt

    let rec eval (state: stmt_state_t) (invoke: invoke_t) (stmt: t): stmt_state_t =
      match state.result with
      | Some _ -> state
      | None   -> match stmt with
        | Skip                  -> state
        | Seq    (l, r)         ->
          let state'  = eval state  invoke l in
          let state'' = eval state' invoke r in
          state''
        | Read   x              ->
          let y::input' = state.expr.io.input in
          {state with expr = {vars = (x, y)::state.expr.vars; io = {state.expr.io with input = input'}}}
        | Write  expr           ->
          let (v, expr') = Expr.eval state.expr invoke expr in
          {state with expr = {expr' with io = {expr'.io with output = expr'.io.output @ [v]}}}
        | Assign (x, expr)      ->
          let (v, expr') = Expr.eval state.expr invoke expr in
          {state with expr = {expr' with vars = (x, v)::expr'.vars}}
        | If     (cond, b1, b2) ->
          let (v, expr') = Expr.eval state.expr invoke cond in
          let state' = {state with expr = expr'} in
          if v != 0
            then eval state' invoke b1
            else eval state' invoke b2
        | While  (cond, code)   ->
          let (v, expr') = Expr.eval state.expr invoke cond in
          let state' = {state with expr = expr'} in
          if v != 0
            then eval state' invoke @@ Seq (code, stmt)
            else state'
        | Repeat (cond, code)   ->
          eval state invoke @@ Seq (code, While (Op ("==", cond, Const 0), code))
        | Proc (f, ps)          ->
          let (_, expr') = Expr.eval state.expr invoke @@ Call (f, ps) in
          {state with expr = expr'}
        | Return expr           ->
          let (v, expr') = Expr.eval state.expr invoke expr in
          {expr = expr'; result = Some v}
  end

module Program =
  struct
    open Language.Program

    let eval (input: int list) (program: t) =
      let rec invoke': bool -> invoke_t = fun must_ret name args io ->
        let func = List.assoc name program.funcs in
        let state = {expr = {vars = List.map2 (fun a b -> (a, b)) func.args args; io = io}; result = None} in
        let state' = Stmt.eval state (invoke' true) func.body in
        let res = match state'.result with
        | Some x                 -> x
        | None when not must_ret -> 0
        | _                      -> failwith "Function has not returned a value"
        in
        (res, state'.expr.io)
      in
      let (_, io') = invoke' false "main" [] {input = input; output = []} in
      io'.output
  end
