open Utils

module Value =
  struct
    include Language.Value

    let toi (x: t): int =
      match x with
      | Int v -> v
      | _ -> failwith "Not an int"

    let tos (x: t): bytes =
      match x with
      | String v -> v
      | _        -> failwith "Not a string"
  end

type expr_state_t = {vars: (string * Value.t) list}
type stmt_state_t = {expr: expr_state_t; result: Value.t option}
type invoke_t = string -> Value.t list -> Value.t

module Builtins =
  struct
    open Value

    type t = {
      args: int;
      invoke: Value.t list -> Value.t
    }

    let read: t = {
      args = 0;
      invoke = fun [] ->
        let () = Printf.printf "> " in
        Int (read_int ())
    }

    let write: t = {
      args = 1;
      invoke = fun [Int n] ->
        let () = Printf.printf "%d\n" n in
        Int 0
    }

    let strmake: t = {
      args = 2;
      invoke = fun [Int n; Int c] -> String (Bytes.make n @@ Char.chr c)
    }

    let strset: t = {
      args = 3;
      invoke = fun [(String s) as r; Int i; Int c] -> Bytes.set s i @@ Char.chr c; r
    }

    let strget: t = {
      args = 2;
      invoke = fun [String s; Int i] -> Int (Char.code @@ Bytes.get s i)
    }

    let strdup: t = {
      args = 1;
      invoke = fun [String s] -> String (Bytes.copy s)
    }

    let strcat: t = {
      args = 2;
      invoke = fun [String s1; String s2] -> String (Bytes.concat Bytes.empty [s1; s2])
    }

    let strcmp: t = {
      args = 2;
      invoke = fun [String s1; String s2] -> Int (Bytes.compare s1 s2)
    }

    let strlen: t = {
      args = 1;
      invoke = fun [String s] -> Int (Bytes.length s)
    }

    let strsub: t = {
      args = 3;
      invoke = fun [String s; Int i; Int l] -> String (Bytes.sub s i l)
    }

    let arrmake: t = {
      args = 2;
      invoke = fun [Int n; v] -> Array (Array.make n v)
    }

    let arrset: t = {
      args = 3;
      invoke = fun [Array a; Int n; v] ->
        Array.set a n v;
        Array a
    }

    let arrget: t = {
      args = 2;
      invoke = fun [Array a; Int n] -> Array.get a n
    }

    let arrlen: t = {
      args = 1;
      invoke = fun [Array a] -> Int (Array.length a)
    }

    let builtins: (string * t) list = [
      ("read" , read );
      ("write", write);
      ("strmake", strmake);
      ("strset" , strset );
      ("strget" , strget );
      ("strdup" , strdup );
      ("strcat" , strcat );
      ("strcmp" , strcmp );
      ("strlen" , strlen );
      ("strsub" , strsub );
      ("arrmake", arrmake);
      ("Arrmake", arrmake);
      ("arrlen" , arrlen );

      ("__arrget", arrget);
      ("__arrset", arrset);
    ]

    let get (func: string): t = List.assoc func builtins
    let invoke (func: string): (Value.t list -> Value.t) =
      (get func).invoke
    let names = List.map (fun (name, _) -> name) builtins
  end

module Expr =
  struct
    include Language.Expr

    let eval_op (op: string) (l': Value.t) (r': Value.t): Value.t =
      let toi x = if x then 1 else 0 in
      let l = Value.toi l' and r = Value.toi r' in
      let r = match op with
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
      in Value.Int r

    let rec eval (state: expr_state_t) (invoke: invoke_t) (expr: t): (Value.t * expr_state_t) =
      let rec eval_args (args: t list) (state: expr_state_t): (Value.t list * expr_state_t) =
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
        let res = invoke f args' in
        (res, state')
  end

module Stmt =
  struct
    open Value
    open Expr
    include Language.Stmt

    let rec eval (state: stmt_state_t) (invoke: invoke_t) (stmt: t): stmt_state_t =
      match state.result with
      | Some _ -> state
      | None   -> match stmt with
        | Skip                  -> state
        | Seq    (l, r)         ->
          let state'  = eval state  invoke l in
          let state'' = eval state' invoke r in
          state''
        | Assign (x, expr)      ->
          let (v, expr') = Expr.eval state.expr invoke expr in
          {state with expr = {vars = (x, v)::expr'.vars}}
        | If     (cond, b1, b2) ->
          let (v, expr') = Expr.eval state.expr invoke cond in
          let state' = {state with expr = expr'} in
          if Value.toi v != 0
            then eval state' invoke b1
            else eval state' invoke b2
        | While  (cond, code)   ->
          let (v, expr') = Expr.eval state.expr invoke cond in
          let state' = {state with expr = expr'} in
          if Value.toi v != 0
            then eval state' invoke @@ Seq (code, stmt)
            else state'
        | Repeat (cond, code)   ->
          eval state invoke @@ Seq (code, While (Op ("==", cond, Const (Int 0)), code))
        | Proc (f, ps)          ->
          let (_, expr') = Expr.eval state.expr invoke @@ Call (f, ps) in
          {state with expr = expr'}
        | Return expr           ->
          let (v, expr') = Expr.eval state.expr invoke expr in
          {expr = expr'; result = Some v}
  end

module Program =
  struct
    open Value
    open Expr
    open Stmt
    include Language.Program

    let eval (program: t) =
      (*print program;*)
      let rec invoke': bool -> invoke_t = fun must_ret name args ->
        try
          let func = List.assoc name program.funcs in
          let state = {expr = {vars = List.map2 (fun a b -> (a, b)) func.args args}; result = None} in
          let state' = Stmt.eval state (invoke' true) func.body in
          let res = match state'.result with
          | Some x                 -> x
          | None when not must_ret -> Int 0
          | _                      -> failwith "Function has not returned any value"
          in res
        with Not_found ->
          Builtins.invoke name args
      in
      let _ = invoke' false "main" [] in
      ()
  end
