type i =
| S_LABEL of string
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_OP    of string
| S_JMP   of string
| S_JIF   of string

module Interpreter =
  struct
    open Interpreter
    open Utils

    let run: int list -> i list -> int list = fun input code' ->
      let code = Array.of_list code' in
      let rec srun' ((state, stack, input, output, i) as c) =
        if i >= Array.length code then c
        else srun' (
          match code.(i) with
          | S_READ ->
              let y::input' = input in
              (state, y::stack, input', output, i + 1)
          | S_WRITE ->
              let y::stack' = stack in
              (state, stack', input, output @ [y], i + 1)
          | S_PUSH n ->
              (state, n::stack, input, output, i + 1)
          | S_LD x ->
              (state, (List.assoc x state)::stack, input, output, i + 1)
          | S_ST x ->
              let y::stack' = stack in
              ((x, y)::state, stack', input, output, i + 1)
          | S_OP op ->
              let y::x::stack' = stack in
              (state, (Expr.eval_op op x y)::stack', input, output, i + 1)
          | S_LABEL _ ->
              (state, stack, input, output, i + 1)
          | S_JMP label ->
              let where = find code @@ S_LABEL label in
              (state, stack, input, output, where)
          | S_JIF label ->
              let y::stack' = stack in
              let where = find code @@ S_LABEL label in
              if y == 0 then (state, stack', input, output, i + 1)
              else (state, stack', input, output, where)
         )
      in
      let (_, _, _, output, _) = srun' ([], [], input, [], 0)
      in output
  end

module Compile =
  struct
    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var x         -> [S_LD   x]
    | Const n       -> [S_PUSH n]
    | Op (op, l, r) -> expr l @ expr r @ [S_OP op]

    let stmt s =
      let rec compile_stmt' s i =
        match s with
        | Skip               -> ([], i)
        | Assign (x, e)      -> (expr e @ [S_ST x], i)
        | Read x             -> ([S_READ; S_ST x], i)
        | Write e            -> (expr e @ [S_WRITE], i)
        | Seq (l, r)         ->
          let (res' , i' ) = compile_stmt' l i  in
          let (res'', i'') = compile_stmt' r i' in
          (res' @ res'', i'')
        | If (cond, c1, c2)  ->
          let label_true  = "if_" ^ string_of_int i ^ "_true"  in
          let label_false = "if_" ^ string_of_int i ^ "_false" in
          let label_end   = "if_" ^ string_of_int i ^ "_end"   in
          let (b1, i') = compile_stmt' c1 (i + 1) in
          let (b2, i'') = compile_stmt' c2 i' in
          (
            expr cond @
            [S_JIF label_true] @
            b2 @
            [S_JMP label_end; S_LABEL label_true] @
            b1 @
            [S_LABEL label_end],
            i''
          )
        | While (cond, code) ->
          let label_begin = "while_" ^ string_of_int i ^ "_begin" in
          let label_true  = "while_" ^ string_of_int i ^ "_true" in
          let label_end   = "while_" ^ string_of_int i ^ "_end" in
          let (body, i') = compile_stmt' code (i + 1) in
          (
            [S_LABEL label_begin] @
            expr cond @
            [S_JIF label_true; S_JMP label_end; S_LABEL label_true] @
            body @
            [S_JMP label_begin; S_LABEL label_end],
            i'
          )
        | Repeat (cond, code) ->
          let label_begin = "repeat_" ^ string_of_int i ^ "_begin" in
          let (body, i') = compile_stmt' code (i + 1) in
          (
            [S_LABEL label_begin] @
            body @
            expr (Op ("==", cond, Const 0))  @
            [S_JIF label_begin],
            i'
          )
      in
      let (res, _) = compile_stmt' s 0 in
      res
  end
