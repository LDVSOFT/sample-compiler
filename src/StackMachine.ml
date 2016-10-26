type i =
| S_LABEL of string
| S_READ
| S_WRITE
| S_PUSH  of int
| S_POP
| S_LD    of string
| S_ST    of string
| S_OP    of string
| S_JMP   of string
| S_JIF   of string
| S_CALL  of string * string list
| S_RET
| S_END
| S_COMM  of string

module Interpreter =
  struct
    open Interpreter
    open Utils

    type frame_t = {
      vars: (string * int) list;
      ip: int
    }

    type state_t = {
      frame: frame_t;
      stack: int list;
      callstack: frame_t list;
      input: int list;
      output: int list;
    }

    let run (input: int list) (code': i list): int list =
      let code = Array.of_list code' in
      let rec run' (state: state_t): state_t =
        if state.frame.ip >= Array.length code
        then failwith "Out of stack program"
        else (
          let next_state = {state with frame = {state.frame with ip = state.frame.ip + 1}} in
          match code.(state.frame.ip) with
          | S_READ   ->
            let y::input' = state.input in
            run' {next_state with stack = y::state.stack; input = input'}
          | S_WRITE  ->
            let y::stack' = state.stack in
            run' {next_state with stack = stack'; output = state.output @ [y]}
          | S_PUSH n ->
            run' {next_state with stack = n::state.stack}
          | S_POP ->
            let _::stack' = state.stack in
            run' {next_state with stack = stack'}
          | S_LD x ->
            run' {next_state with stack = (List.assoc x state.frame.vars)::state.stack}
          | S_ST x ->
            let y::stack' = state.stack in
            run' {state with stack = stack'; frame = {next_state.frame with vars = (x, y)::state.frame.vars}}
          | S_OP op ->
            let y::x::stack' = state.stack in
            run' {next_state with stack = (Expr.eval_op op x y)::stack'}
          | S_LABEL _ | S_COMM _ ->
            run' next_state
          | S_JMP label ->
            let where = find code @@ S_LABEL label in
            run' {state with frame = {state.frame with ip = where}}
          | S_JIF label ->
            let y::stack' = state.stack in
            let where = find code @@ S_LABEL label in
            run' @@ if y == 0 then next_state
            else {state with frame = {state.frame with ip = where}}
          | S_CALL (func, params) ->
            let where = find code @@ S_LABEL func in
            let params' = List.rev params in
            let (stack', substate) =
              let rec f (stack: int list) (substate: (string * int) list) (params: string list): (int list * (string * int) list) =
                match params with
                | []         -> (stack, substate)
                | v::params' ->
                  let y::stack' = stack in
                  f stack' ((v, y)::substate) params'
              in f state.stack [] params'
            in run' {state with stack = stack'; callstack = state.frame::state.callstack; frame = {vars = substate; ip = where}}
          | S_RET ->
            let frame::callstack' = state.callstack in
            run' {state with frame = {frame with ip = frame.ip + 1}; callstack = callstack'}
          | S_END ->
            state
         )
      in
      let state = {frame = {ip = 0; vars = []}; callstack = []; stack = []; input = input; output = []} in
      let state' = run' state in
      state'.output
  end

module Compile =
  struct
    open Language.Expr
    open Language.Stmt
    open Language.Program

    let program (p: Language.Program.t): i list =
      let rec expr (e: Language.Expr.t): i list = match e with
      | Var x         ->
        [
          S_COMM x;
          S_LD x
        ]
      | Const n       ->
        [
          S_COMM (Printf.sprintf "=%d" n);
          S_PUSH n
        ]
      | Op (op, l, r) ->
        expr l @
        expr r @
        [
          S_COMM op;
          S_OP op
        ]
      | Call (f, ps)  ->
        let func = List.assoc f p.funcs in
        (List.concat @@ List.map expr ps) @
        [
          S_COMM (Printf.sprintf "Call %s" f);
          S_CALL (f, func.args)
        ]
      in
      let stmt (name: string) (s: Language.Stmt.t): i list =
        let rec stmt' s i = match s with
          | Skip               -> ([
              S_COMM "Skip"
            ], i)
          | Seq (l, r)         ->
            let (res' , i' ) = stmt' l i  in
            let (res'', i'') = stmt' r i' in
            (res' @ res'', i'')
          | Read x             -> ([
              S_COMM (Printf.sprintf "Read %s" x);
              S_READ;
              S_ST x
            ], i)
          | Write e            -> ([
              S_COMM "Write..."] @
              expr e @ [
              S_WRITE
            ], i)
          | Assign (x, e)      -> ([
              S_COMM (Printf.sprintf "Assign %s..." x)] @
              expr e @ [
              S_ST x
            ], i)
          | If (cond, c1, c2)  ->
            let label_true  = name ^ "_if_" ^ string_of_int i ^ "_true"  in
            let label_false = name ^ "_if_" ^ string_of_int i ^ "_false" in
            let label_end   = name ^ "_if_" ^ string_of_int i ^ "_end"   in
            let (b1, i' ) = stmt' c1 (i + 1) in
            let (b2, i'') = stmt' c2 i' in
            ([
              S_COMM "If..."] @
              expr cond @ [
              S_JIF label_true;
              S_COMM "Else..."] @
              b2 @ [
              S_JMP label_end;
              S_LABEL label_true;
              S_COMM "Then..."] @
              b1 @ [
              S_LABEL label_end;
              S_COMM "EndIf"
            ], i'')
          | While (cond, code) ->
            let label_begin = name ^ "_while_" ^ string_of_int i ^ "_begin" in
            let label_end   = name ^ "_while_" ^ string_of_int i ^ "_end" in
            let (body, i') = stmt' code (i + 1) in
            ([
              S_LABEL label_begin;
              S_COMM "While..."] @
              expr (Op ("==", cond, Const 0)) @ [
              S_JIF label_end;
              S_COMM "Do..."] @
              body @ [
              S_JMP label_begin;
              S_LABEL label_end;
              S_COMM "EndWhile"
            ], i')
          | Repeat (cond, code) ->
            let label_begin = name ^ "_repeat_" ^ string_of_int i ^ "_begin" in
            let (body, i') = stmt' code (i + 1) in
            ([
              S_LABEL label_begin;
              S_COMM "Repeat..."] @
              body @ [
              S_COMM "Until..."] @
              expr (Op ("==", cond, Const 0)) @ [
              S_JIF label_begin;
              S_COMM "EndRepeat"
            ], i')
          | Proc (f, ps)        ->
            let func = List.assoc f p.funcs in
            (
              (List.concat @@ List.map expr ps) @ [
              S_COMM (Printf.sprintf "Call %s (proc)" f);
              S_CALL (f, func.args);
              S_POP
            ], i)
          | Return e             ->
            (
              expr e @ [
              S_RET
            ], i)
        in
        let (res, _) = stmt' s 0 in
        res
      in
      let func (name: string) (f: Language.Program.func): i list = [
        S_COMM (Printf.sprintf "Function %s(%s)..." name @@ String.concat "," f.args);
        S_LABEL name ] @
        stmt name f.body
      in
      stmt "main" p.main @ [
      S_END ] @
      (List.concat @@ List.map (fun (name, f) -> func name f) p.funcs)
  end
