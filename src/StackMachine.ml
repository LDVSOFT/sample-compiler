open Utils
open Language

type i =
| S_LABEL of string
| S_ENTER of string list * string list
| S_PUSH  of Value.t
| S_POP
| S_LD    of string
| S_ST    of string
| S_OP    of string
| S_JMP   of string
| S_JIF   of string
| S_CALL  of string * int
| S_BUILT of string * int
| S_RET
| S_COMM  of string

module Compile =
  struct
    open Language
    open Value
    open Expr
    open Stmt
    open Program
    open Interpreter

    let program (p: Language.Program.t): i list =
      let rec expr (e: Language.Expr.t): i list = match e with
      | Var x         ->
        [
          S_COMM x;
          S_LD x
        ]
      | Const n       ->
        [
          S_COMM (Printf.sprintf "=%s" @@ Value.print n);
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
        try
          let func = List.assoc f p.funcs in
          ([
            S_COMM (Printf.sprintf "Call %s..." f) ] @
            (List.concat @@ List.map expr ps) @ [
            S_COMM (Printf.sprintf "Call %s" f);
            S_CALL (f, List.length func.args)
          ])
        with Not_found ->
          let builtin: Interpreter.Builtins.t = Interpreter.Builtins.get f in
          ([
            S_COMM (Printf.sprintf "Call builtin %s..." f)] @
            (List.concat @@ List.map expr ps) @ [
            S_COMM (Printf.sprintf "Call builtin %s" f);
            S_BUILT (f, builtin.args)
          ])
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
          | Assign (x, e)      -> ([
              S_COMM (Printf.sprintf "Assign %s..." x)] @
              expr e @ [
              S_ST x
            ], i)
          | If (cond, c1, c2)  ->
            let label_true  = name ^ "_if_" ^ string_of_int i ^ "_true"  in
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
              expr (Op ("==", cond, Const (Int 0))) @ [
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
              expr (Op ("==", cond, Const (Int 0))) @ [
              S_JIF label_begin;
              S_COMM "EndRepeat"
            ], i')
          | Proc (f, ps)        ->
            (
              expr (Call (f, ps)) @ [
              S_POP
            ], i)
          | Return e            ->
            (
              expr e @ [
              S_RET
            ], i)
        in
        let (res, _) = stmt' s 0 in
        res
      in
      let func (name: string) (f: Language.Program.func): i list =
        let all_var = Language.Stmt.collect_vars f.body in
        let local_var = SS.diff all_var (SS.of_list f.args)
        in [
          S_COMM (Printf.sprintf "Function %s(%s)..." name @@ String.concat "," f.args);
          S_LABEL name;
          S_ENTER (f.args, SS.elements local_var)] @
          stmt name @@ Seq (f.body, Return (Const (Int 0)))
      in
      List.concat @@ List.map (fun (name, f) -> func name f) p.funcs
  end

module Interpreter =
  struct
    open Interpreter

    type frame_t = {
      vars: (string * Value.t) list;
      ip: int
    }

    type state_t = {
      frame: frame_t;
      stack: Value.t list;
      callstack: frame_t list
    }

    let run (code': i list): unit =
      let code = Array.of_list code' in
      let rec run' (state: state_t): state_t =
        if state.frame.ip >= Array.length code
        then failwith "Out of stack program"
        else (
          let next_state = {state with frame = {state.frame with ip = state.frame.ip + 1}} in
          match code.(state.frame.ip) with
          | S_PUSH n ->
            run' {next_state with stack = n::state.stack}
          | S_POP ->
            let (_, stack') = cut_head state.stack in
            run' {next_state with stack = stack'}
          | S_LD x ->
            run' {next_state with stack = (List.assoc x state.frame.vars)::state.stack}
          | S_ST x ->
            let (y, stack') = cut_head state.stack in
            run' {state with stack = stack'; frame = {next_state.frame with vars = (x, y)::state.frame.vars}}
          | S_OP op ->
            let (y, stack' ) = cut_head state.stack in
            let (x, stack'') = cut_head stack' in
            run' {next_state with stack = (Expr.eval_op op x y)::stack''}
          | S_LABEL _ | S_COMM _ ->
            run' next_state
          | S_JMP label ->
            let where = find code @@ S_LABEL label in
            run' {state with frame = {state.frame with ip = where}}
          | S_JIF label ->
            let (y, stack') = cut_head state.stack in
            let where = find code @@ S_LABEL label in
            run' @@ if Value.toi y == 0 then next_state
            else {state with frame = {state.frame with ip = where}}
          | S_CALL (func, _) ->
            let where = find code @@ S_LABEL func in
            run' {state with callstack = state.frame::state.callstack; frame = {vars = []; ip = where}}
          | S_BUILT (func, cnt) ->
            let (args, stack') = list_cut state.stack cnt in
            let res = Interpreter.Builtins.invoke func (List.rev args) in
            run' {next_state with stack = res::stack'}
          | S_ENTER (params, _) ->
            let params' = List.rev params in
            let (stack', vars') =
              let rec f (stack: Value.t list) (substate: (string * Value.t) list) (params: string list): (Value.t list * (string * Value.t) list) =
                match params with
                | []         -> (stack, substate)
                | v::params' ->
                  let (y, stack') = cut_head stack in
                  f stack' ((v, y)::substate) params'
              in f state.stack next_state.frame.vars params'
            in
            run' {next_state with frame = {next_state.frame with vars = vars'}; stack = stack'}
          | S_RET ->
            match state.callstack with
            | [] -> state
            | frame::callstack' ->
              run' {state with frame = {frame with ip = frame.ip + 1}; callstack = callstack'}
         )
      in
      let enter_point = find code @@ S_LABEL "main" in
      let state = {frame = {ip = enter_point; vars = []}; callstack = []; stack = []} in
      let _ = run' state in
      ()
  end

