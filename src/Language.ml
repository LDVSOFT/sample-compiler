module SS = Set.Make(String)
open Ostap
open Matcher
open Utils

module Value =
  struct
    type t =
    | Int    of int
    | String of bytes

    let print x = match x with
    | Int n    -> string_of_int n
    | String s -> Printf.sprintf "\"%s\"" s

    ostap (
      parse:
        l:DECIMAL {Int l}
      | s:STRING  {String (String.sub s 1 (String.length s - 2)) }
    )
  end

module Expr =
  struct
    type t =
    | Const of Value.t
    | Var   of string
    | Op    of string * t * t
    | Call  of string * t list

    let rec collect_vars expr =
      match expr with
      | Const _      -> SS.empty
      | Var s        -> SS.singleton s
      | Op (_, l, r) -> SS.union (collect_vars l) (collect_vars r)
      | Call (_, ps) -> List.fold_left SS.union SS.empty @@ List.map collect_vars ps

    ostap (
      parse: log_or;

      log_or : l:log_and suf:( "!!"                         log_and)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      log_and: l:bol     suf:( "&&"                         bol    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      bol    : l:add     suf:(("<="|"<"|">="|">"|"=="|"!=") add    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      add    : l:mul     suf:(("+"|"-")                     mul    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      mul    : l:pri     suf:(("*"|"/"|"%")                 pri    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };

      pri:
        v:!(Value.parse)
        { Const v }
      | f:IDENT "(" args:parse_args ")"
        { Call (f, args) }
      | v:IDENT
        {Var v}
      | -"(" parse -")";

      parse_args: args:(parse (-"," parse)*)?
        { collect_args args }
    )
  end

module Stmt =
  struct
    type t =
    | Skip
    | Seq    of t * t
    | Assign of string * Expr.t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Repeat of Expr.t * t
    | Proc   of string * Expr.t list
    | Return of Expr.t

    let rec collect_vars stmt =
      match stmt with
      | Skip             -> SS.empty
      | Seq (l, r)       -> SS.union (collect_vars l) (collect_vars r)
      | Assign (x, e)    -> SS.union (SS.singleton x) (Expr.collect_vars e)
      | If (l, b1, b2)   -> SS.union (Expr.collect_vars l) @@ SS.union (collect_vars b1) (collect_vars b2)
      | While (l, r)     -> SS.union (Expr.collect_vars l) (collect_vars r)
      | Repeat (l, r)    -> SS.union (Expr.collect_vars l) (collect_vars r)
      | Proc (n, ps)     -> Expr.collect_vars @@ Expr.Call (n, ps)
      | Return expr      -> Expr.collect_vars expr

    ostap (
      parse:
        l:stmt suf:(-";" r:stmt)* { List.fold_left (fun l r -> Seq (l, r)) l suf };

      stmt:
        %"skip"
        { Skip }
      | x:IDENT ":=" e:!(Expr.parse)
        { Assign (x, e) }
      | %"if" cond1:!(Expr.parse) %"then" b1:parse suf:(%"elif" !(Expr.parse) %"then" parse)* last:(%"else" parse)? %"fi"
        { List.fold_right (fun (c, b) e -> If (c, b, e)) ((cond1, b1)::suf) (default (Skip) last) }
      | %"while" cond:!(Expr.parse) %"do" body:parse %"od"
        { While (cond, body) }
      | %"repeat" body:parse %"until" cond:!(Expr.parse)
        { Repeat (cond, body) }
      | %"for" init:parse "," cond:!(Expr.parse) "," step:parse %"do" body:parse %"od"
        { Seq (init, While (cond, Seq (body, step))) }
      | %"return" expr:!(Expr.parse)
        { Return expr }
      | f:IDENT "(" args:!(Expr.parse_args) ")"
        { Proc (f, args) }
    )
  end

module Program =
  struct
    type func = {args: string list; body: Stmt.t}
    type t = {funcs: (string * func) list}

    ostap (
      parse: f:funcdef* m:!(Stmt.parse)
        { {funcs = ("main", {args = []; body = m})::f} };

      funcdef: %"fun" f:IDENT "(" args:(IDENT (-"," IDENT)*)? ")" "begin" b:!(Stmt.parse) "end"
        { (f, {args = collect_args args; body = b}) }
    )
  end
