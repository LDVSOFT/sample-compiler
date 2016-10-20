module SS = Set.Make(String)
open Ostap
open Matcher
open Utils

module Expr =
  struct
    type t =
    | Const of int
    | Var   of string
    | Op    of string * t * t

    let rec collect_vars expr =
      match expr with
      | Const _      -> SS.empty
      | Var s        -> SS.singleton s
      | Op (_, l, r) -> SS.union (collect_vars l) (collect_vars r)

    ostap (
      parse: log_or;

      log_or : l:log_and suf:( "!!"                         log_and)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      log_and: l:bol     suf:( "&&"                         bol    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      bol    : l:add     suf:(("<="|"<"|">="|">"|"=="|"!=") add    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      add    : l:mul     suf:(("+"|"-")                     mul    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      mul    : l:pri     suf:(("*"|"/"|"%")                 pri    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };

      pri:
        l:DECIMAL        {Const l}
      | v:IDENT          {Var v}
      | -"(" e:parse -")"
    )
  end

module Stmt =
  struct
    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Repeat of Expr.t * t

    let rec collect_vars stmt =
      match stmt with
      | Skip             -> SS.empty
      | Read x           -> SS.singleton x
      | Write e          -> Expr.collect_vars e
      | Assign (x, e)    -> SS.union (SS.singleton x) (Expr.collect_vars e)
      | Seq (l, r)       -> SS.union (collect_vars l) (collect_vars r)
      | If (l, b1, b2)   -> SS.union (Expr.collect_vars l) @@ SS.union (collect_vars b1) (collect_vars b2)
      | While (l, r)     -> SS.union (Expr.collect_vars l) (collect_vars r)
      | Repeat (l, r)    -> SS.union (Expr.collect_vars l) (collect_vars r)

    ostap (
      parse:
        l:stmt suf:(-";" r:stmt)* { List.fold_left (fun l r -> Seq (l, r)) l suf };

      stmt:
        %"skip"
        { Skip }
      | %"read" "(" name:IDENT ")"
        { Read name }
      | %"write" "(" e:!(Expr.parse) ")"
        { Write e }
      | x:IDENT ":=" e:!(Expr.parse)
        { Assign (x, e) }
      | %"if" cond1:!(Expr.parse) %"then" b1:parse suf:(%"elif" !(Expr.parse) %"then" parse)* last:(%"else" else_body:parse)? %"fi"
        { List.fold_right (fun (c, b) e -> If (c, b, e)) ((cond1, b1)::suf) (default (Skip) last) }
      | %"while" cond:!(Expr.parse) %"do" body:parse %"od"
        { While (cond, body) }
      | %"repeat" body:parse %"until" cond:!(Expr.parse)
        { Repeat (cond, body) }
      | %"for" init:parse "," cond:!(Expr.parse) "," step:parse %"do" body:parse %"od"
        { Seq (init, While (cond, Seq (body, step))) }
    )
  end
