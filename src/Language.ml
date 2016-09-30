open Ostap
open Matcher

module Expr =
  struct
    type t =
    | Const of int
    | Var   of string
    | Op    of string * t * t

    ostap (
      parse: bol;

      bol: l:add suf:(("<="|"<"|">="|">"|"=="|"!=") add)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      add: l:mul suf:(("+"|"-")                     mul)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      mul: l:pri suf:(("*"|"/"|"%")                 pri)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };

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
    | If     of Expr.t * t
    | While  of Expr.t * t

    ostap (
      parse:
        l:stmt ";" r:parse { Seq (l, r) }
      | stmt;

      stmt:
        %"read" "(" name:IDENT ")"                             { Read name }
      | %"write" "(" e:!(Expr.parse) ")"                       { Write e }
      | %"skip"                                                { Skip }
      | %"if"    "(" cond:!(Expr.parse) ")" "{" body:parse "}" { If (cond, body) }
      | %"while" "(" cond:!(Expr.parse) ")" "{" body:parse "}" { While (cond, body) }
      | x:IDENT ":=" e:!(Expr.parse)                           { Assign (x, e) }
    )
  end
