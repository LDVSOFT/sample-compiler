open Ostap
open Matcher

module Expr =
  struct
    type t =
    | Const of int
    | Var   of string
    | Op    of string * t * t

    ostap (
      parse: log_or;

      log_or : l:log_and suf:( "!!"                         log_and)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      log_and: l:bol     suf:( "&&"                         bol    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      bol:     l:add     suf:(("<="|"<"|">="|">"|"=="|"!=") add    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      add:     l:mul     suf:(("+"|"-")                     mul    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
      mul:     l:pri     suf:(("*"|"/"|"%")                 pri    )* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };

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

    ostap (
      parse:
        l:stmt ";" r:parse { Seq (l, r) }
      | stmt;

      stmt:
        %"read" "(" name:IDENT ")"                                    { Read name }
      | %"write" "(" e:!(Expr.parse) ")"                              { Write e }
      | %"skip"                                                       { Skip }
      | %"if" cond:!(Expr.parse) "then" b1:parse "else" b2:parse "fi" { If (cond, b1, b2) }
      | %"while" cond:!(Expr.parse) "do" body:parse "od"              { While (cond, body) }
      | x:IDENT ":=" e:!(Expr.parse)                                  { Assign (x, e) }
    )
  end
