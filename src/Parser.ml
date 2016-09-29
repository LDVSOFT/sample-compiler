open Ostap
open Matcher
open Expr

ostap (
  expr: bol;

  bol: l:add suf:(("<="|"<"|">="|">"|"=="|"!=") add)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
  add: l:mul suf:(("+"|"-")                     mul)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };
  mul: l:pri suf:(("*"|"/"|"%")                 pri)* { List.fold_left (fun l (op, r) -> Op (Token.repr op, l, r)) l suf };

  pri:
    l:DECIMAL        {Const l}
  | v:IDENT          {Var v}
  | -"(" e:expr -")" (* without braces: use everything without dashes *)
)

ostap (
  stmts:
    l:stmt ";" r:stmts { Seq (l, r) }
  | stmt;

  stmt:
    %"read" "(" name:IDENT ")"                   { Read name }
  | %"write" "(" e:expr ")"                      { Write e }
  | %"skip"                                      { Skip }
  | %"if" "(" cond:expr ")" "{" body:stmt "}"    { If (cond, body) }
  | %"while" "(" cond:expr ")" "{" body:stmt "}" { While (cond, body) }
  | x:IDENT ":=" e:expr                          { Assign (x, e) }
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
      inherit Matcher.t s
      inherit Util.Lexers.ident ["read"; "write"; "skip"] s
      inherit Util.Lexers.decimal s
      inherit Util.Lexers.skip [
        Matcher.Skip.whitespaces " \t\n";
        Matcher.Skip.lineComment "--";
        Matcher.Skip.nestedComment "(*" "*)"
      ] s
    end)
    (ostap (stmts -EOF))
