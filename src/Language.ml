module SS = Set.Make(String)
open Ostap
open Matcher
open Utils

module Value =
  struct
    type t =
    | Int    of int
    | String of bytes
    | Array  of t array

    let rec print x = match x with
    | Int n    -> string_of_int n
    | String s -> Printf.sprintf "\"%s\"" s
    | Array a ->
      let b = Buffer.create 1024 in
      Printf.bprintf b "[";
      Printf.bprintf b "%s" @@ String.concat ", " @@ List.map print @@ Array.to_list a;
      Printf.bprintf b "]";
      Buffer.contents b

    ostap (
      parse:
        unboxed
      | boxed;

      unboxed:
        l:DECIMAL {Int l}
      | c:CHAR    {Int (Char.code c)};

      boxed:
        s:STRING                      {String (String.sub s 1 (String.length s - 2)) }
    | "[" s:!(Util.list0 unboxed) "]" {Array (Array.of_list s)}
    | "{" s:!(Util.list0 boxed)   "}" {Array (Array.of_list s)}
    )
  end

module Expr =
  struct
    type t =
    | Const of Value.t
    | Var   of string
    | Op    of string * t * t
    | Call  of string * t list

    let rec print x = match x with
    | Const v -> Printf.sprintf "Const %s" @@ Value.print v
    | Var s -> Printf.sprintf "Var %s" s
    | Op (op, l, r) -> Printf.sprintf "(%s) %s (%s)" (print l) op (print r)
    | Call (f, args) ->
      let b = Buffer.create 32 in
      Printf.bprintf b "%s(" f;
      Printf.bprintf b "%s" @@ String.concat ", " @@ List.map print args;
      Printf.bprintf b ")";
      Buffer.contents b

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
        f:basic coords:(-"[" parse -"]")*
        { List.fold_left (fun l r -> Call ("__arrget", [l; r])) f coords };

      basic:
        v:!(Value.parse)
        { Const v }
      | f:IDENT "(" args:!(Util.list0 parse) ")"
        { Call (f, args) }
      | v:IDENT
        {Var v}
      | -"(" parse -")"
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

    let rec print x = match x with
    | Skip -> "skip"
    | Seq (l, r) -> (print l) ^ ";\n" ^ (print r)
    | Assign (s, e) -> Printf.sprintf "%s := %s" s (Expr.print e)
    | While (e, s) -> Printf.sprintf "while %s do\n%s\nod" (Expr.print e) (print s)
    | Proc (f, args) -> Expr.print @@ Expr.Call (f, args)
    | Return e -> Printf.sprintf "return %s" @@ Expr.print e

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
        s:stmt? ss:(-";" parse)? { Seq (Utils.default Skip s, Utils.default Skip ss) };

      stmt:
        %"skip"
        { Skip }
      | f:IDENT coords:(-"[" !(Expr.parse) -"]")+ ":=" e:!(Expr.parse)
        {
          let (coord, coords') = cut_tail coords in
          let f' = List.fold_left (fun l r -> Expr.Call ("__arrget", [l; r])) (Expr.Var f) coords' in
          Proc ("__arrset", [f'; coord; e])
        }
      | x:IDENT ":=" e:!(Expr.parse)
        { Assign (x, e) }
      | %"if" cond1:!(Expr.parse) %"then" b1:parse suf:(%"elif" !(Expr.parse) %"then" parse)* last:(%"else" parse)? %"fi"
        { List.fold_right (fun (c, b) e -> If (c, b, e)) ((cond1, b1)::suf) (default Skip last) }
      | %"while" cond:!(Expr.parse) %"do" body:parse %"od"
        { While (cond, body) }
      | %"repeat" body:parse %"until" cond:!(Expr.parse)
        { Repeat (cond, body) }
      | %"for" init:parse "," cond:!(Expr.parse) "," step:parse %"do" body:parse %"od"
        { Seq (init, While (cond, Seq (body, step))) }
      | %"return" expr:!(Expr.parse)
        { Return expr }
      | f:IDENT "(" args:!(Util.list0 Expr.parse) ")"
        { Proc (f, args) }
    )
  end

module Program =
  struct
    type func = {args: string list; body: Stmt.t}
    type t = {funcs: (string * func) list}

    let print p =
      let print_f (n, f) =
        Printf.eprintf "fun %s(" n;
        Printf.eprintf "%s" @@ String.concat ", " f.args;
        Printf.eprintf ")\nbegin\n";
        Printf.eprintf "%s\n" @@ Stmt.print f.body;
        Printf.eprintf "end\n\n"
      in
      List.iter print_f p.funcs

    ostap (
      parse: f:funcdef* m:!(Stmt.parse)
        { {funcs = ("main", {args = []; body = m})::f} };

      arg: IDENT;
      funcdef: %"fun" f:IDENT "(" args:!(Util.list0 arg) ")" "begin" b:!(Stmt.parse) "end"
        { (f, {args = args; body = b}) }
    )
  end
