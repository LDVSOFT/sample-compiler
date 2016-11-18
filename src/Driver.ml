open Ostap
open Matcher

let parse (infile: string) =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident [
         "skip"; "if"; "else"; "elif"; "fi";
         "while"; "do"; "od"; "repeat"; "until";
         "for"; "fun"; "return"
       ] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.char s
       inherit Util.Lexers.string s
       inherit Util.Lexers.skip [
         Matcher.Skip.whitespaces " \t\n";
         Matcher.Skip.lineComment "--";
         Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.Program.parse) -EOF))

open Language

let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | _    -> `Int, Sys.argv.(1)
    in
    match parse filename with
    | `Ok stmt ->
      (match mode with
       | `X86 ->
         X86.build (Filename.chop_suffix filename ".expr") stmt
       | `SM  ->
         StackMachine.Interpreter.run (StackMachine.Compile.program stmt)
       | `Int ->
         Interpreter.Program.eval stmt
      )
    | `Fail er -> failwith er
  with
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
