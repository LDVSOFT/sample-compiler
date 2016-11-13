open Ostap
open Matcher

let parse (infile: string) =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident [
         "read"; "write"; "skip"; "if"; "else"; "elif"; "fi";
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
      let rec read acc =
        try
          let r = read_int () in
          Printf.printf "> ";
          read (acc @ [Value.Int r])
        with End_of_file -> acc
      in
      (match mode with
       | `X86 ->
         X86.build (Filename.chop_suffix filename ".expr") stmt
       | `SM  ->
         let input = read [] in
         let output =
           StackMachine.Interpreter.run input (StackMachine.Compile.program stmt)
         in
         List.iter (fun i -> Printf.printf "%s\n" @@ Value.print i) output
       | `Int ->
         let input = read [] in
         let output = Interpreter.Program.eval input stmt in
         List.iter (fun i -> Printf.printf "%s\n" @@ Value.print i) output
      )
    | `Fail er -> Printf.eprintf "%s\n" er
  with
  | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
