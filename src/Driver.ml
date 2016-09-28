open Expr
open Parser

let main = 
  try
    let filename: string = Sys.argv.(1) in
    match parse filename with
    | `Ok stmt -> ignore @@ build stmt (Filename.chop_suffix filename ".expr")
    | `Fail er -> Printf.eprintf "%s\n" er
  with Invalid_argument _ ->
    Printf.eprintf "Usage: rc.byte <name.expr>"
