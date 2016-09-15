open Expr

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
let p =
  Seq (
      Seq (              
          Read "x",
          Read "y"
      ),
      Seq (
          Seq (
              Assign ("x1", Mul (Var "x", Var "x")),
              Assign ("y1", Mul (Var "y", Var "y"))
          ),
          Write (Add (Var "y1", Var "x1"))
      )
    )

(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
(*
let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)
*)
(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
(*
let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" * !"x" * !"y" + !"x" * !"y" + !"y") |>
  write (!"z" + !"y")  
*)
(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
                
let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
let _ =
  let _ = Printf.printf "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n" in
  let _ = Printf.printf "main:\n" in
  Printf.printf ""
  let pr_op opnd =
    match opnd with
    | R n -> x86regs.(n)
    | S o -> Printf.sprintf "%d(%%ebx)" ((-4) * o)
    | M s -> s
    | L n -> Printf.sprintf "$%d" n
  in
  let vars = collect_vars p in
  let stack_p = compile_stmt p in
  let x86_p = x86compile stack_p in
  let _ = List.iter (fun instr ->
    match instr with
    | X86Add (o1, o2) -> Printf.printf "\tADDL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2) -> Printf.printf "\tSUBL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2) -> Printf.printf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mov (o1, o2) -> Printf.printf "\tMOVL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Push o1 -> Printf.printf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop o1 -> Printf.printf "\tPOPL\t%s\n" (pr_op o1)
    | X86Call s -> Printf.printf "\tCALL\t%s\n" s
    | X86Ret -> Printf.printf "\tRET\n"
  ) x86_p in
  let _ = Printf.printf "\tRET\n\n" in
  SS.iter (fun var ->
    Printf.printf "\t.comm %s 4\n" var
  ) vars
