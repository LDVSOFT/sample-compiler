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
      Seq (
        Read "x",
        Read "y"
      ),
      Seq (
        Seq (
          Assign ("x1", Op (Mul, Var "x", Var "x")),
          Assign ("y1", Op (Mul, Var "y", Var "y"))
        ),
        Write (Op (Add, Var "y1", Var "x1"))
      )
    ),
    Seq (
      Seq (
        Seq (
          Read "a",
          Read "b"
        ),
       Write (Op (Gt, Var "a", Var "b"))
      ),
      Seq (
        Write (Op (Div, Var "a", Var "b")),
        Write (Op (Mod, Var "a", Var "b"))
      )
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
let rec pr_op opnd =
  match opnd with
  | R n -> x86regs.(n)
  | S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
  | M s -> s
  | L n -> Printf.sprintf "$%d" n
let rec pr_op8 opnd =
  match opnd with
  | R n -> x86regs8.(n)
  | S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
  | M s -> s
  | L n -> Printf.sprintf "$%d" n

let _ =
  let vars = collect_vars p in
  let code = x86compile (compile_stmt p) in

  Printf.printf "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  Printf.printf "main:\n";
  List.iter (fun instr ->
    match instr with
    | X86Add (o1, o2) -> Printf.printf "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2) -> Printf.printf "\tSUBL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2) -> Printf.printf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86And (o1, o2) -> Printf.printf "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Or  (o1, o2) -> Printf.printf "\tORL  \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Xor (o1, o2) -> Printf.printf "\tXORL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mov (o1, o2) -> Printf.printf "\tMOVL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp (o1, o2) -> Printf.printf "\tCMPL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Div   o1     -> Printf.printf "\tIDIVL\t%s\n" (pr_op o1)
    | X86Setl  o1     -> Printf.printf "\tSETL \t%s\n" (pr_op8 o1)
    | X86Setle o1     -> Printf.printf "\tSETLE\t%s\n" (pr_op8 o1)
    | X86Setg  o1     -> Printf.printf "\tSETG \t%s\n" (pr_op8 o1)
    | X86Setge o1     -> Printf.printf "\tSETGE\t%s\n" (pr_op8 o1)
    | X86Sete  o1     -> Printf.printf "\tSETN \t%s\n" (pr_op8 o1)
    | X86Setne o1     -> Printf.printf "\tSETNE\t%s\n" (pr_op8 o1)
    | X86Push o1      -> Printf.printf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop  o1      -> Printf.printf "\tPOPL \t%s\n" (pr_op o1)
    | X86Ret          -> Printf.printf "\tRET  \n"
    | X86Cdq          -> Printf.printf "\tCDQ  \n"
    | X86Call s       -> Printf.printf "\tCALL \t%s\n" s
  ) code;
  Printf.printf "\tXORL\t%s,\t%s\n\tRET\n\n" (pr_op x86eax) (pr_op x86eax);
  SS.iter (fun var ->
    Printf.printf "\t.comm %s 4\n" var
  ) vars
