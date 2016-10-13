open Language
open Expr
open Stmt
open StackMachine

let x86regs = [|"%esp"; "%ebp"; "%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let x86regs8 = [|"spl"; "%bpl"; "%al"; "%dl"; "%bl"; "%cl"; "%sil"; "%dil"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd = R of int | S of int | M of string | L of int

let x86esp = R (Utils.find x86regs "%esp")
let x86ebp = R (Utils.find x86regs "%ebp")
let x86eax = R (Utils.find x86regs "%eax")
let x86edx = R (Utils.find x86regs "%edx")

let allocate stack =
  match stack with
  | []                              -> R 4 (* preserve esp, ebp, eax and ebx and for stuff :) *)
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

type x86instr = (* src -> dest *)
  | X86Add   of opnd * opnd
  | X86Sub   of opnd * opnd
  | X86Mul   of opnd * opnd
  | X86And   of opnd * opnd
  | X86Or    of opnd * opnd
  | X86Xor   of opnd * opnd
  | X86Mov   of opnd * opnd
  | X86Cmp   of opnd * opnd
  | X86Div   of opnd
  | X86SetCC of string * opnd
  | X86Push  of opnd
  | X86Pop   of opnd
  | X86Ret
  | X86Cdq
  | X86Call  of string
  | X86Jmp   of string
  | X86Jne   of string
  | X86_Label of string

let x86compile : i list -> x86instr list = fun code ->
  let x86addStack s =
    match s with
    | S _ -> [X86Sub (L word_size, x86esp)]
    | _   -> []
  in
  let x86subStack s =
    match s with
    | S _ -> [X86Add (L word_size, x86esp)]
    | _   -> []
  in
  let op_buf = x86eax in
  let re_buf = x86edx in
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
    let (stack', x86code) =
      match i with
      | S_LABEL label ->
        (stack, [X86_Label label])
      | S_JMP label ->
        (stack, [X86Jmp label])
      | S_JIF label ->
        let x::stack' = stack in
        (stack', [X86Mov (x, x86eax); X86Cmp (L 0, x86eax); X86Jne label])
      | S_READ   ->
        let s = allocate stack in
        (s::stack, [X86Call "read"; X86Mov (x86eax, s)] @ x86addStack s)
      | S_WRITE  ->
        let s::stack' = stack in
        (stack', [X86Push s; X86Call "write"] @ x86subStack (S 0) @ x86subStack s)
      | S_PUSH n ->
        let s = allocate stack in
        (s::stack, [X86Mov (L n, s)] @ x86addStack s)
      | S_LD x ->
        let s = allocate stack in
        let res = match s with
        | R _ -> (s::stack, [X86Mov (M x, s)])
        | _   -> (s::stack, [X86Mov (M x, x86eax); X86Mov (x86eax, s); X86Sub (L word_size, x86esp)])
        in res
      | S_ST x ->
        let s::stack' = stack in
        let res = match s with
        | R _ -> (stack', [X86Mov (s, M x)])
        | _   -> (stack', [X86Mov (s, x86eax); X86Mov(x86eax, M x); X86Add (L word_size, x86esp)])
        in res
      | S_OP op -> (
        let x86compile_op2 op l r = match op with
          | "+"  -> X86Add (l, r)
          | "-"  -> X86Sub (l, r)
          | "*"  -> X86Mul (l, r)
          | "&&" -> X86And (l, r)
          | "!!" -> X86Or  (l, r)
          | _ -> invalid_arg ""
        in
        let x86compile_op1 op l = match op with
          | "<"  -> X86SetCC ("L" , l)
          | "<=" -> X86SetCC ("LE", l)
          | ">"  -> X86SetCC ("G" , l)
          | ">=" -> X86SetCC ("GE", l)
          | "==" -> X86SetCC ("E" , l)
          | "!=" -> X86SetCC ("NE", l)
          | _ -> invalid_arg ""
        in
        let protect t t' f = match t with
        | R x -> f t
        | _ -> [X86Mov (t, t')] @ (f t') @ [X86Mov (t', t)]
        in
        let y::x::stack' = stack in (x::stack', (
          match op with
          | "+" | "-" | "*" | "&&" | "!!" -> (
            let checkBool t = match op with
            | "&&" | "!!" -> [X86Mov (t, x86eax); X86Mov (L 0, re_buf); X86Cmp (L 0, x86eax); X86SetCC ("NE", re_buf); X86Mov (re_buf, t)]
            | _ -> []
            in
            checkBool x @ checkBool y @ protect x op_buf (fun x' -> [x86compile_op2 op y x'])
          )
          | "<" | "<=" | ">" | ">=" | "==" | "!=" -> (
            protect x op_buf (fun x' -> [X86Mov (L 0, re_buf); X86Cmp (y, x'); x86compile_op1 op re_buf; X86Mov(re_buf, x')])
          )
          | "/" | "%" -> (
            let res = match op with
              | "/" -> x86eax
              | "%" -> x86edx
            in
            [X86Mov (x, x86eax); X86Cdq; X86Div y; X86Mov (res, x)]
          )
        ) @ x86subStack y)
      )
   in
   x86code @ x86compile' stack' code'
  in
  [X86Push x86ebp; X86Mov (x86esp, x86ebp)] @ x86compile' [] code @ [X86Xor (x86eax, x86eax); X86Pop x86ebp; X86Ret]

let print_code code b =
  let rec pr_op opnd =
    match opnd with
    | R n -> x86regs.(n)
    | S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
    | M s -> s
    | L n -> Printf.sprintf "$%d" n
  in
  let rec pr_op8 opnd =
    match opnd with
    | R n -> x86regs8.(n)
    | S o -> Printf.sprintf "%d(%s)" ((-4) * o) (pr_op x86ebp)
    | M s -> s
    | L n -> Printf.sprintf "$%d" n
  in

  Buffer.add_string b @@ Printf.sprintf "main:\n";
  List.iter (fun instr ->
    match instr with
    | X86Add (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tSUBL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86And (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tANDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Or  (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tORL  \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Xor (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tXORL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mov (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tMOVL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp (o1, o2)  -> Buffer.add_string b @@ Printf.sprintf "\tCMPL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Div   o1      -> Buffer.add_string b @@ Printf.sprintf "\tIDIVL\t%s\n" (pr_op o1)
    | X86SetCC (s, o1) -> Buffer.add_string b @@ Printf.sprintf "\tSET%s\t%s\n" s (pr_op8 o1)
    | X86Push o1       -> Buffer.add_string b @@ Printf.sprintf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop  o1       -> Buffer.add_string b @@ Printf.sprintf "\tPOPL \t%s\n" (pr_op o1)
    | X86Ret           -> Buffer.add_string b @@ Printf.sprintf "\tRET  \n"
    | X86Cdq           -> Buffer.add_string b @@ Printf.sprintf "\tCDQ  \n"
    | X86Call s        -> Buffer.add_string b @@ Printf.sprintf "\tCALL \t%s\n" s
    | X86Jmp  s        -> Buffer.add_string b @@ Printf.sprintf "\tJMP  \t%s\n" s
    | X86Jne s         -> Buffer.add_string b @@ Printf.sprintf "\tJNE  \t%s\n" s
    | X86_Label s -> Buffer.add_string b @@ Printf.sprintf "%s:\n" s
  ) code

let print_compiled: Stmt.t -> string = fun stmt ->
  let buffer = Buffer.create 1024 in
  let asm = x86compile (StackMachine.Compile.stmt stmt) in
  let vars = Stmt.collect_vars stmt in
  Buffer.add_string buffer "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  print_code asm buffer;
  SS.iter (fun var ->
    Buffer.add_string buffer @@ Printf.sprintf "\t.comm %s 4\n" var
  ) vars;
  Buffer.contents buffer

let build: string -> Stmt.t -> int = fun file stmt ->
  let outf = open_out (Printf.sprintf "%s.s" file) in
  let runtime_dir = try
    Sys.getenv "RC_RUNTIME"
    with Not_found -> "../runtime"
  in
  Printf.fprintf outf "%s" (print_compiled stmt);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.s" file runtime_dir file)
