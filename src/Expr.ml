module SS = Set.Make(String)

let eval_op op l r =
  let toi x = if x then 1 else 0 in
  match op with
  | "+" -> l + r
  | "-" -> l - r
  | "*" -> l * r
  | "/" -> l / r
  | "%" -> l mod r
  | "&&" -> toi ((l != 0) && (r != 0))
  | "||" -> toi ((l != 0) || (r != 0))
  | "<"  -> toi (l < r)
  | ">"  -> toi (l > r)
  | "<=" -> toi (l <= r)
  | ">=" -> toi (l >= r)
  | "==" -> toi (l == r)
  | "!=" -> toi (l != r)
  | _ -> invalid_arg "Unknown op"

type expr =
  | Const of int
  | Var   of string
  | Op    of string * expr * expr

let rec eval state expr =
  match expr with
  | Const n      -> n
  | Var   x      -> state x
  | Op (binop, l, r) -> eval_op binop (eval state l) (eval state r)

type stmt =
  | Skip
  | Read   of string
  | Write  of expr
  | Assign of string * expr
  | Seq    of stmt * stmt

let run input stmt =
  let rec run' ((state, input, output) as c) stmt =
    let state' x = List.assoc x state in
    match stmt with
    | Skip          -> c
    | Seq    (l, r) -> run' (run' c l) r
    | Assign (x, e) -> ((x, eval state' e) :: state, input, output)
    | Write   e     -> (state, input, output @ [eval state' e])
    | Read    x     ->
       let y::input' = input in
       ((x, y) :: state, input', output)
  in
  let (_, _, result) = run' ([], input, []) stmt in
  result

let rec collect_vars stmt =
  let rec collect_vars_expr expr =
    match expr with
    | Const _ -> SS.empty
    | Var s -> SS.singleton s
    | Op (_, l, r) -> SS.union (collect_vars_expr l) (collect_vars_expr r)
  in
  match stmt with
  | Skip -> SS.empty
  | Seq (l, r) -> SS.union (collect_vars l) (collect_vars r)
  | Assign (x, e) -> SS.union (SS.singleton x) (collect_vars_expr e)
  | Write e -> collect_vars_expr e
  | Read x -> SS.singleton x

type instr =
  | S_READ
  | S_WRITE
  | S_PUSH  of int
  | S_LD    of string
  | S_ST    of string
  | S_OP    of string

let srun input code =
  let rec srun' (state, stack, input, output) code =
    match code with
    | []       -> output
    | i::code' ->
       srun'
         (match i with
          | S_READ ->
             let y::input' = input in
             (state, y::stack, input', output)
          | S_WRITE ->
             let y::stack' = stack in
             (state, stack', input, output @ [y])
          | S_PUSH n ->
             (state, n::stack, input, output)
          | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
          | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack', input, output)
          | S_OP op ->
             let y::x::stack' = stack in
             (state, (eval_op op x y)::stack', input, output)
         )
         code'
  in
  srun' ([], [], input, []) code

let rec compile_expr expr =
  match expr with
  | Var    x     -> [S_LD   x]
  | Const  n     -> [S_PUSH n]
  | Op (op, l, r) -> compile_expr l @ compile_expr r @ [S_OP op]

let rec compile_stmt stmt =
  match stmt with
  | Skip          -> []
  | Assign (x, e) -> compile_expr e @ [S_ST x]
  | Read    x     -> [S_READ; S_ST x]
  | Write   e     -> compile_expr e @ [S_WRITE]
  | Seq    (l, r) -> compile_stmt l @ compile_stmt r

let x86regs = [|"%esp"; "%ebp"; "%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let x86regs8 = [|"spl"; "%bpl"; "%al"; "%dl"; "%bl"; "%cl"; "%sil"; "%dil"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd = R of int | S of int | M of string | L of int

let find a x =
  let rec find' a x n =
    if a.(n) = x
      then n
      else find' a x (n + 1)
  in find' a x 0

let x86esp = R (find x86regs "%esp")
let x86ebp = R (find x86regs "%ebp")
let x86eax = R (find x86regs "%eax")
let x86edx = R (find x86regs "%edx")

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
  | X86Setl  of opnd
  | X86Setle of opnd
  | X86Setg  of opnd
  | X86Setge of opnd
  | X86Sete  of opnd
  | X86Setne of opnd
  | X86Push  of opnd
  | X86Pop   of opnd
  | X86Ret
  | X86Cdq
  | X86Call  of string

let x86compile : instr list -> x86instr list = fun code ->
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
  let rec x86compile' stack code =
    match code with
    | []       -> []
    | i::code' ->
    let (stack', x86code) =
      match i with
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
          | "||" -> X86Or  (l, r)
          | _ -> invalid_arg ""
        in
        let x86compile_op1 op l = match op with
          | "<"  -> X86Setl  l
          | "<=" -> X86Setle l
          | ">"  -> X86Setg  l
          | ">=" -> X86Setge l
          | "==" -> X86Sete  l
          | "!=" -> X86Setne l
          | _ -> invalid_arg ""
        in
        let op_buf = x86eax in
        let re_buf = x86edx in
        let protect t t' f = match t with
        | R x -> f t
        | _ -> [X86Mov (t, t')] @ (f t') @ [X86Mov (t', t)]
        in
        let y::x::stack' = stack in (x::stack', (
          match op with
          | "+" | "-" | "*" | "&&" | "||" -> (
            let checkBool t = match op with
            | "&&" | "||" -> [X86Mov (t, x86eax); X86Mov (L 0, re_buf); X86Cmp (L 0, x86eax); X86Setne re_buf; X86Mov (re_buf, t)]
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
  (X86Mov (x86esp, x86ebp))::x86compile' [] code

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
    | X86Add (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tSUBL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86And (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Or  (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tORL  \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Xor (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tXORL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mov (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tMOVL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp (o1, o2) -> Buffer.add_string b @@ Printf.sprintf "\tCMPL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Div   o1     -> Buffer.add_string b @@ Printf.sprintf "\tIDIVL\t%s\n" (pr_op o1)
    | X86Setl  o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETL \t%s\n" (pr_op8 o1)
    | X86Setle o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETLE\t%s\n" (pr_op8 o1)
    | X86Setg  o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETG \t%s\n" (pr_op8 o1)
    | X86Setge o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETGE\t%s\n" (pr_op8 o1)
    | X86Sete  o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETE \t%s\n" (pr_op8 o1)
    | X86Setne o1     -> Buffer.add_string b @@ Printf.sprintf "\tSETNE\t%s\n" (pr_op8 o1)
    | X86Push o1      -> Buffer.add_string b @@ Printf.sprintf "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop  o1      -> Buffer.add_string b @@ Printf.sprintf "\tPOPL \t%s\n" (pr_op o1)
    | X86Ret          -> Buffer.add_string b @@ Printf.sprintf "\tRET  \n"
    | X86Cdq          -> Buffer.add_string b @@ Printf.sprintf "\tCDQ  \n"
    | X86Call s       -> Buffer.add_string b @@ Printf.sprintf "\tCALL \t%s\n" s
  ) code;
  Buffer.add_string b @@ Printf.sprintf "\tXORL\t%s,\t%s\n\tRET\n\n" (pr_op x86eax) (pr_op x86eax)

let print_compiled: stmt -> string = fun stmt ->
  let buffer = Buffer.create 1024 in
  let asm = x86compile (compile_stmt stmt) in
  let vars = collect_vars stmt in
  Buffer.add_string buffer "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  print_code asm buffer;
  SS.iter (fun var ->
    Buffer.add_string buffer @@ Printf.sprintf "\t.comm %s 4\n" var
  ) vars;
  Buffer.contents buffer

let build: stmt -> string -> int = fun stmt file ->
  let outf = open_out (Printf.sprintf "%s.s" file) in
  Printf.fprintf outf "%s" (print_compiled stmt);
  close_out outf;
  Sys.command (Printf.sprintf "gcc -o %s runtime/runtime.o %s.s" file file)

