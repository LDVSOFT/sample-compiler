module SS = Set.Make(String)

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq

let eval_op op l r =
  let toi x = if x then 1 else 0 in
  match op with
  | Add -> l + r
  | Sub -> l - r
  | Mul -> l * r
  | Div -> l / r
  | Mod -> l mod r
  | And -> toi ((l != 0) && (r != 0))
  | Or  -> toi ((l != 0) || (r != 0))
  | Lt  -> toi (l < r)
  | Gt  -> toi (l > r)
  | Le  -> toi (l <= r)
  | Ge  -> toi (l >= r)
  | Eq  -> toi (l == r)
  | Neq -> toi (l != r)

type expr =
  | Const of int
  | Var   of string
  | Op    of binop * expr * expr

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
  | S_OP    of binop

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

let x86compile_op2 op l r = match op with
  | Add -> X86Add (l, r)
  | Sub -> X86Sub (l, r)
  | Mul -> X86Mul (l, r)
  | And -> X86And (l, r)
  | Or  -> X86Or  (l, r)

let x86compile_op1 op l = match op with
  | Lt  -> X86Setl  l
  | Le  -> X86Setle l
  | Gt  -> X86Setg  l
  | Ge  -> X86Setge l
  | Eq  -> X86Sete  l
  | Neq -> X86Setne l

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
        let op_buf = x86eax in
        let re_buf = x86edx in
        match op with
        | Add | Sub | Mul | And | Or -> (
          let y::x::stack' = stack in
          let perform t = [x86compile_op2 op y t] in
          match x with
          | R _ -> (x::stack', perform x @ x86subStack y)
          | _   -> (x::stack', [X86Mov (x, op_buf)] @ perform op_buf @ [X86Mov (op_buf, x)] @ x86subStack y)
        )
        | Lt | Le | Gt | Ge | Eq | Neq -> (
          let y::x::stack' = stack in
          let perform t = [X86Xor (re_buf, re_buf); X86Cmp (y, t); x86compile_op1 op re_buf; X86Mov (re_buf, t)] in
          match x with
          | R _ -> (x::stack', perform x @ x86subStack y)
          | _   -> (x::stack', [X86Mov (x, op_buf)] @ perform op_buf @ [X86Mov (op_buf, x)] @ x86subStack y)
        )
        | Div | Mod -> (
          let y::x::stack' = stack in
          let res = match op with
            | Div -> x86eax
            | Mod -> x86edx
          in
          (x::stack', [X86Mov (x, x86eax); X86Cdq; X86Div y; X86Mov (res, x)] @ x86subStack y)
        )
      )
   in
   x86code @ x86compile' stack' code'
  in
  (X86Mov (x86esp, x86ebp))::x86compile' [] code

