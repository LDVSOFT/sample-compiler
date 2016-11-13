open Language
open Expr
open Stmt
open StackMachine
open Utils

let x86regs = [|"%esp"; "%ebp"; "%eax"; "%edx"; "%ebx"; "%ecx"; "%esi"; "%edi"|]
let x86regs8 = [|"spl"; "%bpl"; "%al"; "%dl"; "%bl"; "%cl"; "%sil"; "%dil"|]
let num_of_regs = Array.length x86regs
let word_size = 4

type opnd =
| R of int    (* register*)
| S of int    (* stack ( lower EBP ) *)
| M of string (* mark *)
| L of int    (* const *)

let x86esp = R (Utils.find x86regs "%esp")
let x86ebp = R (Utils.find x86regs "%ebp")
let x86eax = R (Utils.find x86regs "%eax")
let x86edx = R (Utils.find x86regs "%edx")

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
  | X86_Comm  of string

type state_t = {
  stack: opnd list;
  vars: (string * opnd) list;
  locals: int
}

let allocate state =
  let one = match state.stack with
  | []                              -> R 4 (* preserve esp, ebp, eax and ebx and for stuff :) *)
  | (S n)::_                        -> S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S state.locals
  in (one, {state with stack = one::state.stack})

let free state =
  let (x, stack') = cut_head state.stack in
  (x, {state with stack = stack'})

let x86compile (code: i list): x86instr list =
  let x86addStackN n = [X86Sub (L (word_size * n), x86esp)] in
  let x86subStackN n = [X86Add (L (word_size * n), x86esp)] in
  let x86addStack s =
    match s with
    | S _ -> x86addStackN 1
    | _   -> []
  in
  let x86subStack s =
    match s with
    | S _ -> x86subStackN 1
    | _   -> []
  in
  let op_buf = x86eax in
  let re_buf = x86edx in
  let protect t t' f = match t with
  | R x -> f t
  | _ -> [X86Mov (t, t')] @ (f t') @ [X86Mov (t', t)]
  in
  let rec x86compile' (state: state_t) (code: i list) =
    match code with
    | []       -> []
    | i::code' ->
    let (state', x86code) =
      match i with
      | S_COMM comm ->
        (state, [
          X86_Comm (Printf.sprintf "Stack: %s" comm)
        ])
      | S_LABEL label ->
        (state, [
          X86_Label label
        ])
      | S_JMP label ->
        assert (state.stack = []);
        (state, [
          X86Jmp label
        ])
      | S_JIF label ->
        let (x, state') = free state in
        assert (state'.stack = []);
        (state', [
          X86Mov (x, x86eax)] @
          x86subStack x @ [
          X86Cmp (L 0, x86eax);
          X86Jne label
        ])
      | S_CALL (func, argcnt) ->
        let (args, stack') = Utils.list_cut state.stack argcnt in
        let state' = {state with stack = stack'} in
        let args_on_stack = List.length @@ List.filter (function | S _ -> true | _ -> false) args in
        let volatiles = List.filter (function | R _ -> true | _ -> false) state'.stack in
        let (s, state'') = allocate state' in
        (state'',
          List.map (fun o -> X86Push o) volatiles @
          List.map (fun o -> X86Push o) args @ [
          X86Call func;
          X86Mov (x86eax, s)] @
          x86subStackN argcnt @
          List.map (fun o -> X86Pop o) (List.rev volatiles) @
          x86subStackN args_on_stack @
          x86addStack s
        )
      | S_ENTER (args, local_var) ->
        assert(state.stack = []);
        let local_loc = List.mapi (fun i a -> (a, S i)) local_var in
        let local_arg = List.mapi (fun i a -> (a, S (-3 - i))) args in
        ({stack = []; vars = local_arg @ local_loc; locals = List.length local_var}, [
          X86Push x86ebp;
          X86Mov (x86esp, x86ebp)] @
          x86addStackN (List.length local_var)
        )
      | S_RET ->
        let (x, state') = free state in
        let () = assert (state'.stack = []) in
        (state', [
          X86Mov (x, x86eax) ] @
          x86subStack x @
          x86subStackN state.locals @ [
          X86Pop x86ebp;
          X86Ret
        ])
      | S_READ   ->
        let (s, state') = allocate state in
        (state', [
          X86Call "read";
          X86Mov (x86eax, s)] @
          x86addStack s
        )
      | S_WRITE  ->
        let (s, state') = free state in
        (state', [
          X86Push s;
          X86Call "write"] @
          x86subStackN 1 @
          x86subStack s
        )
      | S_PUSH (Int n) ->
        let (s, state') = allocate state in
        (state', [
          X86Mov (L n, s)] @
          x86addStack s
        )
      | S_POP ->
        let (s, state') = free state in
        (state',
          x86subStack s
        )
      | S_LD x ->
        let (s, state') = allocate state in
        (state',
          protect s x86eax (fun s' -> [
            X86Mov (List.assoc x state.vars, s')
          ]) @
          x86addStack s
        )
      | S_ST x ->
        let (s, state') = free state in
        (state',
          protect s x86eax (fun s' -> [
            X86Mov (s', List.assoc x state.vars)
          ]) @
          x86subStack s
        )
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
        let (y, state1) = free state  in
        let (x, state2) = free state1 in
        let (x, state1) = allocate state2 in (*sanity check*)
        (state1, (
          match op with
          | "+" | "-" | "*" | "&&" | "!!" -> (
            let checkBool t = match op with
            | "&&" | "!!" -> [
                X86Mov (t, x86eax);
                X86Mov (L 0, re_buf);
                X86Cmp (L 0, x86eax);
                X86SetCC ("NE", re_buf);
                X86Mov (re_buf, t)
              ]
            | _ -> []
            in
            checkBool x @
            checkBool y @
            protect x op_buf (fun x' -> [
              x86compile_op2 op y x'
            ])
          )
          | "<" | "<=" | ">" | ">=" | "==" | "!=" -> (
            protect x op_buf (fun x' -> [
              X86Mov (L 0, re_buf);
              X86Cmp (y, x');
              x86compile_op1 op re_buf;
              X86Mov(re_buf, x')
            ])
          )
          | "/" | "%" -> (
            let res = match op with
              | "/" -> x86eax
              | "%" -> x86edx
            in
            [
              X86Mov (x, x86eax);
              X86Cdq;
              X86Div y;
              X86Mov (res, x)
            ])
        ) @ x86subStack y)
      )
   in
   x86code @ x86compile' state' code'
  in
  x86compile' {stack = []; vars = []; locals = 0} code

let print_code code b =
  let rec pr_op opnd =
    match opnd with
    | R n -> x86regs.(n)
    | S o -> Printf.sprintf "%d(%s)" ((-4) * (o + 1)) (pr_op x86ebp)
    | M s -> s
    | L n -> Printf.sprintf "$%d" n
  in
  let rec pr_op8 opnd =
    match opnd with
    | R n -> x86regs8.(n)
    | S o -> Printf.sprintf "%d(%s)" ((-4) * (o + 1)) (pr_op x86ebp)
    | M s -> s
    | L n -> Printf.sprintf "$%d" n
  in

  List.iter (fun instr ->
    match instr with
    | X86Add (o1, o2)  -> Printf.bprintf b "\tADDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Sub (o1, o2)  -> Printf.bprintf b "\tSUBL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mul (o1, o2)  -> Printf.bprintf b "\tIMULL\t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86And (o1, o2)  -> Printf.bprintf b "\tANDL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Or  (o1, o2)  -> Printf.bprintf b "\tORL  \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Xor (o1, o2)  -> Printf.bprintf b "\tXORL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Mov (o1, o2)  -> Printf.bprintf b "\tMOVL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Cmp (o1, o2)  -> Printf.bprintf b "\tCMPL \t%s,\t%s\n" (pr_op o1) (pr_op o2)
    | X86Div   o1      -> Printf.bprintf b "\tIDIVL\t%s\n" (pr_op o1)
    | X86SetCC (s, o1) -> Printf.bprintf b "\tSET%s\t%s\n" s (pr_op8 o1)
    | X86Push o1       -> Printf.bprintf b "\tPUSHL\t%s\n" (pr_op o1)
    | X86Pop  o1       -> Printf.bprintf b "\tPOPL \t%s\n" (pr_op o1)
    | X86Ret           -> Printf.bprintf b "\tRET  \n"
    | X86Cdq           -> Printf.bprintf b "\tCDQ  \n"
    | X86Call s        -> Printf.bprintf b "\tCALL \t%s\n" s
    | X86Jmp  s        -> Printf.bprintf b "\tJMP  \t%s\n" s
    | X86Jne s         -> Printf.bprintf b "\tJNE  \t%s\n" s
    | X86_Label s -> Printf.bprintf b "%s:\n" s
    | X86_Comm  s -> Printf.bprintf b "\t#%s\n" s
  ) code

let print_compiled (p: Program.t): string =
  let buffer = Buffer.create 1024 in
  let asm = x86compile (StackMachine.Compile.program p) in
  Buffer.add_string buffer "\t.extern read\n\t.extern write\n\t.global main\n\n\t.text\n";
  print_code asm buffer;
  Buffer.contents buffer

let build (file: string) (p: Program.t): unit =
  let outf = open_out (Printf.sprintf "%s.s" file) in
  let runtime_dir = try
    Sys.getenv "RC_RUNTIME"
    with Not_found -> "../runtime"
  in
  Printf.fprintf outf "%s" (print_compiled p);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s %s/runtime.o %s.s" file runtime_dir file) with
  | 0 -> ()
  | _ -> failwith "gcc failed!"
