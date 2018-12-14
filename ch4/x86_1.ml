open R2
open C1

type label = string

type info = unit           

type register =
  | RSP
  | RBP
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | RAL

type arg =
  | Int of int
  | Reg of register
  | Deref of register * int
  | Bytereg of register
  | Var of string

type cc = C1.cmp
         
type instr =
  | Addq of arg * arg
  | Subq of arg * arg
  | Movq of arg * arg
  | Retq
  | Negq of arg
  | Callq of label
  | Pushq of arg
  | Popq of arg
  | Jmp of label
  | Xorq of arg * arg
  | Cmpq of arg * arg
  | Set of cc * arg
  | Movzbq of arg * arg
  | Jmpif of cc * label
  | Label of label

type block =
  | ABlock of info * (instr list)

type program =
  | AProgram of info * ((label * block) list)

let unique_id = ref 0

let gen_id var =
  unique_id := (!unique_id + 1);
  var ^ (string_of_int !unique_id)
              
let c1_arg_to_x86_arg arg =
  match arg with
  | C1.IntArg num -> Int num
  | C1.VarArg str -> Var str
  | C1.BoolArg b ->
     if b then Int 1 else Int 0
            
let rec c1_stmt_to_x86_instr_ls stmt =
  match stmt with
  | C1.ReturnStmt arg ->
     [Movq (c1_arg_to_x86_arg arg, Reg RAX)]
  | C1.AssignStmt (str, exp) -> 
     (match exp with
      | C1.ArgExp arg -> [Movq (c1_arg_to_x86_arg arg, Var str)]
      | C1.ReadExp -> [Callq "read_int"; Movq (Reg RAX, Var str)]
      | C1.NegExp arg -> [Movq (c1_arg_to_x86_arg arg, Var str); Negq (Var str)]
      | C1.AddExp (arg1, arg2) ->
         [Movq (c1_arg_to_x86_arg arg1, Var str); Addq (c1_arg_to_x86_arg arg2, Var str)]

      | C1.NotExp arg1 ->
         [Movq (c1_arg_to_x86_arg arg1, Var str); Xorq (Int 1, Var str)]
      | C1.CmpExp (cmp, arg1, arg2) ->
         [Cmpq (c1_arg_to_x86_arg arg2, c1_arg_to_x86_arg arg1);
          Set (cmp, Bytereg RAL);
          Movzbq (Bytereg RAL, Var str)])
  | C1.IfStmt (cmp, arg1, arg2, ls1, ls2) ->
     let then_label = gen_id "then" in
     let end_label = gen_id "if_end" in
     [Cmpq (c1_arg_to_x86_arg arg2, c1_arg_to_x86_arg arg1);
      Jmpif (cmp, then_label);] @
       c1_stls_to_x86_instr_ls ls2 @
         [Jmp end_label; Label then_label] @
           c1_stls_to_x86_instr_ls ls1 @
             [Label end_label]
               
and c1_stls_to_x86_instr_ls stls =
  let res = List.fold_left
              (fun accum stmt -> List.rev_append (c1_stmt_to_x86_instr_ls stmt) accum)
              [] stls in
  List.rev res
     
let string_of_register reg =
  match reg with
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RAX -> "rax"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
  | RAL -> "RAL"
         
let string_of_arg arg =
  match arg with
  | Int num -> "(int " ^ string_of_int num ^ ")"
  | Reg reg -> "(reg " ^ string_of_register reg ^ ")"
  | Deref (reg, num) -> "(deref " ^ string_of_register reg ^ " " ^ string_of_int num ^ ")"
  | Var str -> "(var " ^ str ^ ")"
  | Bytereg reg -> "(bytereg " ^ string_of_register reg ^ ")"

let string_of_cc cc =
  match cc with
  | R2.EQ -> "e"
  | R2.L -> "l"
  | R2.LE -> "le"
  | R2.G -> "g"
  | R2.GE -> "ge"     

let string_of_instr instr =
  match instr with
  | Addq (arg1, arg2) -> "(addq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Movq (arg1, arg2) -> "(movq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Negq arg -> "(negq " ^ string_of_arg arg ^ ")"
  | Jmp label -> "(jmp " ^ label ^ ")"
  | Callq label -> "(callq " ^ label ^ ")"
  | Subq (arg1, arg2) -> "(subq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Pushq arg -> "(pushq " ^ string_of_arg arg ^ ")"
  | Popq arg -> "(popq " ^ string_of_arg arg ^ ")"
  | Xorq (arg1, arg2) -> "(xorq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Cmpq (arg1, arg2) -> "(cmpq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Set (cc, arg) -> "(set " ^ string_of_cc cc ^ " " ^ string_of_arg arg ^ ")"
  | Movzbq (arg1, arg2) -> "(movzbq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Jmpif (cc, label) -> "(jmpif " ^ string_of_cc cc ^ " " ^ label ^ ")"
  | Label label -> "(label " ^ label ^ ")"
  | Retq -> "(retq)"

let string_of_instr_ls ls =           
  let rec recur ls accum =
    match ls with
    | [] -> accum
    | hd :: tl -> recur tl (accum ^ string_of_instr hd ^ "\n") 
  in
  recur ls ""
