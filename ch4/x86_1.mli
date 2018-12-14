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

val c1_stls_to_x86_instr_ls : (C1.stmt list) -> (instr list)

val string_of_instr_ls : (instr list) -> string                                                  
