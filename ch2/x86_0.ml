open C0

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

type arg =
  | Int of int
  | Reg of register
  | Deref of register * int
  | Var of string

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

type block =
  | ABlock of info * (instr list)

type program =
  | AProgram of info * ((label * block) list)

exception X86_0_Exception of string              

(* Select Instructions  *)
let to_x86_arg a =
  match a with
  | C0.IntArg num -> Int num
  | C0.VarArg str -> Var str
                           
let c0_statement_to_instr_ls (C0.AssignStmt (arg, exp)) =
  match arg with
  | C0.IntArg _ -> raise (X86_0_Exception "Invalid C0 statement")
  | C0.VarArg var_name ->
     match exp with
     | C0.ArgExp arg -> [Movq (to_x86_arg arg, Var var_name)]  
     | C0.ReadExp ->
        [Callq "read_int"; Movq (Reg RAX, Var var_name)]
     | C0.NegExp arg ->
        [Movq (to_x86_arg arg, Var var_name); Negq (Var var_name)]
     | C0.AddExp (arg1, arg2) ->
        [Movq (to_x86_arg arg1, Var var_name); Addq (to_x86_arg arg2, Var var_name)]

let rec c0_tail_to_instr_helper t accum =
  match t with
  | ReturnTail exp ->
     (match exp with
      | C0.ArgExp a -> List.rev_append [Movq (to_x86_arg a, Reg RAX)] accum
      | C0.ReadExp -> List.rev_append [Callq "read_int"] accum
      | C0.NegExp a -> List.rev_append [Movq (to_x86_arg a, Reg RAX); Negq (Reg RAX)] accum
      | C0.AddExp (arg1, arg2) ->
         List.rev_append
           [Movq (to_x86_arg arg1, Reg RAX); Addq (to_x86_arg arg2, Reg RAX)] accum)
  | SeqTail (stmt, t) ->
     c0_tail_to_instr_helper t (List.rev_append (c0_statement_to_instr_ls stmt) accum)

let c0_tail_to_instr_ls final_instr t =
  let accum_ls = c0_tail_to_instr_helper t [] in
  List.rev_append accum_ls [final_instr]

(* Assign Homes *)

let gen_pos arg_ls = 
  let rec do_gen_pos arg_ls pos accum =
    match arg_ls with
    | [] -> List.rev accum
    | hd :: tl ->
       match hd with
       | C0.IntArg _ -> raise (X86_0_Exception "impossible")
       | C0.VarArg str -> do_gen_pos tl (pos+1) ((str, (-8)*pos) :: accum) in
  do_gen_pos arg_ls 1 []
  
let rec search_pos ls name =
  match ls with
  | [] -> raise (X86_0_Exception "impossible")
  | (str, pos) :: tl -> if name = str then pos else search_pos tl name

let replace_arg kv_ls arg =
  match arg with
  | Int _ -> arg
  | Reg _ -> arg
  | Deref _ -> arg
  | Var str -> Deref (RBP, search_pos kv_ls str)

let assign_homes_for_instr kv_ls instr =
  match instr with
  | Addq (arg1, arg2) -> Addq (replace_arg kv_ls arg1, replace_arg kv_ls arg2)
  | Movq (arg1, arg2) -> Movq (replace_arg kv_ls arg1, replace_arg kv_ls arg2)
  | Negq arg -> Negq (replace_arg kv_ls arg)
  | Jmp _ -> instr
  | Callq _ -> instr
  | _ -> raise (X86_0_Exception "unknown instruction")
 
let rec assign_homes_for_instr_ls kv_ls instr_ls =
  let res = List.fold_left
              (fun accum instr ->
                (assign_homes_for_instr kv_ls instr) :: accum)
              [] instr_ls
  in
  List.rev res

let rec assign_homes_for_tail kv_ls tail =
  let instr_ls = c0_tail_to_instr_ls (Jmp "conclusion") tail in
  assign_homes_for_instr_ls kv_ls instr_ls

(* Patch instructions  *)

let need_patch arg1 arg2 =
  match (arg1, arg2) with
  | (Deref _, Deref _) -> true
  | _ -> false
  
let patch_instruction instr =
  match instr with
  | Addq (arg1, arg2) ->
     if need_patch arg1 arg2 then
       [Movq (arg1, Reg RAX); Addq (Reg RAX, arg2)]
     else [instr]
  | Subq (arg1, arg2) ->
     if need_patch arg1 arg2 then
       [Movq (arg1, Reg RAX); Subq (Reg RAX, arg2)]
     else [instr]
  | Movq (arg1, arg2) ->
     if need_patch arg1 arg2 then
       [Movq (arg1, Reg RAX); Movq (Reg RAX, arg2)]
     else [instr]
  | _ -> [instr]
  
let patch_instrs instr_ls =
  let res = List.fold_left
              (fun accum instr ->
                List.rev_append (patch_instruction instr) accum)
              []
              instr_ls
  in
  List.rev res

(* Printing *)
                      
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

let string_of_arg arg =
  match arg with
  | Int num -> "(int " ^ string_of_int num
  | Reg reg -> "(reg " ^ string_of_register reg
  | Deref (reg, num) -> "(deref " ^ string_of_register reg ^ " " ^ string_of_int num ^ ")"
  | Var str -> "(var " ^ str ^ ")"

let string_of_instr instr =
  match instr with
  | Addq (arg1, arg2) -> "(addq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Movq (arg1, arg2) -> "(movq " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"
  | Negq arg -> "(negq " ^ string_of_arg arg ^ ")"
  | Jmp label -> "(jmp " ^ label ^ ")"
  | Callq label -> "(callq " ^ label
  | _ -> raise (X86_0_Exception "unknown instruction")

let rec string_of_instr_ls ls accum =
  match ls with
  | [] -> accum
  | hd :: tl -> string_of_instr_ls tl (accum ^ string_of_instr hd ^ "\n") 
       
let show_instr_for_c0_prog (C0.AProgram ((str, arg_ls), ls))=
  let kv_ls = gen_pos arg_ls in
  List.fold_left
    (fun accum (l_str, tail) ->
      accum ^
        l_str ^
          ":\n" ^
            (string_of_instr_ls (patch_instrs (assign_homes_for_tail kv_ls tail)) "")
    )
    ""
    ls
     
(* Print X86  *)
let print_arg arg =
  match arg with
  | Int num -> "$" ^ string_of_int num
  | Reg reg -> "%" ^ string_of_register reg
  | Deref (reg, num) -> string_of_int num ^ "(%" ^ string_of_register reg ^ ")"
  | Var _ -> raise (X86_0_Exception "Invalid arg")

let print_instr instr =
  match instr with
  | Addq (arg1, arg2) -> "addq " ^ print_arg arg1 ^ ", " ^ print_arg arg2
  | Subq (arg1, arg2) -> "subq " ^ print_arg arg1 ^ ", " ^ print_arg arg2
  | Movq (arg1, arg2) -> "movq " ^ print_arg arg1 ^ ", " ^ print_arg arg2
  | Retq -> "retq"
  | Negq arg -> "negq " ^ print_arg arg
  | Callq label -> "callq " ^ label
  | Pushq arg -> "pushq " ^ print_arg arg
  | Popq arg -> "popq " ^ print_arg arg
  | Jmp label -> "jmp " ^ label

                            
let print_label label instr_ls =
  List.fold_left
    (fun accum instr ->
      accum ^ print_instr instr ^ "\n")
    (label ^ ":\n")
    instr_ls
    
let print_main arg_ls start_label =
  let sp_sub =
    let len = List.length arg_ls in
    if (len mod 2) = 0
    then len else len + 1 in
  ".global main\n" ^
    "main:\n" ^
      "pushq %rbq\n" ^
        "movq %rsp, %rbp\n" ^
          "subq $" ^ string_of_int (sp_sub*8) ^ ", %rsp\n" ^
            "jmp " ^ start_label ^ "\n"

let print_conclusion arg_ls =
  let sp_sub =
    let len = List.length arg_ls in
    if (len mod 2) = 0
    then len else len + 1 in
  "addq " ^ string_of_int (sp_sub*8) ^ ", %rsp\n" ^
    "popq %rbp\n" ^
      "retq\n"

let print_program (C0.AProgram ((_, arg_ls), lt_ls)) =
  let (label, tail) = List.hd lt_ls in
  let kv_ls = gen_pos arg_ls in
  let instr_ls =
    tail |> c0_tail_to_instr_ls (Jmp "conclusion") |> assign_homes_for_instr_ls kv_ls
    |> patch_instrs in
  (print_label label instr_ls) ^ (print_main arg_ls label) ^ (print_conclusion arg_ls) 
