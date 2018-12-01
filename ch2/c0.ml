open R1

type argument =
  | IntArg of int
  | VarArg of string

and expression =
  | ArgExp of argument
  | ReadExp
  | NegExp of argument
  | AddExp of argument * argument

and statement =
  | AssignStmt of argument * expression

and tail =
  | ReturnTail of expression
  | SeqTail of statement * tail

and label =
  | EndLabel of string * tail
  | ManyLabel of string * tail * label

type info = string * (argument list)
               
type program =
  | AProgram of info * label

exception R1ToC0Error of string

(* Convert R1.expression to C0.expression *)
let r1_to_argument r1_exp =
  match r1_exp with
  | R1.IntExp (num, _) -> IntArg num
  | R1.VarExp (str, _) -> VarArg str
  | _ -> raise (R1ToC0Error "r1_to_argument fails")
                       
let r1_to_expression r1_exp =
  match r1_exp with
  | R1.IntExp _ -> ArgExp (r1_to_argument r1_exp)
  | R1.VarExp _ -> ArgExp (r1_to_argument r1_exp)
  | R1.ReadExp _ -> ReadExp
  | R1.NegExp (exp, _) -> NegExp (r1_to_argument exp)
  | R1.AddExp (exp1, exp2, _) ->
     AddExp (r1_to_argument exp1, r1_to_argument exp2)
  | _ -> raise (R1ToC0Error "r1_to_expression fails")
              
let rec r1_to_tail exp =
  match exp with
  | R1.IntExp _ -> ReturnTail (r1_to_expression exp)
  | R1.ReadExp _ -> ReturnTail (r1_to_expression exp)
  | R1.NegExp _ -> ReturnTail (r1_to_expression exp)
  | R1.AddExp _ -> ReturnTail (r1_to_expression exp)
  | R1.VarExp _ -> ReturnTail (r1_to_expression exp)
  | R1.LetExp (str, exp, body, _) ->
     let assign_stmt = AssignStmt (VarArg str, r1_to_expression exp) in
     SeqTail (assign_stmt, r1_to_tail body)

let r1prog_to_prog prog =
  match prog with
  | R1.AProgram (i, exp) ->
    AProgram (("locals", []), EndLabel ("start", r1_to_tail exp)) 

(* Uncover all the assigned variables and put them in the 
   info section of the program  *)                     
let rec uncover_locals_in_tail t accum =
  match t with
  | ReturnTail _ -> accum
  | SeqTail (AssignStmt (arg, exp), t) -> uncover_locals_in_tail t (arg :: accum)

let rec uncover_locals_in_label l accum =
  match l with
  | EndLabel (_, t) -> List.rev (uncover_locals_in_tail t accum)
  | ManyLabel (_, t, l) -> uncover_locals_in_label l (uncover_locals_in_tail t accum)
                         
let uncover_locals (AProgram (i, l)) =
  let locals_ls = uncover_locals_in_label l [] in
  AProgram (("locals", locals_ls), l)

(* Convert C0.expression to string for print purpose *)
let string_of_arg arg =
  match arg with
  | IntArg num -> string_of_int num
  | VarArg str -> str

let rec string_of_exp exp =
  match exp with
  | ArgExp arg -> string_of_arg arg
  | ReadExp -> "read"
  | NegExp arg -> "(- " ^ string_of_arg arg ^ ")"
  | AddExp (arg1, arg2) -> "(+ " ^ string_of_arg arg1 ^ " " ^ string_of_arg arg2 ^ ")"

let string_of_stmt stmt =
  match stmt with
  | AssignStmt (arg, exp) -> "(assign " ^ string_of_arg arg ^ " " ^ string_of_exp exp ^ ")"

let rec string_of_tail t accum =
  match t with
  | ReturnTail exp -> accum ^ "return " ^ string_of_exp exp
  | SeqTail (stmt, t) -> string_of_tail t (accum ^ (string_of_stmt stmt ^ "\n"))
                       
let rec string_of_label l =
  match l with
  | EndLabel (str, t) -> "(" ^ str ^ " .\n" ^ string_of_tail t "" ^ ")"
  | ManyLabel (str, t, l) ->
     "(" ^ str ^ " .\n" ^ string_of_tail t "" ^ ")\n" ^ string_of_label l

let string_of_info (str, arg_ls) =
  let rec go arg_ls accum =
    match arg_ls with
    | [] -> accum
    | hd :: tl ->
       match hd with
       | IntArg num -> go tl (accum ^ " " ^ string_of_int num)
       | VarArg str -> go tl (accum ^ " " ^ str)
  in
  let res = go arg_ls "" in
  match res with
  | "" -> "()"
  | _ -> "(" ^ str ^ " ." ^ res ^ ")"
       
let string_of_program p =
  match p with
  | AProgram (i, l) -> "(program " ^ string_of_info i ^ "\n(" ^ string_of_label l ^ "))"
