open R2

type arg =
  | IntArg of int
  | VarArg of string
  | BoolArg of bool

type cmp = R2.cmp
             
type exp =
  | ArgExp of arg
  | ReadExp
  | NegExp of arg
  | AddExp of arg * arg
  | NotExp of arg
  | CmpExp of cmp * arg * arg
                            
type stmt =
  | AssignStmt of string * exp
  | ReturnStmt of arg
  | IfStmt of cmp * arg * arg * (stmt list) * (stmt list)
            
type program =
  | AProgram of (string list) * R2.r2_type * (stmt list)
              
(* Flatten an R2 expression  *)

let rec flatten exp bind_name cont =
  match exp with
  | R2.IntExp (num, _) -> AssignStmt (bind_name, ArgExp (IntArg num)) :: cont
  | R2.ReadExp _ -> AssignStmt (bind_name, ReadExp) :: cont
  | R2.NegExp (exp, _) ->
     flatten exp bind_name (AssignStmt (bind_name, NegExp (VarArg bind_name)) :: cont)
  | R2.AddExp (exp1, exp2, _) ->
     let exp1_id = R2.gen_id "tmp" in
     let exp2_id = R2.gen_id "tmp" in
     let exp2_cont = AssignStmt (bind_name, AddExp (VarArg exp1_id, VarArg exp2_id)) :: cont in
     let exp1_cont = flatten exp2 exp2_id exp2_cont in
     flatten exp1 exp1_id exp1_cont
  | R2.VarExp (str, _) -> AssignStmt (bind_name, ArgExp (VarArg str)) :: cont
  | R2.LetExp (str, exp, body, _) ->
     flatten exp str (flatten body bind_name cont)
  | R2.TrueExp _ -> AssignStmt (bind_name, ArgExp (BoolArg true)) :: cont
  | R2.FalseExp _ -> AssignStmt (bind_name, ArgExp (BoolArg false)) :: cont
  | R2.AndExp (exp1, exp2, _) ->
     let inner = R2.IfExp (exp2, TrueExp Ploc.dummy, FalseExp Ploc.dummy, Ploc.dummy) in
     let outer = R2.IfExp (exp1, inner, FalseExp Ploc.dummy, Ploc.dummy) in
     flatten outer bind_name cont
  | R2.NotExp (exp, _) ->
     let exp_id = R2.gen_id "tmp" in
     flatten exp exp_id (AssignStmt (bind_name, NotExp (VarArg exp_id)) :: cont)
  | R2.CmpExp (cmp, exp1, exp2, _) ->
     let exp1_id = R2.gen_id "tmp" in
     let exp2_id = R2.gen_id "tmp" in
     let exp2_cont =
       AssignStmt (bind_name, CmpExp (cmp, VarArg exp1_id, VarArg exp2_id)) :: cont in
     let exp1_cont = flatten exp2 exp2_id exp2_cont in
     flatten exp1 exp1_id exp1_cont
  | R2.IfExp (exp1, exp2, exp3, _) ->
     let exp1_id = R2.gen_id "tmp" in
     let exp2_flatten = flatten exp2 bind_name [] in
     let exp3_flatten = flatten exp3 bind_name [] in
     let exp1_cont =
       IfStmt (R2.EQ, VarArg exp1_id, BoolArg true, exp2_flatten, exp3_flatten) :: cont in
     flatten exp1 exp1_id exp1_cont
                                                                                     
let do_flatten exp =
  let exp_id = gen_id "tmp" in
  flatten exp exp_id [ReturnStmt (VarArg exp_id)]

(* Convert statment list to string *)
let string_of_arg arg =
  match arg with
  | IntArg num -> string_of_int num
  | VarArg var -> var
  | BoolArg b ->
     match b with
     | true -> "#t"
     | false -> "#f"

let string_of_cmp cmp =
  match cmp with
  | R2.EQ -> "eq?"
  | R2.L -> "<"
  | R2.LE -> "<="
  | R2.G -> ">"
  | R2.GE -> ">="

let string_of_exp exp =
  match exp with
  | ArgExp arg -> string_of_arg arg
  | ReadExp -> "(read)"
  | NegExp arg -> "(- " ^ (string_of_arg arg) ^ ")"
  | AddExp (arg1, arg2) -> "(+ " ^ (string_of_arg arg1) ^ " " ^ (string_of_arg arg2) ^ ")"
  | NotExp arg -> "(not " ^ (string_of_arg arg) ^ ")"
  | CmpExp (cmp, arg1, arg2) ->
     "(" ^ (string_of_cmp cmp) ^ " " ^ (string_of_arg arg1) ^ " " ^ (string_of_arg arg2) ^ ")"
  
let string_of_stmt_list stmt_ls =
  let rec recur stmt_ls accum =
    match stmt_ls with
    | [] -> accum
    | hd :: tl ->
       let ending =
         match tl with
         | [] -> ""
         | _ -> "\n" in
       match hd with
       | AssignStmt (str, exp) ->
          recur tl
            (accum ^ "(assign " ^ str ^ " " ^ (string_of_exp exp) ^ ")" ^ ending)
       | ReturnStmt (arg) ->
          recur tl
            (accum ^ "(return " ^ (string_of_arg arg) ^ ")" ^ ending)
       | IfStmt (cmp, arg1, arg2, stmt_ls1, stmt_ls2) ->
          let then_part = recur stmt_ls1 "" in
          let else_part = recur stmt_ls2 "" in
          recur tl
            (accum ^
               "(if (" ^ (string_of_cmp cmp) ^ " " ^ (string_of_arg arg1) ^ " " ^ (string_of_arg arg2) ^ ")\n" ^
                 "(" ^ then_part ^ ")\n" ^
                   "(" ^ else_part ^ ")" ^ ending)
  in
  recur stmt_ls ""
              
    
