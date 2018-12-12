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
let is_exp_simple exp =
  match exp with
  | R2.IntExp _ -> true
  | R2.VarExp _ -> true
  | R2.TrueExp _ -> true
  | R2.FalseExp _ -> true
  | _ -> false

exception TmpException

let simple_exp_to_arg exp =
  match exp with
  | R2.IntExp (num, _) -> IntArg num
  | R2.VarExp (str, _) -> VarArg str
  | R2.TrueExp _ -> BoolArg true
  | R2.FalseExp _ -> BoolArg false
  | _ -> raise TmpException

let replace_var_in_arg arg ori_name tar_name =
  match arg with
  | IntArg _ -> arg
  | VarArg str ->
     if str = ori_name
     then VarArg tar_name else arg
  | BoolArg _ -> arg
       
let rec replace_var_in_stmt stmt ori_name tar_name =
  match stmt with
  | AssignStmt (str, exp) ->
     let res_exp = (match exp with
                    | ArgExp arg -> ArgExp (replace_var_in_arg arg ori_name tar_name)
                    | ReadExp -> ReadExp
                    | NegExp arg -> NegExp (replace_var_in_arg arg ori_name tar_name)
                    | AddExp (arg1, arg2) ->
                       AddExp ((replace_var_in_arg arg1 ori_name tar_name),
                               (replace_var_in_arg arg2 ori_name tar_name))
                    | NotExp arg -> NotExp (replace_var_in_arg arg ori_name tar_name)
                    | CmpExp (cmp, arg1, arg2) -> 
                       CmpExp (cmp,
                               (replace_var_in_arg arg1 ori_name tar_name),
                               (replace_var_in_arg arg2 ori_name tar_name))) in
     AssignStmt (str, res_exp)
  | ReturnStmt arg -> ReturnStmt (replace_var_in_arg arg ori_name tar_name)
  | IfStmt (cmp, arg1, arg2, ls1, ls2) ->
     IfStmt (cmp,
             replace_var_in_arg arg1 ori_name tar_name,
             replace_var_in_arg arg2 ori_name tar_name,
             replace_var_in_stmt_ls ls1 ori_name tar_name,
             replace_var_in_stmt_ls ls2 ori_name tar_name)
                   
and replace_var_in_stmt_ls stmt_ls ori_name tar_name =
       List.map (fun stmt -> replace_var_in_stmt stmt ori_name tar_name) stmt_ls
       
let rec helper_filter exp_ls filtered_ls filter_fn =
  match exp_ls with
  | [] -> (List.rev filtered_ls, [])
  | hd :: tl ->
     match filter_fn hd with
     | true -> helper_filter tl (hd :: filtered_ls) filter_fn
     | false -> (List.rev filtered_ls, hd :: tl)
              
let rec simple_exp_from_ls exp_ls builder =
  let (all_simples, hd_not_simple) = helper_filter exp_ls [] is_exp_simple in
  match hd_not_simple with
  | [] -> builder all_simples
  | hd :: tl ->
     let new_name = gen_id "tmp" in
     let new_exp_ls = all_simples @ (R2.VarExp (new_name, Ploc.dummy) :: tl) in 
     flatten hd new_name (simple_exp_from_ls new_exp_ls builder)  

and flatten exp bind_name cont =
  match exp with
  | R2.IntExp (num, _) -> AssignStmt (bind_name, ArgExp (IntArg num)) :: cont
  | R2.ReadExp _ -> AssignStmt (bind_name, ReadExp) :: cont
  | R2.NegExp (exp, _) ->
     if is_exp_simple exp
     then AssignStmt (bind_name, NegExp (simple_exp_to_arg exp)) :: cont
     else flatten exp bind_name (AssignStmt (bind_name, NegExp (VarArg bind_name)) :: cont) 
  | R2.AddExp (exp1, exp2, _) ->
     simple_exp_from_ls [exp1; exp2]
       (fun ls ->
         AssignStmt (bind_name,
                     AddExp (simple_exp_to_arg (List.nth ls 0),
                             simple_exp_to_arg (List.nth ls 1))) :: cont)
  | R2.VarExp (str, _) -> replace_var_in_stmt_ls cont bind_name str
  | R2.LetExp (str, exp, body, _) ->
     flatten exp str (flatten body bind_name cont)
  | R2.TrueExp _ -> AssignStmt (bind_name, ArgExp (BoolArg true)) :: cont
  | R2.FalseExp _ -> AssignStmt (bind_name, ArgExp (BoolArg false)) :: cont
  | R2.AndExp (exp1, exp2, _) ->
     let inner = R2.IfExp (exp2, TrueExp Ploc.dummy, FalseExp Ploc.dummy, Ploc.dummy) in
     let outer = R2.IfExp (exp1, inner, FalseExp Ploc.dummy, Ploc.dummy) in
     flatten outer bind_name cont
  | R2.NotExp (exp, _) ->
     simple_exp_from_ls [exp]
       (fun ls ->
         AssignStmt (bind_name, NotExp (simple_exp_to_arg (List.hd ls))) :: cont)
  | R2.CmpExp (cmp, exp1, exp2, _) ->
     simple_exp_from_ls [exp1; exp2]
       (fun ls ->
         AssignStmt (bind_name,
                     CmpExp (cmp,
                             simple_exp_to_arg (List.nth ls 0),
                             simple_exp_to_arg (List.nth ls 1))) :: cont)
  | R2.IfExp (exp1, exp2, exp3, _) -> 
     match exp1 with
     | R2.CmpExp (cmp, lhs, rhs, _) ->
        let exp2_flatten = flatten exp2 bind_name [] in
        let exp3_flatten = flatten exp3 bind_name [] in
        simple_exp_from_ls [lhs; rhs]
          (fun ls ->
            IfStmt (cmp,
                    simple_exp_to_arg (List.nth ls 0),
                    simple_exp_to_arg (List.nth ls 1),
                    exp2_flatten,
                    exp3_flatten) :: cont)
     | R2.TrueExp _ ->
        flatten exp2 bind_name cont
     | R2.FalseExp _ ->
        flatten exp3 bind_name cont
     | R2.LetExp (str, bind_exp, body, loc) ->
        flatten bind_exp str
          (flatten (R2.IfExp (body, exp2, exp3, loc)) bind_name cont)
     | R2.NotExp (exp, loc) ->
        flatten (R2.IfExp (exp, exp3, exp2, loc)) bind_name cont
     | _ -> 
        let exp2_flatten = flatten exp2 bind_name [] in
        let exp3_flatten = flatten exp3 bind_name [] in
        simple_exp_from_ls [exp1]
          (fun ls ->
            IfStmt (R2.EQ,
                    simple_exp_to_arg (List.hd ls),
                    BoolArg true,
                    exp2_flatten,
                    exp3_flatten) :: cont)
                                                                                     
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
                   "(" ^ else_part ^ "))" ^ ending)
  in
  recur stmt_ls ""
              
    
