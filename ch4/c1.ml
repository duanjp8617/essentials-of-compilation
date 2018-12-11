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
  | R2.VarExp (str, _) -> AssignStmt (bind_name, ArgExp (VarArg str)) :: cont
  | R2.LetExp (str, exp, body, _) ->
     flatten exp str (flatten body bind_name cont)
  | R2.TrueExp _ -> AssignStmt (bind_name, ArgExp (BoolArg true)) :: cont
  | R2.FalseExp _ -> AssignStmt (bind_name, ArgExp (BoolArg false)) :: cont
  | R2.AndExp (exp1, exp2, _) ->
     let exp1_id = R2.gen_id "tmp" in
     let exp2_id = R2.gen_id "tmp" in
     let exp2_cont = AssignStmt (bind_name, AddExp (VarArg exp1_id, VarArg exp2_id)) :: cont in
     let exp1_cont = flatten exp2 exp2_id exp2_cont in
     flatten exp1 exp1_id exp1_cont
  | NotExp (exp, _) ->
     let exp_id = R2.gen_id "tmp" in
     flatten exp exp_id (AssignStmt (bind_name, NotExp (VarArg exp_id)) :: cont)
  | CmpExp (cmp, exp1, exp2, _) ->
     let exp1_id = R2.gen_id "tmp" in
     let exp2_id = R2.gen_id "tmp" in
     let exp2_cont = Assign
