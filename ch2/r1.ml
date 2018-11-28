type info = unit

type program =
  | AProgram of info * expression

and expression =
  | IntExp of int * Ploc.t
  | ReadExp of Ploc.t
  | NegExp of expression * Ploc.t
  | AddExp of expression * expression * Ploc.t
  | VarExp of string * Ploc.t
  | LetExp of string * expression * expression * Ploc.t
  
let g = Grammar.gcreate (Plexer.gmake ())
let e = Grammar.Entry.create g "expression"
let p = Grammar.Entry.create g "program"

let parse = Grammar.Entry.parse p 

EXTEND
              
p: [
      ["("; "program"; "("; ")"; exp = e; ")" -> AProgram ((), exp)]
];

e : [
          [ num = INT -> IntExp (int_of_string num, loc) ]
          | [ "("; "read"; ")" -> ReadExp loc ]
          | [ "("; "-"; exp = e; ")" -> NegExp (exp, loc) ]
          | [ "("; "+"; exp1 = e; exp2 = e; ")" -> AddExp (exp1, exp2, loc) ]
          | [ var = LIDENT -> VarExp (var, loc) ]
          | [ "("; "let"; "("; "["; var = LIDENT; exp1 = e; "]"; ")"; exp2 = e; ")" -> LetExp (var, exp1, exp2, loc) ]
];
     
END

(*Helper functions for uniquify operation *)
exception R1VarMissInEnv of string

let empty_env () = []

let rec apply_env str env =
  match env with
  | [] -> raise (R1VarMissInEnv str)
  | (var_name, sub_name) :: tl ->
     (if var_name = str
      then sub_name
      else apply_env str tl)

let unique_id = ref 0

let new_id str =
  unique_id := (!unique_id + 1);
  str ^ "." ^ (string_of_int !unique_id)
              
let extend_env str new_str env =
  (str, new_str) :: env

(*Perform uniquify operation, replace each variable name to a unique one *)  
let rec uniquify exp env =
  match exp with
  | IntExp (num, loc) -> IntExp(num, loc)
  | ReadExp loc -> ReadExp loc
  | NegExp (exp, loc) -> NegExp ((uniquify exp env), loc)
  | AddExp (exp1, exp2, loc) -> AddExp ((uniquify exp1 env), (uniquify exp2 env), loc)
  | VarExp (str, loc) -> VarExp ((apply_env str env), loc)
  | LetExp (str, exp1, body, loc) ->
     let new_str = new_id str in
     LetExp (new_str, (uniquify exp1 env), (uniquify body (extend_env str new_str env)), loc)

let do_uniquify exp =
  uniquify exp (empty_env ())

let int_or_var exp =
  match exp with
  | IntExp _ -> true
  | VarExp _ -> true
  | ReadExp _ -> true
  | _ -> false 
       
let is_exp_simple exp =
  match exp with
  | IntExp _ -> true
  | ReadExp _ -> true
  | NegExp (exp, _) -> int_or_var exp
  | AddExp (exp1, exp2, _) -> (int_or_var exp1) && (int_or_var exp2)
  | VarExp _ -> true
  | LetExp (str, exp, body, _) -> false

let rec remove_sub exp var_name body =
  match exp with
  | IntExp (num, loc) -> LetExp (var_name, IntExp (num, loc), body, loc)
  | ReadExp loc -> LetExp (var_name, ReadExp loc, body, loc)
  | NegExp (exp, loc) ->
     if int_or_var exp
     then LetExp (var_name, NegExp (exp, loc), body, loc)
     else
       let new_name = new_id "tmp" in
       remove_sub exp new_name
         (LetExp (var_name, NegExp (VarExp (new_name, loc), loc), body, loc))
  | AddExp (exp1, exp2, loc) ->
     (match (int_or_var exp1, int_or_var exp2) with
      | (true, true) -> LetExp (var_name, AddExp (exp1, exp2, loc), body, loc)
      | (true, false) ->
         let new_name = new_id "tmp" in
         remove_sub exp2 new_name
           (LetExp (var_name, AddExp (exp1, VarExp (new_name, loc), loc), body, loc))
      | (false, true) ->
         let new_name = new_id "tmp" in
         remove_sub exp1 new_name
           (LetExp (var_name, AddExp (VarExp (new_name, loc), exp2, loc), body, loc))
      | (false, false) ->
         let new_name1 = new_id "tmp" in
         let new_name2 = new_id "tmp" in
         let new_body =
           LetExp (
               var_name,
               AddExp (VarExp (new_name1, loc), VarExp (new_name2, loc), loc),
               body,
               loc) in
         remove_sub exp1 new_name1 (remove_sub exp2 new_name2 new_body))
  | VarExp (name, loc) ->
     LetExp (var_name, VarExp (name, loc), body, loc)
  | LetExp (let_name, let_exp, let_body, loc) ->
     (match (is_exp_simple let_exp, is_exp_simple let_body) with
      | (true, true) ->
         let new_let_body = LetExp (var_name, let_body, body, loc) in
         LetExp (let_name, let_exp, new_let_body, loc)
      | (true, false) ->
         LetExp (let_name, let_exp, (remove_sub let_body var_name body), loc)
      | (false, true) ->
         remove_sub let_exp let_name (LetExp (var_name, let_body, body, loc))
      | (false, false) ->
         remove_sub let_exp let_name (remove_sub let_body var_name body))

and remove_complex exp =
  match exp with
  | IntExp _ -> exp
  | ReadExp _ -> exp
  | NegExp (exp, loc) ->
     if int_or_var exp
     then NegExp (exp, loc)
     else
       let new_name = new_id "tmp" in
       remove_sub exp new_name (NegExp (VarExp (new_name, loc), loc))
  | AddExp (exp1, exp2, loc) ->
     (match (int_or_var exp1, int_or_var exp2) with
      | (true, true) -> AddExp (exp1, exp2, loc)
      | (true, false) ->
         let new_name = new_id "tmp" in
         remove_sub exp2 new_name (AddExp (exp1, VarExp (new_name, loc), loc))
      | (false, true) ->
         let new_name = new_id "tmp" in
         remove_sub exp1 new_name (AddExp (VarExp (new_name, loc), exp2, loc))
      | (false, false) ->
         let new_name1 = new_id "tmp" in
         let new_name2 = new_id "tmp" in
         let new_body =
           AddExp (VarExp (new_name1, loc), VarExp (new_name2, loc), loc)
         in
         remove_sub exp1 new_name1 (remove_sub exp2 new_name2 new_body))
  | VarExp (name, loc) ->
     VarExp (name, loc)
  | LetExp (let_name, let_exp, let_body, loc) ->
     (match (is_exp_simple let_exp, is_exp_simple let_body) with
      | (true, true) ->        
         LetExp (let_name, let_exp, let_body, loc)
      | (true, false) ->
         LetExp (let_name, let_exp, (remove_complex let_body), loc)
      | (false, true) ->
         remove_sub let_exp let_name let_body
      | (false, false) ->
         remove_sub let_exp let_name (remove_complex exp))
     
let rec string_of_exp exp =
  match exp with
  | IntExp (num, _) -> string_of_int num
  | ReadExp _ -> "(read)"
  | NegExp (exp, _) -> "(-" ^ (string_of_exp exp) ^ ")"
  | AddExp (exp1, exp2, _) -> "(+" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"
  | VarExp (str,_) -> str
  | LetExp (str, exp, body, _) -> "(let([" ^ str ^ " " ^ (string_of_exp exp) ^ "]) " ^ (string_of_exp body) ^ ")"
