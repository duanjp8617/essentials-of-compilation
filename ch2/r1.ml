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
       
let rec is_exp_simple exp =
  match exp with
  | IntExp _ -> true
  | ReadExp _ -> true
  | NegExp (exp, _) -> int_or_var exp
  | AddExp (exp1, exp2, _) -> (int_or_var exp1) && (int_or_var exp2)
  | VarExp _ -> true
  | LetExp (str, exp, body, _) -> is_exp_simple exp && is_exp_simple body

let rec helper_filter exp_ls filtered_ls filter_fn =
  match exp_ls with
  | [] -> (List.rev filtered_ls, [])
  | hd :: tl ->
     match filter_fn hd with
     | true -> helper_filter tl (hd :: filtered_ls) filter_fn
     | false -> (List.rev filtered_ls, hd :: tl)
              
(* Given a list of expression and a simple expression builder,
   this function returns a new expression that is simplified. *)
let rec simple_exp_from_ls exp_ls builder =
  let (all_simples, hd_not_simple) = helper_filter exp_ls [] int_or_var in
  match hd_not_simple with
  | [] -> builder all_simples
  | hd :: tl ->
     let new_name = new_id "tmp" in
     let new_exp_ls = all_simples @ (VarExp (new_name, Ploc.dummy) :: tl) in 
     simplify hd new_name (simple_exp_from_ls new_exp_ls builder)

(* Given an exp: expression, a let_name:String, a let_body: expression,
   this function create a new let expression with the form
   LetExp (let_name, let_exp, let_body). The let_exp will be a 
   simplified expression created by exp. *)
and simplify exp let_name let_body = 
  match exp with
  | IntExp (num, loc) -> LetExp (let_name, IntExp (num, loc), let_body, Ploc.dummy)
  | ReadExp loc -> LetExp (let_name, ReadExp loc, let_body, Ploc.dummy)
  | NegExp (exp, loc) ->
     simple_exp_from_ls [exp] (fun simple_ls ->
         LetExp (let_name, NegExp (List.hd simple_ls, loc), let_body, Ploc.dummy))
  | AddExp (exp1, exp2, loc) ->
     simple_exp_from_ls (exp1 :: [exp2])
       (fun simple_ls ->
         LetExp (let_name,
                 AddExp (List.nth simple_ls 0, List.nth simple_ls 1, loc),
                 let_body,
                 Ploc.dummy))
  | VarExp (var_name, loc) ->
     LetExp (let_name, VarExp (var_name, loc), let_body, Ploc.dummy)
  | LetExp (inner_str, inner_exp, inner_body, loc) ->
     match is_exp_simple exp with
     | true ->
        (match inner_body with
         | VarExp (str, _) ->
            if str = inner_str
            then LetExp (let_name, inner_exp, let_body, Ploc.dummy)
            else LetExp (inner_str, inner_exp, let_body, Ploc.dummy)
         | _ ->
            LetExp (inner_str, inner_exp, LetExp (let_name, inner_body, let_body, Ploc.dummy), loc))
     | false ->
        let new_let_body = simplify inner_body let_name let_body in
        simplify inner_exp inner_str new_let_body                             

let remove_complex exp =
  if is_exp_simple exp
  then
    exp
  else
    let new_name = new_id "tmp" in simplify exp new_name (VarExp (new_name, Ploc.dummy))
   
let rec string_of_exp exp =
  match exp with
  | IntExp (num, _) -> string_of_int num
  | ReadExp _ -> "(read)"
  | NegExp (exp, _) -> "(-" ^ (string_of_exp exp) ^ ")"
  | AddExp (exp1, exp2, _) -> "(+" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"
  | VarExp (str,_) -> str
  | LetExp (str, exp, body, _) -> "(let([" ^ str ^ " " ^ (string_of_exp exp) ^ "]) " ^ (string_of_exp body) ^ ")"
