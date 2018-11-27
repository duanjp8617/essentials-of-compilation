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

    
