type program =
  | AProgram of expression

and cmp =
  | EQ
  | L
  | LE
  | G
  | GE
  
and expression =
  | IntExp of int * Ploc.t
  | ReadExp of Ploc.t
  | NegExp of expression * Ploc.t
  | AddExp of expression * expression * Ploc.t
  | VarExp of string * Ploc.t
  | LetExp of string * expression * expression * Ploc.t
  | TrueExp of Ploc.t
  | FalseExp of Ploc.t
  | AndExp of expression * expression * Ploc.t
  | NotExp of expression * Ploc.t
  | CmpExp of cmp * expression * expression * Ploc.t
  | IfExp of expression * expression * expression * Ploc.t
  
let g = Grammar.gcreate (Plexer.gmake ())
let c = Grammar.Entry.create g "cmp"
let e = Grammar.Entry.create g "expression"
let p = Grammar.Entry.create g "program"

let parse = Grammar.Entry.parse p 

EXTEND
              
p: [
      ["("; "program"; exp = e; ")" -> AProgram (exp)]
];

e : [
          [ num = INT -> IntExp (int_of_string num, loc) ]
          | [ "("; "read"; ")" -> ReadExp loc ]
          | [ "("; "-"; exp = e; ")" -> NegExp (exp, loc) ]
          | [ "("; "+"; exp1 = e; exp2 = e; ")" -> AddExp (exp1, exp2, loc) ]
          | [ var = LIDENT -> VarExp (var, loc) ]
          | [ "("; "let"; "("; "["; var = LIDENT; exp1 = e; "]"; ")"; exp2 = e; ")" -> LetExp (var, exp1, exp2, loc) ]
          | [ "t" -> TrueExp loc ]
          | [ "f" -> FalseExp loc ]
          | [ "("; "and"; exp1 = e; exp2 = e; ")" -> AndExp (exp1, exp2, loc) ]
          | [ "("; "not"; exp = e; ")" -> NotExp (exp, loc) ]
          | [ "("; "if"; exp1 = e; exp2 = e; exp3 = e; ")" -> IfExp (exp1, exp2, exp3, loc) ]
          | [ "("; cmp = c; exp1 = e; exp2 = e; ")" -> CmpExp (cmp, exp1, exp2, loc) ]
              ];

c : [
    [ "eq" -> EQ ]
    | [ "<" -> L ]
    | [ "<=" -> LE ]
    | [ ">" -> G ]
    | [ ">=" -> GE ]
  ];
     
END

(* Generic environment operations  *)

exception MissInEnv of string
                          
let empty_env () = []
                 
let rec apply_env str env =
  match env with
  | [] -> raise (MissInEnv str)
  | (var_name, sub_name) :: tl ->
     (if var_name = str
      then sub_name
      else apply_env str tl)
    
let extend_env str new_str env =
  (str, new_str) :: env

(* Type checking a R2 program *)

type r2_type =
  | IntT
  | BoolT

type typed_program =
  | TypedProgram of r2_type * expression
              
exception TypeError of string * Ploc.t

(* Throw TypeError exception  *)
let rec typecheck exp type_env =
  match exp with
  | IntExp _ -> IntT
  | ReadExp _ -> IntT
  | NegExp (exp, loc) ->
     (match typecheck exp type_env with
      | IntT -> IntT
      | BoolT -> raise (TypeError ("The NegExp expects IntT.", loc)))
  | AddExp (exp1, exp2, loc) ->
     (match ((typecheck exp1 type_env), (typecheck exp2 type_env)) with
      | (IntT, IntT) -> IntT
      | _ -> raise (TypeError ("The AddExp expects two IntT.", loc)))
  | VarExp (str, loc) ->
     (try apply_env str type_env with
      | MissInEnv str -> raise (TypeError ("Variable " ^ str ^ " misses in environment.", loc)))
  | LetExp (str, exp1, exp2, loc) ->
     typecheck exp2 (extend_env str (typecheck exp1 type_env) type_env)
  | TrueExp _ -> BoolT
  | FalseExp _ -> BoolT
  | AndExp (exp1, exp2, loc) ->
     (match ((typecheck exp1 type_env), (typecheck exp2 type_env)) with
      | (BoolT, BoolT) -> BoolT
      | _ -> raise (TypeError ("The AndExp expects two BoolT.", loc)))
  | NotExp (exp, loc) ->
     (match typecheck exp type_env with
      | BoolT -> BoolT
      | _ -> raise (TypeError ("The NotExp expects BoolT.", loc)))
  | CmpExp (cmp, exp1, exp2, loc) -> 
     (match ((typecheck exp1 type_env), (typecheck exp2 type_env)) with
      | (BoolT, BoolT) -> BoolT
      | (IntT, IntT) -> BoolT
      | _ -> raise (TypeError ("The CmpExp expects two identical types.", loc)))
  | IfExp (exp1, exp2, exp3, loc) ->
     (match typecheck exp1 type_env with
      | BoolT ->
         (match ((typecheck exp2 type_env),(typecheck exp3 type_env)) with
          | (BoolT, BoolT) -> BoolT
          | (IntT, IntT) -> IntT
          | _ ->
             raise (TypeError ("The IfExp expects two identical types in then-else branch.", loc)))
      | _ -> raise (TypeError ("The IfExp expects a BoolT in condition.", loc)))
         
       
(* Generate unique IDs  *)
let unique_id = ref 0

let gen_id var =
  unique_id := (!unique_id + 1);
  var ^ "." ^ (string_of_int !unique_id)

(* uniquify  *)
let rec do_uniquify exp env =
  match exp with
  | IntExp (num, loc) -> IntExp(num, loc)
  | ReadExp loc -> ReadExp loc
  | NegExp (exp, loc) -> NegExp ((do_uniquify exp env), loc)
  | AddExp (exp1, exp2, loc) -> AddExp ((do_uniquify exp1 env), (do_uniquify exp2 env), loc)
  | VarExp (str, loc) -> VarExp ((apply_env str env), loc)
  | LetExp (str, exp1, body, loc) ->
     let new_str = gen_id str in
     LetExp (new_str, (do_uniquify exp1 env), (do_uniquify body (extend_env str new_str env)), loc)
  | TrueExp loc -> TrueExp loc
  | FalseExp loc -> FalseExp loc
  | AndExp (exp1, exp2, loc) -> AndExp ((do_uniquify exp1 env), (do_uniquify exp2 env), loc)
  | NotExp (exp, loc) -> NotExp ((do_uniquify exp env), loc)
  | CmpExp (cmp, exp1, exp2, loc) ->
     CmpExp (cmp, (do_uniquify exp1 env), (do_uniquify exp2 env), loc)
  | IfExp (exp1, exp2, exp3, loc) ->
     IfExp ((do_uniquify exp1 env), (do_uniquify exp2 env), (do_uniquify exp3 env), loc)

let uniquify exp =
  do_uniquify exp []
