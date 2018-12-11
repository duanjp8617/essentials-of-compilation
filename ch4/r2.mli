(* R2 type definition *)
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

(* The parser  *)
val parse : char Stream.t -> program

(* Type checking  *)
type r2_type =
  | IntT
  | BoolT

type typed_program =
  | TypedProgram of r2_type * expression
              
exception TypeError of string * Ploc.t

val typecheck : expression -> ((string * r2_type) list) -> r2_type 

(* Generate unique IDs  *)
val gen_id : string -> string

(* Uniquify an expression *)
val uniquify : expression -> expression                         
(* val do_uniquify : expression -> expression
 * val remove_complex : expression -> expression
 * val remove_stupid_let : expression -> expression
 * val string_of_exp : expression -> string *)                                    
