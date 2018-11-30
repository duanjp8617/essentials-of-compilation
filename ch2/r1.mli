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

val parse : char Stream.t -> program
val do_uniquify : expression -> expression
val remove_complex : expression -> expression
val remove_stupid_let : expression -> expression
val string_of_exp : expression -> string                                    
