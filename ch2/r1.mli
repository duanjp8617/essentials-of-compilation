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
