open R1

type info = R1.info

type argument =
  | IntArg of int
  | VarArg of string

and expression =
  | ArgExp of argument
  | ReadExp
  | NegExp of argument
  | AddExp of argument * argument

and statement =
  | AssignStmt of argument * expression

and tail =
  | ReturnTail of expression
  | SeqTail of statement * tail

and label =
  | EndLabel of string * tail
  | ManyLabel of string * tail * label

type program =
  | AProgram of info * label

exception R1ToC0Error of string

val r1prog_to_prog : R1.program -> program
val string_of_program : program -> string                                     
