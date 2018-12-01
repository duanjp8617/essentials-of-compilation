open R1

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

type info = string * (argument list)
               
type program =
  | AProgram of info * label

exception R1ToC0Error of string

val r1prog_to_prog : R1.program -> program
val uncover_locals : program -> program

val string_of_program : program -> string                                     

