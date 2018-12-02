open R1

type argument =
  | IntArg of int
  | VarArg of string

type expression =
  | ArgExp of argument
  | ReadExp
  | NegExp of argument
  | AddExp of argument * argument

type statement =
  | AssignStmt of argument * expression

type tail =
  | ReturnTail of expression
  | SeqTail of statement * tail

type label = string             
             
type info = label * (argument list)
               
type program =
  | AProgram of info * ((label * tail) list)
   
exception R1ToC0Error of string

val r1prog_to_prog : R1.program -> program
val uncover_locals : program -> program

val string_of_program : program -> string                                     

