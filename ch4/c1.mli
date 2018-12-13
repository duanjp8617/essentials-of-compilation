open R2

type arg =
  | IntArg of int
  | VarArg of string
  | BoolArg of bool

type cmp = R2.cmp
             
type exp =
  | ArgExp of arg
  | ReadExp
  | NegExp of arg
  | AddExp of arg * arg
  | NotExp of arg
  | CmpExp of cmp * arg * arg
                            
type stmt =
  | AssignStmt of string * exp
  | ReturnStmt of arg
  | IfStmt of cmp * arg * arg * (stmt list) * (stmt list)
            
type program =
  | AProgram of (string list) * R2.r2_type * (stmt list)
   
val do_flatten : R2.expression -> (stmt list)

val uncover_locals : (stmt list) -> (string list)
  
val string_of_stmt_list : (stmt list) -> string

val string_of_arg_list : (string list) -> string                                           
