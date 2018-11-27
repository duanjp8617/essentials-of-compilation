open R1

let string_of_loc loc =
  let fp = Ploc.first_pos loc in
  let lp = Ploc.last_pos loc in
  let ln = Ploc.line_nb loc in
  let lnl = Ploc.line_nb_last loc in
  let bp = Ploc.bol_pos loc in
  let bpl = Ploc.bol_pos_last loc in
  string_of_int ln ^ "-" ^ string_of_int lnl ^ ":" ^ string_of_int (fp - bp + 1) ^ "-" ^ string_of_int (lp - bpl)

                                                                                           
let parse_output prog =
  (match prog with
   | AProgram (i, exp) -> do_uniquify exp);
  print_endline "parse succeed";
  ()

let main () = 
  try Stream.of_channel (open_in Sys.argv.(1)) |> parse |> parse_output
  with 
  | Ploc.Exc (loc, Stream.Error msg) -> print_endline (string_of_loc loc ^ ": [bad syntax] " ^ msg); exit 1
                                     
let () = main ()    
