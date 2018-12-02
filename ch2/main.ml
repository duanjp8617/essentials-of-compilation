open R1
open C0
open X86_0

let string_of_loc loc =
  let fp = Ploc.first_pos loc in
  let lp = Ploc.last_pos loc in
  let ln = Ploc.line_nb loc in
  let lnl = Ploc.line_nb_last loc in
  let bp = Ploc.bol_pos loc in
  let bpl = Ploc.bol_pos_last loc in
  string_of_int ln ^ "-" ^ string_of_int lnl ^ ":" ^ string_of_int (fp - bp + 1) ^ "-" ^ string_of_int (lp - bpl)

                                                                                           
let parse_r1_output prog =
  (match prog with
   | R1.AProgram (i, exp) ->
      exp |> do_uniquify |> remove_complex |> remove_stupid_let |> string_of_exp |> print_endline);
  print_endline "parse succeed";
  ()
    
                                                                                           
let parse_c0_output prog =
  (match prog with
   | R1.AProgram (i, exp) ->
      let new_r1_prog =
        R1.AProgram (i, exp |> do_uniquify |> remove_complex |> remove_stupid_let) in
      new_r1_prog |>
        r1prog_to_prog |>
        uncover_locals |>
        (* string_of_program |> *)
        (* show_instr_for_c0_prog |> *)
        print_program |>
        print_endline);
  ()

let main () = 
  try Stream.of_channel (open_in Sys.argv.(1)) |> parse |> parse_c0_output
  with 
  | Ploc.Exc (loc, Stream.Error msg) -> print_endline (string_of_loc loc ^ ": [bad syntax] " ^ msg); exit 1
                                     
let () = main ()    
