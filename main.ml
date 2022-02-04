open Support.Error
open Syntax
open Format
open Core

let inFile = "input.f"
let outfile = "output.f"

let parseFile inFile =
  let ch_in =
    try open_in inFile
    with _ ->
      print_string "Nie udało się otworzyć pliku";
      exit 1
  in
  let lexbuf = Lexing.from_channel ch_in in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with _ -> error (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in ch_in;
  result

let normalizing t formatter =
  let unfolded = unfold t in
  let res = normalize unfolded [] in
  pp_print_raw_term formatter res

let comparing t1 t2 formatter =
  let unfolded_t1 = unfold t1 and unfolded_t2 = unfold t2 in
  let res = beta_compare unfolded_t1 unfolded_t2 in
  pp_print_bool formatter res

let main () =
  let t1, t2_option = parseFile inFile in
  let ch_out = open_out outfile in
  let formatter = formatter_of_out_channel ch_out in
  (match t2_option with
  | None -> normalizing t1 formatter
  | Some t2 -> comparing t1 t2 formatter);
  pp_print_flush formatter ();
  close_out ch_out;
  exit 0

let _ = main ()
