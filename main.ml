open Support.Error
open Syntax
open Format
open Core

let inFile : string option ref = ref None
let outfile = "output.f"

let parseFile () =
  Arg.parse []
    (function
      | s -> (
          match !inFile with
          | Some _ -> failwith "You must specify at most one input file"
          | None -> inFile := Some s))
    "";
  let infile = match !inFile with None -> "input.f" | Some v -> v in
  let ch_in =
    try open_in infile
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
  let restored = restore_names res in
  pp_print_term formatter restored

let comparing t1 t2 formatter =
  let unfolded_t1 = unfold t1 and unfolded_t2 = unfold t2 in
  let res = beta_compare unfolded_t1 unfolded_t2 in
  pp_print_bool formatter res

let main () =
  let t1, t2_option = parseFile () in
  let ch_out = open_out outfile in
  let formatter = formatter_of_out_channel ch_out in
  (match t2_option with
  | None -> normalizing t1 formatter
  | Some t2 -> comparing t1 t2 formatter);
  pp_print_newline formatter ();
  pp_print_flush formatter ();
  close_out ch_out;
  exit 0

let _ = main ()
