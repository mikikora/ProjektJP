open Support.Error
open Syntax
open Format
open Core

let inFile = "input.f"

let parseFile inFile =
  let pi =
    try open_in inFile
    with _ ->
      print_string "Nie ma pliku";
      exit 1
  in
  let lexbuf = Lexing.from_channel pi in
  let result =
    try Parser.toplevel Lexer.main lexbuf
    with _ -> error (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  result

let main () =
  let parsed = parseFile inFile in
  print_term (fst parsed);
  print_flush ();
  print_newline ();
  let unfolded = unfold (fst parsed) in
  print_raw_term unfolded;
  print_newline ();
  print_flush ();
  let res = normalize unfolded [] in
  print_raw_term res;
  print_newline ();
  print_flush ();
  exit 0

let _ = main ()
