{
  open Parser
  open Lexing
  open Support.Error

  (* type token = Parser.token *)
  

  let lineno   = ref 1
  and start    = ref 0

  and filename = ref ""
  and startLex = ref dummyinfo

  let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

  let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

  let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)




  let reservedWords = [
    (* Keywords *)
    ("pair", PAIR);
    ("lambda", LAMBDA);
    ("true", TRUE);
    ("false", FALSE);
    ("if", IF);
    ("fix", FIX);
    ("fst", FST);
    ("snd", SND);
    ("nil", NIL);
    ("head", HEAD);
    ("tail", TAIL);
    ("isnil", ISNIL);
    ("then", THEN);
    ("else", ELSE);
    ("add", ADD);
    ("sub", SUB);
    ("mul", MUL);
    ("eq", EQ);
    (* symbols *)
    ("+", ADD);
    ("-", SUB);
    ("*", MUL);
    ("=", EQ);
    (":", COLON);
    (";", SEMICOLON);
    (".", DOT);
    ("(", LPAR);
    (")", RPAR);
    (",", COMMA);
  ]

  let (symbolTable : (string, Parser.token) Hashtbl.t) = Hashtbl.create 1024
  let () =
    List.iter (function (str, t) -> Hashtbl.add symbolTable str t) reservedWords
  
  let createID str =
    try 
      Hashtbl.find symbolTable str
    with _ ->
      VAR str

  let text = lexeme
}

let identifier    = ['_' 'a'-'z' 'A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9' ''']*
let number        = ['0'-'9']+ 


rule main = parse
  | [' ' '\009' '\012']+     
  { main lexbuf }

  | [' ' '\009' '\012']*"\n" 
  { newline lexbuf; main lexbuf }

  | "//"
  { startLex := info lexbuf; line_comment lexbuf }

  | "*/" 
  { error (info lexbuf) "Unmatched end of comment" }

  | "/*" 
  { startLex := info lexbuf; comment lexbuf; main lexbuf }

  | eof
  { EOF }

  | number as num
  { 
    try NUM (int_of_string num)
    with _ -> error (info lexbuf) "there is a problem with number"
  }

  | identifier
  {
    createID (text lexbuf)
  }

  | '+' | '-' | '*' | '=' | ':'
  | ';' | '.' | '(' | ')' | ','
  { createID (text lexbuf) }

  | _ {error (info lexbuf) "Illegal character" }



and line_comment = parse
  | '\n'
  { new_line lexbuf; main lexbuf }

  | eof { EOF }

  | _
  { line_comment lexbuf }

and comment = parse
  | "*/"
  { }
  
  | eof
  { error (!startLex) "Comment not terminated" }

  | '\n'
  { newline lexbuf; comment lexbuf }
  
  | _
  { comment lexbuf }
