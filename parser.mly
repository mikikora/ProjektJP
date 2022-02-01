
%{
  open Syntax
%}

%token ADD
%token SUB
%token MUL
%token EQ
%token LAMBDA
%token <string> VAR
%token <int> NUM
%token TRUE
%token FALSE
%token IF
%token FIX
%token COLON
%token FST
%token SND
%token NIL
%token HEAD
%token TAIL
%token ISNIL
%token SEMICOLON
%token DOT
%token LPAR
%token RPAR
%token EOF
%token COMMA
%token THEN
%token ELSE

%start toplevel
%type <Syntax.term * Syntax.term option> toplevel
%type <Syntax.term> term
%type <Syntax.term> appterm
%type <Syntax.term> aterm
%%

toplevel:
  | term option(SEMICOLON) EOF
  { ($1, None) }
  | t1=term SEMICOLON t2=term option(SEMICOLON) EOF
  { (t1, Some t2)}

term:
  | appterm
  { $1 }
  | IF t1=term THEN t2=term ELSE t3=term
  { TmIf (t1, t2, t3) }
  | LAMBDA v=VAR DOT t=term
  { TmAbs (v, t) }
  | LPAR t1=term COMMA t2=term RPAR
  { TmPair (t1, t2) }


appterm:
  | aterm
  { $1 }
  | appterm aterm
  { TmApp ($1, $2) }
  | t1=aterm ADD t2=aterm
  { TmAdd (t1, t2) }
  | t1=aterm SUB t2=aterm
  { TmSub (t1, t2) }
  | t1=aterm MUL t2=aterm
  { TmMul (t1, t2) }
  | t1=aterm EQ t2=aterm
  { TmEq (t1, t2) }
  | FIX t=aterm
  { TmFix t }
  | FST t=aterm
  { TmFst t }
  | SND t=aterm
  { TmSnd t }
  | t1=aterm COLON COLON t2=aterm
  { TmCons (t1, t2) }
  | HEAD t=aterm
  { TmHd t }
  | TAIL t=aterm
  { TmTl t }
  | ISNIL t=aterm
  { TmIsNil t }

aterm:
  | LPAR t=term RPAR
  { t }
  | n=NUM
  { TmNum n }
  | v=VAR
  { TmVar v }
  | TRUE
  { TmTru }
  | FALSE
  { TmFal }
  | NIL
  { TmNil }


