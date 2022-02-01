open Format


type term =
    TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
(* liczby naturalne *)
  | TmNum of int
  | TmAdd of term * term
  | TmMul of term * term
  | TmSub of term * term
  | TmEq  of term * term
(* wartosci logiczne *)
  | TmTru 
  | TmFal 
  | TmIf  of term * term * term
(* rekursja *)
  | TmFix of term
(* pary *)
  | TmPair of term * term
  | TmFst of term
  | TmSnd of term
  (* listy *)
  | TmNil 
  | TmCons of term * term
  | TmHd of term
  | TmTl of term
  | TmIsNil of term

type raw_term =
    TRId of string
  | TRVar of int
  | TRAbs of string * int
  | TRApp of raw_term * raw_term


val print_term : term -> unit