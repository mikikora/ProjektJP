open Format

type term =
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
  (* liczby naturalne *)
  | TmNum of int
  | TmAdd of term * term
  | TmMul of term * term
  | TmSub of term * term
  | TmEq of term * term
  (* wartosci logiczne *)
  | TmTru
  | TmFal
  | TmIf of term * term * term
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
  | TrTemp of int
  | TrId of string
  | TrVar of int
  | TrAbs of raw_term
  | TrApp of raw_term * raw_term

val pp_print_term : Format.formatter -> term -> unit
val pp_print_raw_term : Format.formatter -> raw_term -> unit
(* To debbug *)
val print_term : term -> unit
val print_raw_term : raw_term -> unit
