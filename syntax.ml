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
  | TrTemp of int (* zmienna wolna wynikająca z wejścia obliczeniami pod lambdę *)
  | TrId of string
  | TrVar of int
  | TrAbs of raw_term
  | TrApp of raw_term * raw_term

(* printer *)
let rec pp_print_term fmtr = function
  | TmIf (t1, t2, t3) ->
      pp_open_hvbox fmtr 0;
      pp_print_string fmtr "if";
      pp_print_space fmtr ();
      pp_print_term fmtr t1;
      pp_print_space fmtr ();
      pp_print_string fmtr "then";
      pp_print_space fmtr ();
      pp_print_term fmtr t2;
      pp_print_space fmtr ();
      pp_print_string fmtr "else";
      pp_print_space fmtr ();
      pp_print_term fmtr t3;
      pp_close_box fmtr ()
  | TmAbs (id, t) ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "lambda ";
      pp_print_string fmtr id;
      pp_print_string fmtr ".";
      pp_print_space fmtr ();
      pp_print_term fmtr t;
      pp_close_box fmtr ()
  | TmPair (t1, t2) ->
      pp_open_hovbox fmtr 1;
      pp_print_string fmtr "(";
      pp_print_space fmtr ();
      pp_print_term fmtr t1;
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_term fmtr t2;
      pp_print_string fmtr ")";
      pp_close_box fmtr ()
  | t -> pp_print_appterm fmtr t

and pp_print_appterm fmtr t =
  let print_two t1 t2 sym =
    pp_open_hvbox fmtr 0;
    pp_print_aterm fmtr t1;
    pp_print_space fmtr ();
    pp_print_string fmtr sym;
    pp_print_space fmtr ();
    pp_print_aterm fmtr t2;
    pp_close_box fmtr ()
  and print_one t sym =
    pp_open_hvbox fmtr 0;
    pp_print_string fmtr sym;
    pp_print_space fmtr ();
    pp_print_aterm fmtr t;
    pp_close_box fmtr ()
  in
  match t with
  | TmApp (t1, t2) ->
      pp_open_hvbox fmtr 0;
      pp_print_appterm fmtr t1;
      pp_print_space fmtr ();
      pp_print_aterm fmtr t2;
      pp_close_box fmtr ()
  | TmAdd (t1, t2) -> print_two t1 t2 "+"
  | TmSub (t1, t2) -> print_two t1 t2 "-"
  | TmMul (t1, t2) -> print_two t1 t2 "*"
  | TmEq (t1, t2) -> print_two t1 t2 "="
  | TmCons (t1, t2) -> print_two t1 t2 "::"
  | TmFix t -> print_one t "fix"
  | TmFst t -> print_one t "fst"
  | TmSnd t -> print_one t "snd"
  | TmHd t -> print_one t "head"
  | TmTl t -> print_one t "tail"
  | TmIsNil t -> print_one t "isnil"
  | _ as t -> pp_print_aterm fmtr t

and pp_print_aterm fmtr = function
  | TmNum n -> pp_print_int fmtr n
  | TmVar v -> pp_print_string fmtr v
  | TmTru -> pp_print_string fmtr "true"
  | TmFal -> pp_print_string fmtr "false"
  | TmNil -> pp_print_string fmtr "nil"
  | _ as t ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "(";
      pp_print_cut fmtr ();
      pp_print_term fmtr t;
      pp_print_cut fmtr ();
      pp_print_string fmtr ")";
      pp_close_box fmtr ()

let rec pp_print_raw_term fmtr = function
  | TrAbs t ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "lambda.";
      pp_print_space fmtr ();
      pp_print_raw_term fmtr t;
      pp_close_box fmtr ()
  | t -> pp_print_raw_appterm fmtr t

and pp_print_raw_appterm fmtr = function
  | TrApp (t1, t2) ->
      pp_open_hvbox fmtr 0;
      pp_print_raw_appterm fmtr t1;
      pp_print_space fmtr ();
      pp_print_raw_aterm fmtr t2;
      pp_close_box fmtr ()
  | t -> pp_print_raw_aterm fmtr t

and pp_print_raw_aterm fmtr = function
  | TrTemp n ->
      pp_print_string fmtr "&";
      pp_print_int fmtr n
  | TrId v -> pp_print_string fmtr v
  | TrVar n -> pp_print_int fmtr n
  | t ->
      pp_open_hvbox fmtr 1;
      pp_print_string fmtr "(";
      pp_print_cut fmtr ();
      pp_print_raw_term fmtr t;
      pp_print_cut fmtr ();
      pp_print_string fmtr ")";
      pp_close_box fmtr ()

let print_term t = 
  pp_print_term std_formatter t

let print_raw_term t = 
  pp_print_raw_term std_formatter t