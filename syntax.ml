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
  | TrId of string
  | TrVar of int
  | TrAbs of raw_term
  | TrApp of raw_term * raw_term

(* printer *)
let rec print_term = function
  | TmIf (t1, t2, t3) ->
      open_hvbox 0;
      print_string "if";
      print_space ();
      print_term t1;
      print_space ();
      print_string "then";
      print_space ();
      print_term t2;
      print_space ();
      print_string "else";
      print_space ();
      print_term t3;
      close_box ()
  | TmAbs (id, t) ->
      open_hvbox 1;
      print_string "lambda ";
      print_string id;
      print_string ".";
      print_space ();
      print_term t;
      close_box ()
  | TmPair (t1, t2) ->
      open_hovbox 1;
      print_string "(";
      print_space ();
      print_term t1;
      print_string ",";
      print_space ();
      print_term t2;
      print_string ")";
      close_box ()
  | t -> print_appterm t

and print_appterm t =
  let print_two t1 t2 sym =
    open_hvbox 0;
    print_aterm t1;
    print_space ();
    print_string sym;
    print_space ();
    print_aterm t2;
    close_box ()
  and print_one t sym =
    open_hvbox 0;
    print_string sym;
    print_space ();
    print_aterm t;
    close_box ()
  in
  match t with
  | TmApp (t1, t2) ->
      open_hvbox 0;
      print_appterm t1;
      print_space ();
      print_aterm t2;
      close_box ()
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
  | _ as t -> print_aterm t

and print_aterm = function
  | TmNum n -> print_int n
  | TmVar v -> print_string v
  | TmTru -> print_string "true"
  | TmFal -> print_string "false"
  | TmNil -> print_string "nil"
  | _ as t ->
      open_hvbox 1;
      print_string "(";
      print_cut ();
      print_term t;
      print_cut ();
      print_string ")";
      close_box ()

let rec print_raw_term = function
  | TrAbs t ->
      open_hvbox 1;
      print_string "lambda.";
      print_space ();
      print_raw_term t;
      close_box ()
  | t -> print_raw_appterm t

and print_raw_appterm = function
  | TrApp (t1, t2) ->
      open_hvbox 0;
      print_raw_appterm t1;
      print_space ();
      print_raw_aterm t2;
      close_box ()
  | t -> print_raw_aterm t

and print_raw_aterm = function
  | TrId v -> print_string v
  | TrVar n -> print_int n
  | t ->
      open_hvbox 1;
      print_string "(";
      print_cut ();
      print_raw_term t;
      print_cut ();
      print_string ")";
      close_box ()
