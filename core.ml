open Syntax
open Support.Error
open Format

(* type used to differentiate lambda and variables existing before unfolding *)
type unsugar_term =
  | UsVar of string
  | UsOldVar of string
  | UsAbs of string * unsugar_term
  | UsOldAbs of string * unsugar_term
  | UsApp of unsugar_term * unsugar_term

(* type and module used in removing names from variables *)
type keys = KVar of string | KOldVar of string

module Keys = struct
  type t = keys

  let compare t1 t2 =
    match (t1, t2) with
    | KVar v1, KVar v2 -> String.compare v1 v2
    | KOldVar v1, KOldVar v2 -> String.compare v1 v2
    | KVar _, KOldVar _ -> -1
    | KOldVar _, KVar _ -> 1
end

module KeyMap = Map.Make (Keys)

(* unfold all terms to raw lambda expressions *)
let unfold t =
  (* remove sugar returns lambda expression with named variables,
     abstractions and applications *)
  let rec remove_sugar : term -> unsugar_term = function
    | TmVar v -> UsOldVar v
    | TmAbs (v, t) ->
        let new_t = remove_sugar t in
        UsOldAbs (v, new_t)
    | TmApp (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        UsApp (new_t1, new_t2)
    | TmTru -> UsAbs ("t", UsAbs ("f", UsVar "t"))
    | TmFal -> UsAbs ("t", UsAbs ("f", UsVar "f"))
    | TmIf (t1, t2, t3) ->
        let new_t1 = remove_sugar t1
        and new_t2 = remove_sugar t2
        and new_t3 = remove_sugar t3 in
        UsApp (UsApp (UsApp (UsAbs ("b", UsVar "b"), new_t1), new_t2), new_t3)
    | TmFix t ->
        let new_t = remove_sugar t in
        let under_f =
          UsAbs
            ( "x",
              UsApp
                ( UsVar "f",
                  UsAbs ("y", UsApp (UsApp (UsVar "x", UsVar "x"), UsVar "y"))
                ) )
        in
        UsApp (UsAbs ("f", UsApp (under_f, under_f)), new_t)
    | TmPair (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        UsAbs ("b", UsApp (UsApp (UsVar "b", new_t1), new_t2))
    | TmFst t ->
        let new_t = remove_sugar t and tru = remove_sugar TmTru in
        UsApp (new_t, tru)
    | TmSnd t ->
        let new_t = remove_sugar t and fls = remove_sugar TmFal in
        UsApp (new_t, fls)
    | TmNum n ->
        let succ n =
          UsAbs
            ( "f",
              UsAbs
                ("x", UsApp (UsVar "f", UsApp (UsApp (n, UsVar "f"), UsVar "x")))
            )
        in
        let rec generate_num n0 acc =
          if acc = n then n0 else generate_num (succ n0) (acc + 1)
        in
        let zero = UsAbs ("f", UsAbs ("x", UsVar "x")) in
        generate_num zero 0
    | TmAdd (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        UsAbs
          ( "f",
            UsAbs
              ( "x",
                UsApp
                  ( UsApp (new_t1, UsVar "f"),
                    UsApp (UsApp (new_t2, UsVar "f"), UsVar "x") ) ) )
    | TmMul (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        UsAbs
          ( "f",
            UsAbs
              ("x", UsApp (UsApp (new_t1, UsApp (new_t2, UsVar "f")), UsVar "x"))
          )
    | TmSub (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        let succ =
          TmAbs
            ( "n",
              TmAbs
                ( "f",
                  TmAbs
                    ( "x",
                      TmApp
                        ( TmVar "f",
                          TmApp (TmApp (TmVar "n", TmVar "f"), TmVar "x") ) ) )
            )
        in
        let zz =
          let c0 = TmNum 0 in
          TmPair (c0, c0)
        in
        let ss =
          TmAbs
            ("p", TmPair (TmSnd (TmVar "p"), TmApp (succ, TmSnd (TmVar "p"))))
        in
        let prd =
          remove_sugar (TmAbs ("m", TmFst (TmApp (TmApp (TmVar "m", ss), zz))))
        in
        UsApp (UsApp (new_t2, prd), new_t1)
    | TmEq (t1, t2) ->
        let fls = remove_sugar TmFal and tru = remove_sugar TmTru in
        let _and =
          UsAbs ("b", UsAbs ("c", UsApp (UsApp (UsVar "b", UsVar "c"), fls)))
        in
        let iszero m = UsApp (UsApp (m, UsAbs ("x", fls)), tru) in
        let s1 = remove_sugar (TmSub (t1, t2))
        and s2 = remove_sugar (TmSub (t2, t1)) in
        UsApp (UsApp (_and, iszero s1), iszero s2)
    | TmNil -> UsAbs ("f", UsAbs ("n", UsVar "n"))
    | TmCons (t1, t2) ->
        let new_t1 = remove_sugar t1 and new_t2 = remove_sugar t2 in
        UsAbs
          ( "f",
            UsAbs
              ( "n",
                UsApp
                  ( UsApp (UsVar "f", new_t1),
                    UsApp (UsApp (new_t2, UsVar "f"), UsVar "n") ) ) )
    | TmHd t ->
        let new_t = remove_sugar t in
        UsApp
          ( UsApp (new_t, UsAbs ("x", UsAbs ("y", UsVar "x"))),
            UsOldVar "HEAD ERROR" )
    | TmTl t ->
        let f =
          TmAbs
            ( "x",
              TmAbs
                ( "p",
                  TmPair
                    (TmSnd (TmVar "p"), TmCons (TmVar "x", TmSnd (TmVar "p")))
                ) )
        in
        remove_sugar (TmFst (TmApp (TmApp (t, f), TmPair (TmNil, TmNil))))
    | TmIsNil t ->
        let new_t = remove_sugar t in
        let tru = remove_sugar TmTru and fls = remove_sugar TmFal in
        UsApp (UsApp (new_t, UsAbs ("x", UsAbs ("y", fls))), tru)
  in

  (* Do debuggowania *)
  let rec unsugar_to_term (t : unsugar_term) : term =
    match t with
    | UsVar v -> TmVar v
    | UsOldVar v -> TmVar ("&" ^ v)
    | UsAbs (v, t) ->
        let new_t = unsugar_to_term t in
        TmAbs (v, new_t)
    | UsOldAbs (v, t) ->
        let new_t = unsugar_to_term t in
        TmAbs ("&" ^ v, new_t)
    | UsApp (t1, t2) ->
        let new_t1 = unsugar_to_term t1 and new_t2 = unsugar_to_term t2 in
        TmApp (new_t1, new_t2)
  in
  (* Removes names from variables *)
  let rec remove_names (t : unsugar_term) (gamma : int KeyMap.t) : raw_term =
    let add_elem elem map =
      let new_map = KeyMap.map (function n -> n + 1) map in
      KeyMap.update elem (function _ -> Some 0) new_map
    in
    match t with
    | UsVar v -> (
        match KeyMap.find_opt (KVar v) gamma with
        | Some n -> TrVar n
        | None -> failwith "free usvar")
    | UsOldVar v -> (
        match KeyMap.find_opt (KOldVar v) gamma with
        | Some n -> TrVar n
        | None -> TrId v)
    | UsAbs (v, t) ->
        let new_gamma = add_elem (KVar v) gamma in
        let new_t = remove_names t new_gamma in
        TrAbs new_t
    | UsOldAbs (v, t) ->
        let new_gamma = add_elem (KOldVar v) gamma in
        let new_t = remove_names t new_gamma in
        TrAbs new_t
    | UsApp (t1, t2) ->
        let new_t1 = remove_names t1 gamma and new_t2 = remove_names t2 gamma in
        TrApp (new_t1, new_t2)
  in

  let unsugar = remove_sugar t in
  remove_names unsugar KeyMap.empty

type 'a env = 'a list
type machine_value = Clo of raw_term * machine_value env
type stack = (raw_term * machine_value env) list

let print_debug v =
  print_string ("debug " ^ v);
  print_newline ();
  print_flush ()

let rec krivine (t : raw_term) env cont : raw_term * machine_value env * stack =
  match t with
  | TrVar n ->
      let (Clo (t', e')) = List.nth env n in
      krivine t' e' cont
  | TrAbs t' -> (
      match cont with
      | [] -> (t, env, cont)
      | (t'', env') :: cont' -> krivine t' (Clo (t'', env') :: env) cont')
  | TrApp (t1, t2) -> krivine t1 env ((t2, env) :: cont)
  | TrTemp _ | TrId _ -> (t, env, cont)

let rec is_beta_normal (t : raw_term) (e : machine_value env) =
  let rec a t e =
    match t with
    | TrTemp _ | TrId _ -> true
    | TrApp (t1, t2) -> a t1 e && is_beta_normal t2 e
    | TrVar n ->
        let (Clo (new_t, new_e)) = List.nth e n in
        a new_t new_e
    | _ -> false
  in
  match t with
  | TrAbs t -> is_beta_normal t (Clo (TrId "absurd", []) :: e)
  | TrVar n -> (
      match List.nth_opt e n with
      | Some (Clo (new_t, new_e)) -> is_beta_normal new_t new_e
      | None -> true)
  | _ -> a t e

let rec subtitute_from_env (t : raw_term) (e : machine_value env) =
  if e = [] then t
  else
    match t with
    | TrTemp _ | TrId _ -> t
    | TrVar n ->
        let (Clo (t', e')) = List.nth e n in
        subtitute_from_env t' e'
    | TrAbs t' -> TrAbs (subtitute_from_env t' e)
    | TrApp (t1, t2) ->
        let t1' = subtitute_from_env t1 e and t2' = subtitute_from_env t2 e in
        TrApp (t1', t2')

let rec remove_temp_variables t temp_num =
  match t with
  | TrTemp n -> if n = temp_num then TrVar n else t
  | TrId _ | TrVar _ -> t
  | TrAbs t' -> TrAbs (remove_temp_variables t' (temp_num + 1))
  | TrApp (t1, t2) ->
      let t1' = remove_temp_variables t1 temp_num
      and t2' = remove_temp_variables t2 temp_num in
      TrApp (t1', t2')

let rec create_temp_variables t temp_num =
  match t with
  | TrTemp _ | TrId _ -> t
  | TrVar n -> if n = temp_num then TrTemp n else t
  | TrAbs t' -> TrAbs (create_temp_variables t' (temp_num + 1))
  | TrApp (t1, t2) ->
      let t1' = create_temp_variables t1 temp_num
      and t2' = create_temp_variables t2 temp_num in
      TrApp (t1', t2')

let rec normalize t (e : machine_value env) =
  if is_beta_normal t e then subtitute_from_env t e
  else
    let new_t, env, cont = krivine t e [] in
    let normalized_t =
      match new_t with
      | TrAbs t' ->
          let temp_t = create_temp_variables t' 0 in
          let normalized_temp_t = normalize temp_t env in
          TrAbs (remove_temp_variables normalized_temp_t 0)
      | TrId _ | TrVar _ | TrTemp _ -> new_t
      | _ -> failwith "what?"
    in
    assert (is_beta_normal normalized_t []);
    let normalize_cont = List.map (function t, e -> normalize t e) cont in
    List.fold_left
      (fun acc elem -> TrApp (acc, elem))
      normalized_t normalize_cont

let beta_compare t1 t2 =
  let t1', e1', cont1 = krivine t1 [] []
  and t2', e2', cont2 = krivine t2 [] [] in
  if List.compare_lengths cont1 cont2 <> 0 then false
  else
    let normalized_t1 = normalize t1' e1'
    and normalized_t2 = normalize t2' e2' in
    if normalized_t1 <> normalized_t2 then false
    else
      List.fold_left2
        (fun acc (t1, e1) (t2, e2) ->
          let normalized_t1 = normalize t1 e1
          and normalized_t2 = normalize t2 e2 in
          acc && normalized_t1 = normalized_t2)
        true cont1 cont2
