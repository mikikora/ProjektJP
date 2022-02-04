open Syntax
open Support.Error

val unfold : term -> raw_term

type 'a env = 'a list
type machine_value = Clo of raw_term * machine_value env
type stack = (raw_term * machine_value env) list

val normalize : raw_term -> machine_value env -> raw_term
