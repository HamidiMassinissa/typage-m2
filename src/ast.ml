(** expressions à la curry *)

type term =
  | Const of constant
  | Var of variable
  | Pair of term * term
  | Lambda of variable * term
  | App of term * term
  | Let of variable * term * term

 and constant = string

 and variable = string

 and type_identifier = string

 and t = term
