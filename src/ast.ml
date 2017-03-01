(** typage à la curry *)

type term =
  | Const of constant
  | Var of variable
  | Pair of term * term
  | Lambda of variable * term
  | App of term * term
  | Let of variable * term * term
 and constant = string
 and variable = string

 and ty =
   | TyBase of type_identifier
   | TyProduct of ty * ty
   | TyArrow of ty * ty

 and type_identifier = string

 (** type definition related to curry typing algorithm *)
 and tyscheme =
   | TypeVar of type_variable
   | TypeBase of type_identifier
   | TypeArrow of tyscheme * tyscheme
   | TypeProduct of tyscheme * tyscheme

 and type_variable = string

 and t = term
