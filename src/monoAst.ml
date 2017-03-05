(** type definition related to monomorphic typing algorithm *)

type tyscheme =
   | TVar of type_variable
   | TBase of type_identifier
   | TArrow of tyscheme * tyscheme
   | TProduct of tyscheme * tyscheme

 and type_variable = string

 and type_identifier = string

 and t = tyscheme

