(** Polymorphism à la ML *)

(** We have a subtle distinction in the type construction, like so:
      1. Type matrice:
           M ::= Τ | α | M × M | M → M | μ α.M (α ≠ M i.e. has to be contractive)

      2. Type
           S ::= ∀α₁...∀α.M

   where type matrice is a particular case of type 
 *)

type t = term
           
 (** expressions à la curry *)
 and term =
   | Const of constant
   | Var of variable
   | Pair of term * term
   | Lambda of variable * term
   | App of term * term
   | Let of variable * term * term

 and constant = string

 and variable = string

 and tyscheme =
   | TVar of type_variable
   | TBase of type_identifier
   | TArrow of tyscheme * tyscheme
   | TProduct of tyscheme * tyscheme
   | TRec of binders * tyscheme

 and type_variable = string

 and type_identifier = string

 and binders = type_variable list

(** type matrice would correspond to the following type definition
    which is the same as typescheme defined in ast.ml for monomor-
    phic types:
         type tymatrice =
           | TBase of type_identifier
           | TVar of type_variable
           | TProduct of tymatrice * tymatrice
           | TArrow of tymatrice * tymatrice
*)

 and polyty =
   TForAll of type_variable list * tyscheme 
