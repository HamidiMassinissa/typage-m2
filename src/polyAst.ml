(** Polymorphism à la ML *)

(** expressions à la Curry *)

(** We have a subtle distinction in the type construction, like so:
      1. Type matrice:
           M ::= Τ | α | M × M | M → M

      2. Type
           S ::= ∀α₁...∀α.M

   where type matrice is a particular case of type 
 *)

(* type matrice would correspond to the following type definition
   which is the same as typescheme defined in ast.ml for monomor-
   phic types:
        type tymatrice =
          | TypeBase of type_identifier
          | TypeVar of type_variable
          | TypeProduct of tymatrice * tymatrice
          | TypeArrow of tymatrice * tymatrice
*)
open MonoAst

type polyty =
  TForAll of type_variable list * tyscheme 

 and t = polyty
