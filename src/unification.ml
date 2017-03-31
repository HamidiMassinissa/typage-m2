(* this module implements a unification algorithm *)
open Ast
open MonoAst

(** type definition related to first order equational system

    in this system we have the following symetry between
    first order terms and type schemes

      1. TypeVar of type_variable              <==>   Var of variable

      2. TypeBase if type_identifier           <==>   Cons of constant

      3. TypeArrow of tyscheme * tyscheme   |  /__\
         TypeProduct of tyscheme * tyscheme |  \  /  Fun of fun_sym * term list

*)
type tyconstraint = TyEq of tyscheme * tyscheme
 and equational_system = tyconstraint list
