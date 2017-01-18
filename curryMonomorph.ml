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
   | TypeVar of term
   | TypeBase of type_identifier
   | TypeArrow of tyscheme * tyscheme
   | TypeProduct of tyscheme * tyscheme



(** type definition related to first order equational system

    in this system we have the following symetry between
    first order terms and type schemes

      1. TypeVar of term                       <==>   Var of variable

      2. TypeBase if type_identifier           <==>   Cons of constant

      3. TypeArrow of tyscheme * tyscheme   | /____\
         TypeProduct of tyscheme * tyscheme | \    /  Fun of fun_sym * term list

*)
type tyconstraint = TyEq of tyscheme * tyscheme
 and equational_system = ty list


(** We introduce a table of type schemes for constants, called STC, such that
   for example: STC(fst)=alpha*beta->alpha
*)
let stc : (constant * tyscheme) list = []

let type_variable_identifier s = s

let fresh_type_variable =
  let r : int ref = ref 0 in
  "alpha" ^ (string_of_int !r)

let build_equational_system prog =
  let rec aux prog se =
    match prog with
    | Const c ->
       let ty_c =
	 try
	   List.assoc c stc
	 with
	   Not_found ->
	   failwith (Printf.sprintf
		       "Build_equational_system: Unknown constant %s" c)
       in
       TyEq (TypeVar prog, ty_c)::se

    | Var v ->
       TyEq (TypeVar prog, TypeVar prog)::se

    | Pair (t1, t2) ->
       let se_t1 = aux t1 se
       and se_t2 = aux t2 se in
       TyEq (TypeVar prog, TypeProduct (TypeVar t1, TypeVar t2))
       :: se_t1 @ se_t2

    | Lambda (v, t) ->
       let se_t = aux t se in
       TyEq (TypeVar prog, TypeArrow (TypeVar v, TypeVar t))::se_t

    | App (t1, t2) ->
       let se_t1 = aux t1 se
       and se_t2 = aux t2 se in
       TyEq (TypeVar t1, TypeArrow (TypeVar t2, TypeVar prog))
       :: se_t1 @ se_t2

    | Let (v, t1, t2) ->
       let se_t1 = aux t1 se
       and se_t2 = aux t2 se in
       TyEq (TypeVar prog, TypeVar t2)
       :: TyEq (TypeVar v, TypeVar t1)
       :: se_t1 @ se_t2
  in
  aux prog []
