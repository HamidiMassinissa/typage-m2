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

(** type definition related to first order equational system

    in this system we have the following symetry between
    first order terms and type schemes

      1. TypeVar of term                       <==>   Var of variable

      2. TypeBase if type_identifier           <==>   Cons of constant

      3. TypeArrow of tyscheme * tyscheme   |  /__\
         TypeProduct of tyscheme * tyscheme |  \  /  Fun of fun_sym * term list

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
  let rec aux prog alpha_m se =
    match prog with
    | Const c ->
       let ty_c = try
	   List.assoc c stc
	 with Not_found ->
	   failwith (Printf.sprintf
		       "Build_equational_system: Unknown constant %s" c)
       in
       TyEq (alpha_m, ty_c)::se

    | Var v ->
       let v = fresh_type_variable in
       TyEq (alpha_m, TypeVar v)::se

    | Pair (n, l) ->
       let alpha_n = TypeVar fresh_type_variable in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar fresh_type_variable in
       let se_l = aux l alpha_l se in
       TyEq (alpha_m, TypeProduct (alpha_n, alpha_l))
       :: se_n @ se_l

    | Lambda (x, n) ->
       let alpha_n = TypeVar fresh_type_variable in
       let se_n = aux n alpha_n se in
       let alpha_x = TypeVar fresh_type_variable in
       TyEq (alpha_m, TypeArrow (alpha_x, alpha_n))::se_n

    | App (n, l) ->
       let alpha_n = TypeVar fresh_type_variable in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar fresh_type_variable in
       let se_l = aux l alpha_l se in
       TyEq (alpha_n, TypeArrow (alpha_l, alpha_m))
       :: se_n @ se_l

    | Let (x, n, l) ->
       let alpha_n = TypeVar fresh_type_variable in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar fresh_type_variable in
       let se_l = aux l alpha_l se in
       let alpha_x = TypeVar fresh_type_variable in
       TyEq (alpha_m, alpha_l)
       :: TyEq (alpha_x, alpha_n)
       :: se_n @ se_l
  in
  let alpha_m = TypeVar fresh_type_variable in
  aux prog alpha_m []

let eqs1 =
  let term1 = Let("x", Arrow("x", "x"), App("x", "x")) in
  build_equational_system term1
