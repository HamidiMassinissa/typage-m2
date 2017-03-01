open Ast
open Unification
open PrettyPrinter

module TypingEnvironment : sig
  type t
  type key = variable
  type value = type_variable
  val empty: t
  val bind: t -> key -> value -> t
  val lookup: t -> key -> value 
end = struct
  type t = (variable * type_variable) list
  type key = variable
  type value = type_variable
  let empty = []
  let bind env var tyvar =
    (var,tyvar)::env
  let lookup env var =
    List.assoc var env
end


(** We introduce a table of type schemes for constants, called STC, such that
   for example: STC(fst)=alpha*beta->alpha
*)
let stc : (constant * tyscheme) list =
  let bool = TypeBase "bool" in
  let int = TypeBase "int" in
  [
    ("+", TypeArrow (TypeProduct (int,int),int)]

let type_variable_identifier s = s

let fresh_type_variable =
  let r = ref 0 in
  fun t ->
  incr r;
  if !(Options.verbose)
  then "α(" ^ t ^ ")" ^ (string_of_int !r)
  else "α" ^ (string_of_int !r)

let build_equational_system prog =
  let rec aux prog alpha_m se tyenv =
    match prog with
    | Const c ->
       let ty_c = try
	   List.assoc c stc
	 with Not_found ->
	   failwith (Printf.sprintf
		       "Build_equational_system: Unknown constant %s" c)
       in
       let se = TyEq (alpha_m, ty_c)::se in
       Printf.printf "%s : %s\n"
                     (term_to_string prog)
                     (tconstraint_to_string [TyEq (alpha_m, ty_c)]);
       se 

    | Var v ->
       let tyvar = try
           TypingEnvironment.lookup tyenv v
         with Not_found ->
           fresh_type_variable v
       in
       let se = TyEq (alpha_m, TypeVar tyvar)::se in
       Printf.printf "%s : %s\n"
                     (term_to_string prog)
                     (tconstraint_to_string [TyEq (alpha_m, TypeVar tyvar)]);
       se 

    | Pair (n, l) ->
       let alpha_n = TypeVar (fresh_type_variable (term_to_string n)) in
       let se_n = aux n alpha_n se tyenv in
       let alpha_l = TypeVar (fresh_type_variable (term_to_string l)) in
       let se_l = aux l alpha_l se tyenv in
       let se = TyEq (alpha_m, TypeProduct (alpha_n, alpha_l)) :: se_n @ se_l in
       Printf.printf "%s : %s\n"
                     (term_to_string prog)
                     (tconstraint_to_string [TyEq (alpha_m, TypeProduct (alpha_n, alpha_l))]);
       se
       

    | Lambda (x, n) ->
       let fresh_x = fresh_type_variable x in
       let alpha_x = TypeVar fresh_x in
       let tyenv' = TypingEnvironment.bind tyenv x fresh_x in
       let alpha_n = TypeVar (fresh_type_variable (term_to_string n)) in
       let se_n = aux n alpha_n se tyenv' in
       let se = TyEq (alpha_m, TypeArrow (alpha_x, alpha_n))::se_n in
       Printf.printf "%s : %s\n"
		     (term_to_string prog)
		     (tconstraint_to_string [TyEq (alpha_m, TypeArrow (alpha_x, alpha_n))]);
       se 

    | App (n, l) ->
       let alpha_n = TypeVar (fresh_type_variable (term_to_string n)) in
       let se_n = aux n alpha_n se tyenv in
       let alpha_l = TypeVar (fresh_type_variable (term_to_string l)) in
       let se_l = aux l alpha_l se tyenv in
       let se = TyEq (alpha_n, TypeArrow (alpha_l, alpha_m)) :: se_n @ se_l in
       Printf.printf "%s : %s\n"
		     (term_to_string prog)
		     (tconstraint_to_string [TyEq (alpha_n, TypeArrow (alpha_l, alpha_m))]);
       se 

    | Let (x, n, l) ->
       let fresh_x = fresh_type_variable x in
       let alpha_x = TypeVar fresh_x in
       let tyenv' = TypingEnvironment.bind tyenv x fresh_x in
       let alpha_n = TypeVar (fresh_type_variable (term_to_string n)) in
       let se_n = aux n alpha_n se tyenv' in
       let alpha_l = TypeVar (fresh_type_variable (term_to_string l)) in
       let se_l = aux l alpha_l se tyenv' in
       let se =
         TyEq (alpha_m, alpha_l)
         :: TyEq (alpha_x, alpha_n)
         :: se_n @ se_l
       in
       Printf.printf "%s : %s\n"
                     (term_to_string prog)
                     (tconstraint_to_string [TyEq (alpha_m, alpha_l); TyEq (alpha_x, alpha_n)]);
       se 
  in
  let alpha_m = TypeVar (fresh_type_variable (term_to_string prog)) in
  aux prog alpha_m [] TypingEnvironment.empty

