open Ast
open Unification
open PrettyPrinter


(** We introduce a table of type schemes for constants, called STC, such that
   for example: STC(fst)=alpha*beta->alpha
*)
let stc : (constant * tyscheme) list = []

let type_variable_identifier s = s

let fresh_type_variable =
  let r = ref 0 in
  fun () ->
  incr r;
  "α" ^ (string_of_int !r)

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
       Printf.printf "%s : %s\n" (term_to_string prog) (tconstraint_to_string se);
       TyEq (alpha_m, ty_c)::se

    | Var v ->
       let v = fresh_type_variable () in
       Printf.printf "%s : %s\n" (term_to_string prog) (tconstraint_to_string se);
       TyEq (alpha_m, TypeVar v)::se

    | Pair (n, l) ->
       let alpha_n = TypeVar (fresh_type_variable ()) in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar (fresh_type_variable ()) in
       let se_l = aux l alpha_l se in
       Printf.printf "%s : %s\n" (term_to_string prog) (tconstraint_to_string se);
       TyEq (alpha_m, TypeProduct (alpha_n, alpha_l))
       :: se_n @ se_l

    | Lambda (x, n) ->
       let alpha_n = TypeVar (fresh_type_variable ()) in
       let se_n = aux n alpha_n se in
       let alpha_x = TypeVar (fresh_type_variable ()) in
       Printf.printf "%s : %s\n"
		     (term_to_string prog)
		     (tconstraint_to_string se_n);
       TyEq (alpha_m, TypeArrow (alpha_x, alpha_n))::se_n

    | App (n, l) ->
       let alpha_n = TypeVar (fresh_type_variable ()) in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar (fresh_type_variable ()) in
       let se_l = aux l alpha_l se in
       Printf.printf "%s : %s\n"
		     (term_to_string prog)
		     (tconstraint_to_string (se_n@se_l));
       TyEq (alpha_n, TypeArrow (alpha_l, alpha_m))
       :: se_n @ se_l

    | Let (x, n, l) ->
       let alpha_n = TypeVar (fresh_type_variable ()) in
       let se_n = aux n alpha_n se in
       let alpha_l = TypeVar (fresh_type_variable ()) in
       let se_l = aux l alpha_l se in
       let alpha_x = TypeVar (fresh_type_variable ()) in
       Printf.printf "%s : %s\n" (term_to_string prog) (tconstraint_to_string se);
       TyEq (alpha_m, alpha_l)
       :: TyEq (alpha_x, alpha_n)
       :: se_n @ se_l
  in
  let alpha_m = TypeVar (fresh_type_variable ()) in
  aux prog alpha_m []

