(* recursive types *)

type ty =
  | Tybase of type_identifier
  | Tyvar of type_variable
  | Tybind of type_variable * ty
  | Typroduct of ty * ty
  | Tyarrow of ty * ty
 and type_identifier = string
 and type_variable = string

let rec apply_subst term subst bind =
  match term with
  | Tybase _ -> term
  | Tyvar x ->
     Tyvar (List.assoc subst bind)
  | Tybind (x,ty) ->
     Tybind (x, (List.assoc subst bind))
  | Typroduct (ty1,ty2) ->
     let ty1' = apply_subst ty1 subst bind
     and ty2' = apply_subst ty2 subst bind in
     Typroduct (ty1', ty2')
  | Tyarrow (ty1,ty2) ->
     let ty1' = apply_subst ty1 subst bind
     and ty2' = apply_subst ty2 subst bind in
     Tyarrow (ty1', ty2')

let unfold ty =
  match ty with
    Tybind (x',ty') ->
    let subst = (x',ty')::[] in
    apply_subst ty' subst x'
  | _ -> ty

let rec tree_of ty pi =
  match ty with
    Tybase t when pi=[] -> t

  | Tybase t -> assert false

  | Tybind (x,ty) ->
     let ty' = unfold ty x in
     tree_of ty' pi

  | Typroduct (ty1,ty2) when pi=[] -> "Product"

  | Typroduct (ty1,ty2) ->
     (match pi with
     | "1"::tail -> tree_of ty1 pi
     | "2"::tail -> tree_of ty2 pi
     | _ -> failwith "Arity mismatch")

  | Tyarrow (ty1,ty2) when pi=[] -> "Arrow"

  | Tyarrow (ty1,ty2) ->
     (match pi with
     | "1"::tail -> tree_of ty1 pi
     | "2"::tail -> tree_of ty2 pi
     | _ -> failwith "Arity mismatch")

  | _ -> failwith "tree_of is not defined for open terms"

let rectype =
  Tyarrow(Tyvar "int",
	  Tybind("x", Tybind("y", (Tybind ("z",
					   Tyarrow(Tyvar "int", Tyvar "int"))))))

let app = tree_of rectype ["2"]

let type1 = Tybind ("x", (Tyarrow (Tyvar "x", Tyvar "x")))
let type1' = tree_of type1 ["1"]
let unfold' = unfold type1 "x"
