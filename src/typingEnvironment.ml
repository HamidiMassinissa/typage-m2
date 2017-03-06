open PolyAst
open MonoAst
open Ast

module OrderedTypeVariables = struct
  type t = type_variable
  let compare = String.compare
end

module TypeVariablesSet = Set.Make(OrderedTypeVariables)

module TypingEnvironment : sig
  type t
  type key = variable
  type value = polyty 
  val empty: t
  val bind: t -> key -> value -> t
  exception BindingNotFound
  val lookup: t -> key -> value 
  val map_assoc: (value -> value) -> t -> t
  val free_variables: t -> TypeVariablesSet.t 
  val generalize: t -> tyscheme -> value
end = struct
  type t = (variable * polyty) list

  type key = variable

  type value = polyty 

  let empty = []

  let bind env var tyvar =
    (* TODO occursCheck *)
    (var,tyvar)::env

  exception BindingNotFound
              
  let lookup env var =
    try
      List.assoc var env
    with Not_found -> raise BindingNotFound

  let map_assoc f env =
    List.map (fun (key,value) -> (key, f value)) env

  let rec tyscheme_free_variables tysch =
    match tysch with
    | TVar tv -> TypeVariablesSet.singleton tv
    | TBase _ -> TypeVariablesSet.empty
    | TArrow (ity,oty) | TProduct (ity,oty) ->
       let s1 = tyscheme_free_variables ity in
       let s2 = tyscheme_free_variables oty in
       TypeVariablesSet.union s1 s2

  let free_variables env =
    List.fold_left
      (fun set (_,TForAll (tvs,tys)) ->
        let s = tyscheme_free_variables tys in
        let tvs_set = TypeVariablesSet.of_list tvs in
        let fv_tys = TypeVariablesSet.diff s tvs_set in
        TypeVariablesSet.union set fv_tys 
      ) TypeVariablesSet.empty env

  let generalize (tyenv:t) (typ:tyscheme) : polyty =
    let typ_fv = tyscheme_free_variables typ in
    let env_fv = free_variables tyenv in
    let tvs = TypeVariablesSet.(elements (diff typ_fv env_fv)) in
    TForAll (tvs,typ)

end

