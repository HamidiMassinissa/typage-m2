(* this module implements type inference algorithm by Damas-Milner-Tofte *)
open Ast
open PolyAst
open MonoAst
open Unification
open PrettyPrinter

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

module Substitution : sig
  type t
  type substitution
  val identity: substitution
  val apply: substitution -> tyscheme -> tyscheme 
  val apply_to_polyty: substitution -> polyty -> polyty
  val apply_to_env: substitution -> TypingEnvironment.t -> TypingEnvironment.t
  val compose: substitution -> substitution -> substitution
  val bind: substitution -> type_variable -> tyscheme -> substitution
end = struct

  module OrderedSubstitution = struct
    type t = type_variable
    let compare = String.compare
  end

  module SubstitutionMap = Map.Make(OrderedSubstitution)

  type substitution = tyscheme SubstitutionMap.t 

  type t = substitution

  let identity = SubstitutionMap.empty 

  let rec apply subst tysch =
    match tysch with
    | TVar tv ->
       (try
         SubstitutionMap.find tv subst
       with Not_found -> tysch)
    | TBase tb -> tysch
    | TArrow (ity,oty) ->
       let ity' = apply subst ity in
       let oty' = apply subst oty in
       TArrow (ity',oty')
    | TProduct (aty,bty) ->
       let aty' = apply subst aty in
       let bty' = apply subst bty in
       TProduct (aty',bty')

  let apply_to_polyty subst (TForAll (vs,tysch)) =
    let tysch' = apply subst tysch in
    let vs' =
      List.filter
        (*(fun tv -> List.(mem tv (fst (split subst)))) vs*)
        (fun tv -> SubstitutionMap.mem tv subst) vs
    in
    TForAll (vs', tysch')

  let apply_to_env subst env =
    TypingEnvironment.map_assoc
      (fun pty -> apply_to_polyty subst pty) env

  let apply_to_subst subst s =
    SubstitutionMap.map (fun tysch -> apply subst tysch) s

  let compose asubst bsubst =
    let bsubst' = apply_to_subst asubst bsubst in
    SubstitutionMap.union asubst bsubst'
    
  exception InvalidBinding of string

  let bind subst tv tysch =
    (*let rec mem subst tv =
      List.exists (fun (tv',_) -> tv = tv') subst*)

    let rec occurs_check tv tysch =
      match tysch with
      | TVar tv' when tv = tv' ->
         raise (InvalidBinding
                  (Printf.sprintf "type variable %s occurs in type %s"
                                  tv (typescheme_to_string tysch)))
      | TArrow (ity,oty) ->
         occurs_check tv ity;
         occurs_check tv oty
      | TProduct (aty,bty) ->
         occurs_check tv aty;
         occurs_check tv bty
      | TBase _ | TVar _ -> ()

    in
    if (SubstitutionMap.mem tv subst)
    then raise (InvalidBinding
                  (Printf.sprintf "type variable %s already binded" tv))
    else
      begin
        occurs_check tv tysch;
        SubstitutionMap.add tv tysch subst
      end
    
end

let polyStc : (constant * polyty) list =
  let bool = TBase "bool" in
  let int = TBase "int" in
  let a = TVar "α" in
  let b = TVar "β" in
  [
    ("+", TForAll ([], TArrow(TProduct(int,int),int)));
    ("fst", TForAll (["α";"β"],TArrow(TProduct(a,b),a)));
    ("snd", TForAll (["α";"β"],TArrow(TProduct(a,b),b)));
    ("ifthenelse", TForAll (["α"],TArrow(TProduct(TProduct(bool,a),a),a)));
    ("fix", TForAll (["α"], TArrow(TArrow(a,a),a)))
  ]

let fresh_type_variable =
  let r = ref 0 in
  fun t ->
  incr r;
  if !(Options.verbose)
  then "α(" ^ t ^ ")" ^ (string_of_int !r)
  else "α" ^ (string_of_int !r)

let instantiate (TForAll (tvs,pty):polyty) : tyscheme =
  let rec aux pty =
    match pty with
    | TBase tb -> pty
    | TVar tv ->
       if (List.mem tv tvs)
       then TVar (fresh_type_variable tv)
       else pty
    | TArrow (ity,oty) ->
       let ity' = aux ity in
       let oty' = aux oty in
       TArrow (ity',oty')
    | TProduct (aty,bty) ->
       let aty' = aux aty in
       let bty' = aux bty in
       TProduct (aty',bty')
  in
  aux pty

let most_general_unifier (TyEq (aty,bty)) =
  let rec unify aty bty =
    begin match (aty,bty) with
    | aty, bty when aty == bty -> (* erase *)
       Printf.printf "type equality encountred %s %s\n"
                     (typescheme_to_string aty) (typescheme_to_string bty);
       Substitution.identity

    | TVar tv, bty -> (* bind *)
       Substitution.bind (Substitution.identity) tv bty

    | aty, TVar tv -> (* reorient *)
       Substitution.bind (Substitution.identity) tv aty

    | TArrow (aity,aoty), TArrow (bity,boty) -> (* decompose *)
       let subst1 = unify aity bity in
       let aoty' = Substitution.apply subst1 aoty in
       let boty' = Substitution.apply subst1 boty in
       let subst2 = unify aoty' boty' in
       Substitution.compose subst2 subst1

    | TProduct (aty1,bty1), TProduct (aty2,bty2) -> (* decompose *)
       let subst1 = unify aty1 aty2 in
       let bty1' = Substitution.apply subst1 bty1 in
       let bty2' = Substitution.apply subst1 bty2 in
       let subst2 = unify bty1' bty2' in
       Substitution.compose subst2 subst1

    | aty, bty ->
       failwith
         (Printf.sprintf "Unification failed: Couldn't unify %s with %s\n"
                         (typescheme_to_string aty) (typescheme_to_string bty))
    end
  in
  unify aty bty

(** algorithm specification
    Input: An environment Γ and a term M such that FV(M) ⊆ Γ.
    Output: A type A and a substitution σ such that σ(Γ) ⊢ M:A.

    OCaml type of damasMilnerTofte auxilary function:
         [aux]: term -> TypingEnvironment.t -> tyscheme
 *)
let damasMilnerTofte term =
  let rec aux context term =
    match term with
    | Var x ->
       let typeA = try
           TypingEnvironment.lookup context x
         with TypingEnvironment.BindingNotFound ->
           failwith (Printf.sprintf "Unbound variable %s" x)
       in
       instantiate typeA, Substitution.identity

    | Const c ->
       let typeA = try
           List.assoc c polyStc
         with Not_found ->
           failwith (Printf.sprintf "Unknown constant %s" c)
       in
       instantiate typeA, Substitution.identity

    | Lambda (x,n) ->
       let alpha = TVar (fresh_type_variable x) in
       let context' = TypingEnvironment.bind context x (TForAll ([],alpha)) in
       let (typeB,ro) = aux context' n in
       let alpha' = Substitution.apply ro alpha in
       (* base cases of this algorithm call [instantiate]
          which returns a matrice type (i.e. a type without
          binders) so, length of vs should be null.
              assert(List.length vs == 0);
        *)
       TArrow (alpha', typeB), ro

    | App (n,l) ->
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let (typeC,roC) = aux context' l in
       let alpha = TVar (fresh_type_variable "AppRule") in
       let typeB' = Substitution.apply roC typeB in
       let mu = most_general_unifier (TyEq (typeB',TArrow (typeC, alpha))) in
       (Substitution.apply mu alpha,
        Substitution.compose (Substitution.compose mu roC) roB)

    | Pair (n,l) ->
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let (typeC,roC) = aux context' l in
       let typeB' = Substitution.apply roC typeB in
       (TProduct (typeB', typeC), Substitution.compose roC roB)
 
    | Let (x,n,l) ->
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let genB = TypingEnvironment.generalize context' typeB in
       let context'' = TypingEnvironment.bind context x genB in
       let (typeC,roC) = aux context'' l in
       (typeC, Substitution.compose roC roB)

  in
  aux TypingEnvironment.empty term
