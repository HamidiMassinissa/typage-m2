open MonoAst
open PolyAst
open TypingEnvironment
open PrettyPrinter

module Substitution : sig
  type t
  type substitution
  val identity: substitution
  val apply: substitution -> tyscheme -> tyscheme 
  val apply_to_polyty: substitution -> polyty -> polyty
  val apply_to_env: substitution -> TypingEnvironment.t -> TypingEnvironment.t
  val compose: substitution -> substitution -> substitution
  val bind: substitution -> type_variable -> tyscheme -> substitution
  val print: substitution -> unit 
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
  (* SubstitutionMap.union asubst bsubst' since OCaml 4.03 *)
    SubstitutionMap.merge
      (fun key a b ->
        match a,b with
        | Some a, Some b -> Some a
        | Some a, None -> Some a
        | None, Some b -> Some b
        | _, _ -> None
      ) asubst bsubst'
    
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

  let print subst =
    SubstitutionMap.iter
      (fun tv tysch ->
        Printf.printf "%s ↦ %s\n" tv (typescheme_to_string tysch)) subst
    
end
