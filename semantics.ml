type program =
  | Var of variable
  | Const of constant
  | Pair of program * program
  | App of program * program
  | Lambda of typed_variable * program
  | Let of typed_variable * program * program

 and ty =
   | TyBase of tybase
   | TyProduct of ty * ty
   | TyArrow of ty * ty
                       
 and tybase = Int | Bool
                      
 and variable = string

 and typed_variable = VarTyped of variable * ty
                              
 and constant = string

module StringSet = Set.Make(String)
    
let rec free_variables prog =
  match prog with
  | Var s ->
  (* StringSet.add s StringSet.empty *)
     StringSet.singleton s
                   
  | Const _ ->
     StringSet.empty
       
  | Pair (p1, p2) | App (p1, p2) ->
     let l1 = free_variables p1 
     and l2 = free_variables p2 in
     StringSet.union l1 l2
                     
  | Lambda (tv, p) ->
     let l = free_variables p in
     let VarTyped (s, ty) = tv in
     StringSet.remove s l
                      
  | Let (tv, p1, p2) ->
     let l1 = free_variables p1
     and l2 = free_variables p2 in
     let VarTyped (s, _) = tv in
     StringSet.union l1 (StringSet.remove s l2)

(** useless by the way *)
let rec bound_variables prog =
  match prog with
  | Var _ | Const _ ->
     StringSet.empty
  | Pair (p1, p2) | App (p1, p2) ->
     let b1 = bound_variables p1
     and b2 = bound_variables p2 in
     StringSet.union b1 b2
  | Lambda (tv, p) ->
     let b = bound_variables p
     and VarTyped(s, _) = tv in
     StringSet.add s b
  | Let (tv, p1, p2) ->
     let b1 = bound_variables p1
     and b2 = bound_variables p2
     and VarTyped (s, _) = tv in
     StringSet.union b1 (StringSet.add s b2)

let rec alpha_conversion prog = prog

let alpha_equivalence prog prog' = true

let variable_clashes p subst =
  let subst_freed = List.map free_variables subst in
  let free_vars = List.fold_left StringSet.union StringSet.empty subst_freed
  and bound_vars = bound_variables p in
  StringSet.inter free_vars bound_vars
  
let rec apply_substitution prog subst =
  match prog with
  | Var s as v ->
     begin
       try
         List.assoc s subst
       with
         Not_found -> v
     end
  | Const s as c -> c
  | Pair (p1, p2) ->
     let p1_subst = apply_substitution p1 subst
     and p2_subst = apply_substitution p2 subst in
     Pair (p1_subst, p2_subst)
  | App (p1, p2) ->
     let p1_subst = apply_substitution p1 subst
     and p2_subst = apply_substitution p2 subst in
     App (p1_subst, p2_subst)
  | Lambda (tv, p) ->
  (** here we have to check if no variable is captured
   * So before we apply the substitution [subst], we
   * need to check that no free variable in [subst]
   * clashes with a bound variable in [p]
   *)
     let clashes = variable_clashes p (List.(snd (split subst))) in
     let p_subst =
       if (StringSet.is_empty clashes)
       then apply_substitution p subst
       else (** alpha_conversion *)
         (** rename the bound variables in [p] so that we avoid
          * capturing free variables from [subst] *)
         let p_alpha_converted = alpha_conversion p in
         apply_substitution p_alpha_converted subst
     in
     Lambda (tv, p_subst)
  | Let (tv, p1, p2) ->
     (** ... also here *)
     let p1_subst = apply_substitution p1 subst in
     let VarTyped (s, _) = tv in
     let subst' = (s, p1_subst)::subst in
     let clashes = variable_clashes p2 (List.(snd (split subst))) in
     let p2_subst =
       if (StringSet.is_empty clashes)
       then apply_substitution p2 subst'
       else (** alpha_conversion *)
         let p2_alpha_converted = alpha_conversion p2 in
         apply_substitution p2_alpha_converted subst'
     in
     Let (tv, p1_subst, p2_subst) 
     

let rec bigstep_eval prog =
  match prog with
  | Var s -> ()
  | Const s -> ()
  | Pair (p1, p2) ->
     (** *)
     ()
  | App (p1, p2) ->
     (** *)
     ()
  | Lambda (x, p1) -> ()
  | Let (x, p1, p2) -> ();;


let type_constant : (constant * ty) list = []

let environment : (variable * ty) list = []

let rec curry_monomorph env term =
  match term with
  | Var s ->
     begin
       try
         List.assoc s env
       with
         Not_found -> failwith "Unbound variable"
     end
       
  | Const s ->
     begin
       try
         List.assoc s type_constant
       with
         Not_found -> failwith "Unknown constant"
     end
     
  | Pair (p1, p2) ->
     let ty_p1 = curry_monomorph env p1 in
     let ty_p2 = curry_monomorph env p2 in
     TyProduct(ty_p1, ty_p2)
              
  | App (p1, p2) ->
     let ty_p1 = curry_monomorph env p1 in
     begin match ty_p1 with
     | TyArrow (tyA, tyB) ->
        let ty_p2 = curry_monomorph env p2 in
        if ty_p2 == tyA
        then tyB
        else failwith "Argument type mismatch in application"
     | _ -> failwith "Not an application"
     end
                        
  | Lambda (tv, p) ->
     let VarTyped (s, ty_v) = tv in
     let tyB = curry_monomorph ((s, ty_v)::env) p in
     TyArrow(ty_v, tyB)
        
  | Let (v, p1, p2) ->
     let VarTyped (s, tyA) = v in
     let tyA_expected = curry_monomorph env p1 in
     if tyA == tyA_expected
     then curry_monomorph ((s, tyA)::env) p2
     else failwith "Given type does not match type of expression"
