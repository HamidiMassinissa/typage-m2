(* this module implements type inference algorithm by Damas-Milner-Tofte *)
open Ast
open PolyAst
open MonoAst
open Unification
open PrettyPrinter
open TypingEnvironment
open Substitution

let polyStc : (constant * polyty) list =
  let bool = TBase "bool" in
  let int = TBase "int" in
  let a = TVar "α" in
  let b = TVar "β" in
  [
    ("1", TForAll ([], int));
    ("2", TForAll ([], int));
    ("true", TForAll ([], bool));
    ("+", TForAll ([], TArrow(TProduct(int,int),int)));
    ("fst", TForAll (["α";"β"],TArrow(TProduct(a,b),a)));
    ("snd", TForAll (["α";"β"],TArrow(TProduct(a,b),b)));
    ("ifthenelse", TForAll (["α"],TArrow(TProduct(TProduct(bool,a),a),a)));
    ("fix", TForAll (["α"], TArrow(TArrow(a,a),a)))
  ]

let fresh_instantiate_type_variable =
  let r = ref 0 in
  fun _ ->
  incr r;
  "ζ" ^ (string_of_int !r)

let fresh_type_variable =
  let r = ref 0 in
  fun t ->
  incr r;
  if !Options.verbose
  then "α(" ^ t ^ ")" ^ (string_of_int !r)
  else "α" ^ (string_of_int !r)

let instantiate (TForAll (tvs,tysch):polyty) : tyscheme =
  let rec aux tysch renamings =
    match tysch with
    | TBase tb as tysch -> renamings,tysch
    | TVar tv as tysch ->
       if (List.mem tv tvs)
       then
         (try
            renamings,TVar (List.assoc tv renamings)
          with Not_found ->
            let fresh_inst_var = fresh_instantiate_type_variable tv in
            let renamings = (tv,fresh_inst_var)::renamings in
            renamings,TVar fresh_inst_var)
       else renamings,tysch
    | TArrow (ity,oty) ->
       let renamings,ity' = aux ity renamings in
       let renamings,oty' = aux oty renamings in
       renamings,TArrow (ity',oty')
    | TProduct (aty,bty) ->
       let renamings,aty' = aux aty renamings in
       let renamings,bty' = aux bty renamings in
       renamings,TProduct (aty',bty')

    | TRec (binders,ty) ->
       let renamings,ty' = aux ty renamings in
       renamings,TRec (binders,ty)
  in
  snd (aux tysch [])

let most_general_unifier (TyEq(aty,bty)) =
  let rec unify aty bty =
    begin match (aty,bty) with
    | aty, bty when aty == bty -> (* erase *)
       Substitution.identity

    | TVar tv, bty -> (* bind *)
       (try
          Substitution.bind ~rectype:!Options.rectype Substitution.identity tv bty
        with
        | Substitution.OccurCheck (tv,tysch) ->
           (Printf.printf
             "Unification failed: type variable %s appears in type scheme %s\n"
             tv (typescheme_to_string tysch));
           failwith
             (Printf.sprintf
             "Unification failed: type variable %s appears in type scheme %s\n"
             tv (typescheme_to_string tysch)))

    | aty, TVar tv -> (* reorient *)
       Substitution.bind ~rectype:!Options.rectype Substitution.identity tv aty

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
let damasMilnerTofte ?(context) term =
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
  match context with
  | Some ctxt -> aux ctxt term
  | None -> aux TypingEnvironment.empty term
