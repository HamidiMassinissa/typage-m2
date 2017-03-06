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
       Printf.printf "%s\n" (term_to_string term);
       let typeA = try
           TypingEnvironment.lookup context x
         with TypingEnvironment.BindingNotFound ->
           failwith (Printf.sprintf "Unbound variable %s" x)
       in
       instantiate typeA, Substitution.identity

    | Const c ->
       Printf.printf "%s\n" (term_to_string term);
       let typeA = try
           List.assoc c polyStc
         with Not_found ->
           failwith (Printf.sprintf "Unknown constant %s" c)
       in
       instantiate typeA, Substitution.identity

    | Lambda (x,n) ->
       Printf.printf "%s\n" (term_to_string term);
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
       Printf.printf "%s\n" (term_to_string term);
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let (typeC,roC) = aux context' l in
       let alpha = TVar (fresh_type_variable "AppRule") in
       let typeB' = Substitution.apply roC typeB in
       let mu = most_general_unifier (TyEq (typeB',TArrow (typeC, alpha))) in
       (Substitution.apply mu alpha,
        Substitution.compose (Substitution.compose mu roC) roB)

    | Pair (n,l) ->
       Printf.printf "%s\n" (term_to_string term);
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let (typeC,roC) = aux context' l in
       let typeB' = Substitution.apply roC typeB in
       (TProduct (typeB', typeC), Substitution.compose roC roB)
 
    | Let (x,n,l) ->
       Printf.printf "%s\n" (term_to_string term);
       let (typeB,roB) = aux context n in
       let context' = Substitution.apply_to_env roB context in
       let genB = TypingEnvironment.generalize context' typeB in
       let context'' = TypingEnvironment.bind context x genB in
       let (typeC,roC) = aux context'' l in
       (typeC, Substitution.compose roC roB)

  in
  aux TypingEnvironment.empty term
