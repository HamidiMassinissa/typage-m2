(** implementation of unification algorithm *)

type t = term
 and term =
   | Cons of constant
   | Var of variable
   | Fun of fun_sym  * term list

 and constant = string

 and variable = string

 and fun_sym = string

type tconstraint =
  Equality of term * term

(** An equational system is a set of equations of the form s = t *)
type equational_system = tconstraint list

type substitution = (variable * term) list

let variables_of_term (t:term) : variable list =
  let rec aux acc t =
    match t with
      Cons c -> acc
    | Var v -> v::acc
    | Fun (f, ts) -> List.fold_left aux acc ts
  in
  aux [] t

(** The Unification Algorithm [unify]

  (i)   Take an equational system E

  (ii)  Compute a new system P by applying the transformation rules
        as far as possible

  (iii) If the system P is in solved form
        - then send the answer Psubst (the substitution {x1/t1, ... , xn/tn}
        - else fail

 *)
let rec unify (e:equational_system) : substitution =
  match e with
    [] -> []
  | Equality (t1, t2)::t ->
     match t1, t2 with
     | t1, t2 when t1 = t2 -> (* erase *)
        (* we check structural equality with '=' *)
        unify t

     | t1, Var x -> (* orient *)
        (* TODO find a way to ensure that t1 is not a variable *)
        unify (Equality (Var x, t1)::t)

     | Fun (f1,l1), Fun (f2,l2) -> (* decompose *)
        let len_1 = List.length l1
        and len_2 = List.length l2 in
        if (len_1<>len_2) || (f1 <> f2)
        then failwith "decompose rule: Terms have different arities"
        else
          let enriched_e =
            List.fold_left2 (fun acc e1 e2 -> Equality (e1, e2)::acc) t l1 l2
          in
          unify enriched_e

     | Var x as t1, t2 -> (* replace *)
        (* check whether x belongs to Var(E) -Done by pattern matching ... *)
        (* ... and x doesn't belong to Var(t2) *)
        let vars_of_t2 = variables_of_term t2 in
	[]

     | _, _ -> (* No rule is applicable *)
        unify t

(** The equational system E={s1=t1, ... , sn=tn} is in solved form iff
  (i)  All the si are distinct variables
  (ii) No si appears in tj (in other words, no variables appear in
       substitution's image, then no cycle is permitted
 *)
let solved_form equational_system = true

let solve e =
  let s = unify e in
  if (solved_form s) then s
  else failwith "The equational system does not have a solution"


let fake_eq_sys = [ Equality(
                        Fun ("f",[Var "x"; Fun("h", [Cons "b"]); Cons "c"]),
                        Fun ("f",[Fun("g",[Var "y"]); Var "y"; Cons "c"]));
                    Equality(Var "x", Cons "c")]

let _ = solve fake_eq_sys



(** deprecated [to_substitution] *)
let to_substitution equational_system : substitution =
  List.map
    (fun (Equality(t1, t2)) ->
      match t1 with
      | Var s -> (s, t2)
      | _ -> assert false)
    equational_system
