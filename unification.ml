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
  aux [] t;;

let replace_and_check (eqs: equational_system) (var: variable) (term: term) =
  let r = ref false in
  let rec aux t =
    match t with
    | Cons _ -> t
    | Var x when x = var ->
       r := true; term
    | Var x -> t
    | Fun (f,ts) ->
       let ts' =
	 List.map (fun t -> aux t) ts
       in
       Fun (f,ts')
  in
  !r,
  List.map
    (fun (Equality (s,t)) ->
     let s' = aux s in
     let t' = aux t in
     Equality (s', t')) eqs

exception Found_variable

let f term var =
  let rec aux =
    function
    | Var x when x=var -> true
    | Fun (_,ts) -> List.for_all aux ts
    | Var _ | Cons _ -> false
  in
  aux term

let belongs_to_var (eqs:equational_system) var =
  List.for_all
    (fun
	(Equality (s,t)) ->
	  let s' = f s var in
	  let t' = f t var in
	  s'&&t'
	) eqs

let rec occur_check term var =
  f term var

let replace eqs var term =
  let rec aux =
    function
    | Cons _ as t-> t
    | Var x when x=var -> term
    | Var x as t -> t
    | Fun (f,ts) ->
       let ts' = List.map (fun t -> aux t) ts in
       Fun (f,ts')
  in
  List.map
    (fun
	(Equality (s,t)) ->
	 let s' = aux s in
	 let t' = aux t in
	 Equality (s',t')
	) eqs

(** The Unification Algorithm [unify]

  (i)   Take an equational system E

  (ii)  Compute a new system P by applying the transformation rules
        as far as possible

  (iii) If the system P is in solved form
        - then send the answer Psubst (the substitution {x1/t1, ... , xn/tn}
        - else fail

 *)
let rec unify (e:equational_system) : equational_system =
  match e with
    [] -> []
  | (Equality (t1, t2) as eq)::eqs ->
     match t1, t2 with
     | t1, t2 when t1 = t2 -> (* erase *)
        (* we check structural equality with '=' *)
        unify eqs

     | t1, Var x -> (* orient *)
        (* TODO find a way to ensure that t1 is not a variable *)
        unify (Equality (Var x, t1)::eqs)

     | Fun (f1,l1), Fun (f2,l2) -> (* decompose *)
        let len_1 = List.length l1
        and len_2 = List.length l2 in
        if (len_1<>len_2) || (f1 <> f2)
        then failwith "decompose rule: Terms have different arities"
        else
          let enriched_e =
            List.fold_left2 (fun acc e1 e2 -> Equality (e1, e2)::acc) eqs l1 l2
          in
          unify enriched_e

     | Var x, s when belongs_to_var eqs x -> (* replace: EU{x=s} *)
	let occur = occur_check s x in
	if (occur)
	then
	  failwith (Printf.sprintf
	       "replace rule: occur check -
		variable %s appears in the right term" x)
	else
	  let eqs' = replace eqs x s in
	  unify eqs'@[eq]

     | _, _ -> (* No rule is applicable *)
        unify eqs

(* [f(x, h(b), c) = f(g(y), y, c); x = c] *)
let eq0 = [ Equality(
                Fun ("f",[Var "x"; Fun("h", [Cons "b"]); Cons "c"]),
                Fun ("f",[Fun("g",[Var "y"]); Var "y"; Cons "c"]));
            Equality(Var "x", Cons "c")]
let eq0' = unify eq0

(* [x=a; y=b] *)
let eq1 = [ Equality(Var "x", Cons "a"); Equality(Var "y", Cons "b") ]
let eq1' = unify eq1

(* [x=g(y); y=b] *)
let eq2 = [ Equality(Var "x", Fun("g", [Var "y"]));
	    Equality(Var "y", Cons "b") ]
let eq2' = unify eq2


(** The equational system E={s1=t1, ... , sn=tn} is in solved form iff
  (i)  All the si are distinct variables
  (ii) No si appears in tj (in other words, no variables appear in
       substitution's image, then no cycle is permitted
 *)
let solved_form equational_system = true

let to_substitution equational_system : substitution =
  List.map
    (fun (Equality(t1, t2)) ->
      match t1 with
      | Var s -> (s, t2)
      | _ -> failwith "The given equational system is not in resolved form!")
    equational_system

let solve e =
  let s = unify e in
  if (solved_form s) then s
  else failwith "The equational system does not have a solution"

let _ = solve eq0
