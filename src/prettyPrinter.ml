open Ast
open MonoAst
open PolyAst
open Unification


let rec typescheme_to_string t =
  match t with
  | TVar tv -> tv
  | TBase tid -> tid
  | TArrow (tscha,tschb) ->
     "(" ^ typescheme_to_string tscha ^ " → " ^
       typescheme_to_string tschb ^ ")"
  | TProduct (tscha,tschb) ->
     "(" ^ typescheme_to_string tscha ^ " × " ^
       typescheme_to_string tschb ^ ")"

let polytype_to_string pty =
  let aux (TForAll (tvs,tys)) =
    let tvs =
      if List.length tvs = 0
      then ""
      else
        List.fold_left (fun acc tv -> "∀" ^ tv ^ acc) "" tvs ^ "."
    in
    tvs ^ typescheme_to_string tys
  in
  aux pty

let tconstraint_to_string t =
  let aconstraint = function
    | TyEq (a, b) ->
      Printf.sprintf "%s =?= %s"
        (typescheme_to_string a)
        (typescheme_to_string b)
  in
  "[\n" ^ String.concat "\n" (List.map aconstraint t) ^ "\n]"

let rec term_to_string t =
  match t with
  | Const c -> c
  | Var v -> v
  | Pair (n,l) ->
     "<" ^ term_to_string n ^ "," ^ term_to_string l ^ ">"
  | Lambda (x,n) ->
     "λ" ^ x ^ "." ^ term_to_string n
  | App (n,l) ->
     "(" ^ term_to_string n ^ ") (" ^ term_to_string l ^ ")"
  | Let (x,n,l) ->
     "let " ^ x ^ "=" ^ term_to_string n ^ " in " ^ term_to_string l
