(* this module implements a unification algorithm *)

open Ast
open BuildTypeConstraints

let rec typescheme_to_string t =
  match t with
  | TypeVar tv -> tv
  | TypeBase tid -> tid
  | TypeArrow (tscha,tschb) ->
     typescheme_to_string tscha ^ " → " ^
       typescheme_to_string tschb
  | TypeProduct (tscha,tschb) ->
     typescheme_to_string tscha ^ " × " ^
       typescheme_to_string tschb

let tconstraint_to_string t =
  let aconstraint = function
    | TyEq (a, b) ->
      Printf.sprintf "%s =?= %s"
        (typescheme_to_string a)
        (typescheme_to_string b)
  in
  "[\n" ^ String.concat "\n" (List.map aconstraint t) ^ "\n]"

let term_to_string t = ""
