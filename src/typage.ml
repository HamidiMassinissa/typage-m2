open Ast
open BuildTypeConstraints
open Unification
open PrettyPrinter

let main =
  let term2 = Lambda("x", App(Var "x", Var "x")) in
  let eqs2 = build_equational_system term2 in
  Printf.printf "%s\n" (tconstraint_to_string eqs2)

