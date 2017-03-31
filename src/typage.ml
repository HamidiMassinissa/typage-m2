open Ast
open PolyTypeInference
open PrettyPrinter
open Substitution

let main =
  CommandLineHandler.parse_command_line_arguments;

  let polyterm0 = Lambda("x",App(Var "x",Var "x")) in
  let ty,subst = damasMilnerTofte polyterm0 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm0)
                (typescheme_to_string ty);
 
  let polyterm0 =
    Lambda("f",
           App(
               Lambda("x", App(Var "f",(App(Var "x",Var "x")))),
               Lambda("x", App(Var "f",(App(Var "x",Var "x"))))))
  in
  let ty,subst = damasMilnerTofte polyterm0 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm0)
                (typescheme_to_string ty);
    

  let polyterm5 = Lambda("x",Var "x") in
  let ty, subst = damasMilnerTofte polyterm5 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm5)
                (typescheme_to_string ty);

  let polyterm4 = Lambda("x", Lambda("y", App(Var "x", Var "y"))) in
  let ty, subst = damasMilnerTofte polyterm4 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm4)
                (typescheme_to_string ty);

  let polyterm3 = Let("f", Lambda("x", Var "x"), App(Var "f", Const "1")) in
  let ty, subst = damasMilnerTofte polyterm3 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm3)
                (typescheme_to_string ty);

  (*
  let term2 = Lambda("x", App(Var "x", Var "x")) in
  let eqs2 = build_equational_system term2 in
  Printf.printf "%s\n%s\n"
                (term_to_string term2)
                (tconstraint_to_string eqs2);
   *)

  let polyterm1 = Lambda("x", App(Const "+", Pair (Var "x", Var "x"))) in
  let ty, subst = damasMilnerTofte polyterm1 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm1)
                (typescheme_to_string ty);

  let polyterm2 = Lambda("x", Pair(Var "x", Var "x")) in
  let ty, subst = damasMilnerTofte polyterm2 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n\n"
                (term_to_string polyterm2)
                (typescheme_to_string ty);
