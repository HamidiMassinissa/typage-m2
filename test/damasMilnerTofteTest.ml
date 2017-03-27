open OUnit2
open Ast
open MonoAst
open PolyAst
open PolyTypeInference
open PrettyPrinter
open Substitution
open TypingEnvironment

let variable_with_no_context_should_fail test_ctxt =
  let term = Var "x" in
  assert_raises
    (Failure "Unbound variable x")
    (fun _ -> damasMilnerTofte term)

let variable_should_have_var_type_and_id_substitution_when_in_context test_ctxt =
  let term = Var "x"
  and context = TypingEnvironment.(bind empty "x" (TForAll (["α1"],TVar "α1"))) in
  let ty, subst = damasMilnerTofte ~context term in
  let expected_ty = TVar "ζ1"
  and expected_substitution = Substitution.identity in
  assert_equal expected_ty ty;
  assert_equal expected_substitution subst

let identity_should_have_arrow_type_and_id_substitution test_ctxt =
  let term = Lambda("x",Var "x") in
  let ty, subst = damasMilnerTofte term in
  let expected_ty = TArrow(TVar "α1", TVar "α1")
  and expected_substitution = Substitution.identity in
  assert_equal expected_ty ty;
  assert_equal expected_substitution subst;
  Printf.printf "\n%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string term)
                (typescheme_to_string ty)

let identity_app_on_variable_with_no_context_should_fail test_ctxt =
  let term = App(Lambda("x",Var "x"), Var "x") in
  assert_raises
    (Failure "Unbound variable x")
    (fun _ -> damasMilnerTofte term)

let identity_app_on_variable_when_in_context test_ctxt =
  let term = App(Lambda("x",Var "x"), Var "x")
  and context = TypingEnvironment.(bind empty "x" (TForAll (["α1"],TVar "α1"))) in
  let ty, subst = damasMilnerTofte ~context term in
  Printf.printf "\n%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string term)
                (typescheme_to_string ty)

let identity_app_on_integer_constant test_ctxt =
  let term = App(Lambda("x",Var "x"), Const "1")
  and context = TypingEnvironment.(bind empty "x" (TForAll (["α1"],TVar "α1"))) in
  let ty, subst = damasMilnerTofte ~context term in
  Printf.printf "\n%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string term)
                (typescheme_to_string ty)

let local_identity_applied_to_variable test_ctxt =
  let term = Let("f",Lambda("x",Var "x"), App(Var "f",Var "x"))
  and context = TypingEnvironment.(bind empty "x" (TForAll (["α1"],TVar "α1"))) in
  let ty, subst = damasMilnerTofte ~context term in
  Printf.printf "\n%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string term)
                (typescheme_to_string ty)

let local_identity_applied_to_integer_constant test_ctxt =
  let term = Let("f",Lambda("x",Var "x"), App(Var "f",Const "1")) in
  let ty, subst = damasMilnerTofte term in
  Printf.printf "\n%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string term)
                (typescheme_to_string ty)

let trying_to_type_omega_combinator_should_fail test_ctxt =
  let omega_combinator = Lambda("x", App(Var "x",Var "x")) in
  assert_raises
    (Failure "Unification failed: type variable \206\1774 appears in type scheme (\206\1774 \226\134\146 \206\1775)\n")
    (fun _ -> damasMilnerTofte omega_combinator)

let suite =
  "DamasMilnerTofteTests">:::
    [
      "simple variable term with no context information should fail"
      >::variable_with_no_context_should_fail;

      "variable should have var type WHEN IT IS IN THE CONTEXT"
      >::variable_should_have_var_type_and_id_substitution_when_in_context;

      "identity should have arrow type and the identity substitution"
      >::identity_should_have_arrow_type_and_id_substitution;

      "identity application on variable which is not in context"
      >::identity_app_on_variable_with_no_context_should_fail;

      "identity application on variable which is in context"
      >::identity_app_on_variable_when_in_context;

      "identity application on integer constant"
      >::identity_app_on_integer_constant;

      "local identity applied to variable"
      >::local_identity_applied_to_variable;

      "local identity applied to integer constant"
      >::local_identity_applied_to_integer_constant;

      "Test 1 should fail: recursive construction needed
       in this case to type Ω combinator"
      >::trying_to_type_omega_combinator_should_fail;

      (*"Damas Milner Tofte"
      >::damasMilnerTofteTest1;

      "Damas Milner Tofte Generalization"
      >::damasMilnerTofteTest2;*)
    ]

let run () =
  run_test_tt_main suite

















(*
let damasMilnerTofteTest1 test_ctxt =
  let polyterm4 = Lambda("x", Lambda("y", App(Var "x", Var "y"))) in
  let ty, subst = damasMilnerTofte polyterm4 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string polyterm4)
                (typescheme_to_string ty)

let damasMilnerTofteTest2 test_ctxt =
  let polyterm3 = Let("f", Lambda("x", Var "x"), App(Var "f", Const "1")) in
  let ty, subst = damasMilnerTofte polyterm3 in
  Printf.printf "%s" (Substitution.to_string subst);
  Printf.printf "%s\n%s\n"
                (term_to_string polyterm3)
                (typescheme_to_string ty)
                *)

  (*
  let term2 = Lambda("x", App(Var "x", Var "x")) in
  let eqs2 = build_equational_system term2 in
  Printf.printf "%s\n%s\n"
                (term_to_string term2)
                (tconstraint_to_string eqs2);

  (* polymorphic types test *)
  (* Test 1 should fail: recursive construction needed in this case
     to type Ω combinator
  Printf.printf "\nTesting Polymorphic Types\n";
  let polyterm0 = Lambda("x", App(Var "x",Var "x")) in
  let ty, subst = damasMilnerTofte polyterm0 in
  Printf.printf "%s\n%s\n"
                (term_to_string polyterm0)
                (typescheme_to_string ty);
   *)

  let polyterm1 = Lambda("x", App(Const "+", Pair (Var "x", Var "x"))) in
  let ty, subst = damasMilnerTofte polyterm1 in
  Printf.printf "%s\n%s\n"
                (term_to_string polyterm1)
                (typescheme_to_string ty);

  let polyterm2 = Lambda("x", Pair(Var "x", Var "x")) in
  let ty, subst = damasMilnerTofte polyterm2 in
  Printf.printf "%s\n%s\n"
                (term_to_string polyterm2)
                (typescheme_to_string ty);
   *)
