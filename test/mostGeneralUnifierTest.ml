open OUnit2
open MonoAst
open Unification
open Substitution

(* PolyTypeInference most_general_unifier *)
let mgu_test_success test_ctxt =
  let aty = TArrow(TBase "a",
                   TProduct(TVar "z",TVar "y"))

  and bty = TArrow(TVar "x",
                   TProduct(
                       TArrow(TBase "a",TVar "x"),
                       TProduct(TVar "z",TVar "x")))
  in
  let eq_system = TyEq (aty,bty) in
  let resolved_eq_system =
    [ (* This order was deduced from printing the resulting substitution *)
      "z", TArrow(TBase "a",TBase "a");
      "y", TProduct(TArrow(TBase "a",TBase "a"),TBase "a");
      "x", TBase "a";
    ]
  in
  let expected =
    List.fold_right
      (fun (tyv,tysch) subst ->
        Substitution.bind subst tyv tysch)
      resolved_eq_system
      Substitution.identity
  in
  Printf.printf "%s" (Substitution.to_string (PolyTypeInference.most_general_unifier eq_system));
  assert_equal
    expected
    (PolyTypeInference.most_general_unifier eq_system)

let mgu_should_not_loop_forever test_ctx =
  let aty = TArrow(TVar "x",TVar "y")
  and bty = TArrow(TProduct(TVar "y",TBase "a"),TBase "a") in
  let eq_system = TyEq (aty,bty) in
  Printf.printf "%s" (Substitution.to_string (PolyTypeInference.most_general_unifier eq_system))

let mgu_should_fail_with_occur_check test_ctx = ()

let suite =
  "unificationTests">:::
    [
      "mgu test which should succeed">::mgu_test_success;
      "mgu test that should not loop forever">::mgu_should_not_loop_forever;
    ]

let run () =
  run_test_tt_main suite
