open OUnit2
open MonoAst
open PolyAst
open PolyTypeInference

(* PolyTypeInference Instantiate *)
let ptys =
  [
    TForAll (["α";"β"], TArrow(TVar "α",TVar "β")),
    (TArrow (TVar "ζ1",TVar "ζ2"));

    TForAll (["α";"β"], TArrow(TVar "α",TVar "β")),
    (TArrow (TVar "ζ3",TVar "ζ4"))

 ]

let test_successive_instantiates test_ctxt =
  ignore (
      List.map
        (fun (input, expected) ->
          assert_equal
            expected
            (PolyTypeInference.instantiate input)
        ) ptys)

let instantiate_replaces_only_bound_variable test_ctxt =
  let pty = TForAll (["α"], TArrow(TVar "α",TVar "β"))
  and expected = TArrow (TVar "ζ1",TVar "β") in
  assert_equal
    expected
    (PolyTypeInference.instantiate pty)

let instantiate_replaces_nothing test_ctxt =
  let pty = TForAll ([], TVar "α")
  and expected = TVar "α" in
  assert_equal
    expected
    (PolyTypeInference.instantiate pty)

let instantiate_rename_same_variables_in_different_branches_to_a_unique_fresh_variable test_ctxt =
  let pty = TForAll(["α"],TArrow(TVar "α",TVar "α")) in
  let result = PolyTypeInference.instantiate pty in
  Printf.printf "\n%s" (PrettyPrinter.typescheme_to_string result);
  match result with
  | TArrow(TVar itv,TVar otv) ->
     assert_equal itv otv
  | TArrow _ | TVar _ | TBase _ | TProduct _ -> assert false

let suite =
  "polyTypeInferenceTests">:::
    [
      "Successive instantiates"
      >::test_successive_instantiates;

      "Instantiate replaces only bound variable"
      >::instantiate_replaces_only_bound_variable;

      "Instantiate replaces nothing"
      >::instantiate_replaces_nothing;

      "Instantiate keep information equality
       when visiting branches of polytype"
      >::instantiate_rename_same_variables_in_different_branches_to_a_unique_fresh_variable
    ]

let run () =
  run_test_tt_main suite
