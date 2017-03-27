open OUnit2
open TypingEnvironment
open MonoAst
open PolyAst

let generalize_quantify_all_monotype_variables test_ctxt =
  let context = TypingEnvironment.empty
  and monotype = TVar "α"
  and expected = TForAll(["α"],TVar "α") in
  let result = TypingEnvironment.generalize context monotype in
  Printf.printf "\n%s" (PrettyPrinter.polytype_to_string result);
  assert_equal expected result

(* Some examples from TD4Exo3 typage-m2 *)
let generalize_when_all_monotype_variables_are_bound_in_the_context_1 test_ctxt =
  let context =
    TypingEnvironment.
    (bind
       (bind empty "x" (TForAll([],TVar "α")))
       "y" (TForAll(["α"],(TArrow(TVar "α",TVar "β"))))
    )
  and monotype = TProduct(TVar "α",TArrow(TVar "α",TVar "β"))
  and expected = TForAll([],TProduct(TVar "α",TArrow(TVar "α",TVar "β"))) in
  let result = TypingEnvironment.generalize context monotype in
  Printf.printf "\n%s" (PrettyPrinter.polytype_to_string result);
  assert_equal expected result

let generalize_when_all_monotype_variables_are_bound_in_the_context_2 test_ctxt =
  let context =
    TypingEnvironment.
    (bind
       (bind empty "x" (TForAll([],TVar "α")))
       "y" (TForAll(["α"],TVar "α"))
    )
  and monotype = TProduct(TVar "α",TArrow(TVar "α",TVar "β"))
  and expected = TForAll(["β"],TProduct(TVar "α",TArrow(TVar "α",TVar "β"))) in
  let result = TypingEnvironment.generalize context monotype in
  Printf.printf "\n%s" (PrettyPrinter.polytype_to_string result);
  assert_equal expected result
    
let suite =
  "typingEnvionmentTests">:::
    [
      "generalize in an empty environment should
       quantify all variables of the monotype"
      >::generalize_quantify_all_monotype_variables;

      "[1] generalize should quantify no monotype variable
       when all of them are bound in the context"
      >::generalize_when_all_monotype_variables_are_bound_in_the_context_1;

      "[2] generalize should quantify no monotype variable
       when all of them are bound in the context"
      >::generalize_when_all_monotype_variables_are_bound_in_the_context_2;

    ]

let run () =
  run_test_tt_main suite
