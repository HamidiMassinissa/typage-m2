let run_tests =
  TypingEnvironmentTest.run ();
  PolyTypeInferenceTest.run ();
  MostGeneralUnifierTest.run ();
  DamasMilnerTofteTest.run ();
