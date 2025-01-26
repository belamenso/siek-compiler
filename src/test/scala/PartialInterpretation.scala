class PartialInterpretation extends munit.FunSuite {

  test("partial interpretation") {
    for (p <- Seq("(+ -8 10)", "(+ 10 (- (+ 5 3)))", "(+ 1 (+ 3 1))", "(- (+3 (- 5)))").map(readProgram))
    assertEquals(
      interpLint(p),
      interpLint(peLint(p))
    )
  }
}
