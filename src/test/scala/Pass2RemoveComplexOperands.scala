import compiler._

class Pass2RemoveComplexOperands extends munit.FunSuite {

  def testPass(programPre: String, programPost: String): Unit =
    assertEquals(removeComplexOperands(readProgram(programPre)), readProgram(programPost))
    assertEquals(Interpreter.interpretProgram(removeComplexOperands(readProgram(programPre))),
                 Interpreter.interpretProgram(readProgram(programPost)))

  test("remove complex operands 1") {
    testPass(
      "(let [x (+ 42 (- 10))] (+ x 10))",
      "(let [x (let [.tmp.1 (- 10)] (+ 42 .tmp.1))] (+ x 10))")
  }

  test("remove complex operands 2") {
    testPass(
      "(+ 1 2)",
      "(+ 1 2)")
  }

  test("remove complex operands 3") {
    testPass(
      "(let [a 1] (let [b 2] (+ a b)))",
      "(let [a 1] (let [b 2] (+ a b)))")
  }

  test("remove complex operands 4") {
    testPass(
      """(let [a (+ 1 (+ 2 3))]
           (let [b (+ (- a a) a)]
             (+ (+ a b) (+ b a))))""",
      """(let [a (let
                    [.tmp.1 (+ 2 3)]
                     (+ 1 .tmp.1))]
                    (let
                      [b
                        (let [.tmp.2 (- a a)]
                          (+ .tmp.2 a))]
            (let [.tmp.3 (+ a b)]
              (let [.tmp.4 (+ b a)]
                (+ .tmp.3 .tmp.4)))))"""
    )
  }
}
