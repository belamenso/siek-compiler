import compiler._

class Pass2RemoveComplexOperands extends munit.FunSuite {

  def testPass(programPre: String, programPost: String): Unit =
    assertEquals(removeComplexOperands(uniquify(readProgram(programPre))), readProgram(programPost))
    assertEquals(
      Interpreter.interpretProgram(removeComplexOperands(uniquify(readProgram(programPre)))),
      Interpreter.interpretProgram(readProgram(programPost))
    )

  test("remove complex operands 1") {
    testPass(
      "(let [x (+ 42 (- 10))] (+ x 10))",
      "(let [x.1 (let [.tmp.1 (- 10)] (+ 42 .tmp.1))] (+ x.1 10))"
    )
  }

  test("remove complex operands 2") {
    testPass(
      "(+ 1 2)",
      "(+ 1 2)"
    )
  }

  test("remove complex operands 3") {
    testPass(
      "(let [a 1] (let [b 2] (+ a b)))",
      "(let [a.1 1] (let [b.2 2] (+ a.1 b.2)))"
    )
  }

  test("remove complex operands 4") {
    testPass(
      """(let [a (+ 1 (+ 2 3))]
           (let [b (+ (- a a) a)]
             (+ (+ a b) (+ b a))))""",
      """(let [a.1 (let
                    [.tmp.1 (+ 2 3)]
                     (+ 1 .tmp.1))]
                    (let
                      [b.2
                        (let [.tmp.2 (- a.1 a.1)]
                          (+ .tmp.2 a.1))]
            (let [.tmp.3 (+ a.1 b.2)]
              (let [.tmp.4 (+ b.2 a.1)]
                (+ .tmp.3 .tmp.4)))))"""
    )
  }

  test("remove complex oeprands 5") {
    testPass(
      """(let [y (let [x 20] (+ x (let [x 22] x)))] y)""",
      """(let [y.1
                (let [x.2 20]
                  (let [x.3 22]
                    (+ x.2 x.3)))]
            y.1)"""
    )
  }
}
