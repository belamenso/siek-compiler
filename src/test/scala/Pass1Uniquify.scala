import compiler._

class PassUniquify extends munit.FunSuite {

  def testPass(programPre: String, programPost: String): Unit =
    assertEquals(uniquify(readProgram(programPre)), readProgram(programPost))
    assertEquals(Interpreter.interpretProgram(uniquify(readProgram(programPre))),
                 Interpreter.interpretProgram(readProgram(programPost)))

  test("uniquify 1") {
    testPass("(let [x (+ 1 1)] (+ (let [x 12] x) x))",
             "(let [x.1 (+ 1 1)] (+ (let [x.2 12] x.2) x.1))")
  }

  test("uniquify 2") {
    testPass(
      "(let [x (+ 1 (let [x 1] x))] (+ (let [y 12] y) x))",
      "(let [x.1 (+ 1 (let [x.2 1] x.2))] (+ (let [y.3 12] y.3) x.1))")
  }

  test("uniquify 3") {
    testPass(
      "(let [x (+ 1 (let [x 1] x))] (+ (let [y 12] y) x))",
      "(let [x.1 (+ 1 (let [x.2 1] x.2))] (+ (let [y.3 12] y.3) x.1))")
  }
}
