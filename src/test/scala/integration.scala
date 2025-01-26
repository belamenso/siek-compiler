import compiler._

class Integration extends munit.FunSuite {
  test("main") {
    val r = compileAndRunProgram("""(let [a 10] (let [b 23] (+ a (- b 19))))""")
    assertEquals(r, 14L)
  }
}
