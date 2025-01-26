import compiler._

class ExplicateControl extends munit.FunSuite {
  test("explicate control 1") {
    assertEquals(
      explicateControl(
        removeComplexOperands(uniquify(readProgram("""(let [y (let [x 20] (+ x (let [x 22] x)))] y)""")))
      ).butWithNoInfo,
      CProgram(
        info = Map(),
        body = Map(
          "start" -> SeqCTail(
            stmt = AssignCStmt(name = "x.2", expr = CInt(value = 20L)),
            tail = SeqCTail(
              stmt = AssignCStmt(name = "x.3", expr = CInt(value = 22L)),
              tail = SeqCTail(
                stmt = AssignCStmt(
                  name = "y.1",
                  expr = PrimCExpr(op = "+", args = List(CVar(name = "x.2"), CVar(name = "x.3")))
                ),
                tail = ReturnCTail(expr = CVar(name = "y.1"))
              )
            )
          )
        )
      )
    )
  }

  test("explicate control with nested arithmetic") {
    assertEquals(
      explicateControl(removeComplexOperands(uniquify(readProgram(
        """(let [x 10]
            (let [y (+ x (+ x 5))]
              y))"""
      )))).butWithNoInfo,
      CProgram(
        info = Map(),
        body = Map(
          "start" -> SeqCTail(
            stmt = AssignCStmt(name = "x.1", expr = CInt(value = 10L)),
            tail = SeqCTail(
              stmt = AssignCStmt(
                name = ".tmp.1",
                expr = PrimCExpr(op = "+", args = List(CVar(name = "x.1"), CInt(value = 5L)))
              ),
              tail = SeqCTail(
                stmt = AssignCStmt(
                  name = "y.2",
                  expr = PrimCExpr(op = "+", args = List(CVar(name = "x.1"), CVar(name = ".tmp.1")))
                ),
                tail = ReturnCTail(expr = CVar(name = "y.2"))
              )
            )
          )
        )
      )
    )
  }

  test("explicate control with multiple variables same scope") {
    assertEquals(
      explicateControl(removeComplexOperands(uniquify(readProgram(
        """(let [x 1]
            (let [y 2]
              (let [z (+ x y)]
                z)))"""
      )))).butWithNoInfo,
      CProgram(
        info = Map(),
        body = Map(
          "start" -> SeqCTail(
            stmt = AssignCStmt(name = "x.1", expr = CInt(value = 1L)),
            tail = SeqCTail(
              stmt = AssignCStmt(name = "y.2", expr = CInt(value = 2L)),
              tail = SeqCTail(
                stmt = AssignCStmt(
                  name = "z.3",
                  expr = PrimCExpr(op = "+", args = List(CVar(name = "x.1"), CVar(name = "y.2")))
                ),
                tail = ReturnCTail(expr = CVar(name = "z.3"))
              )
            )
          )
        )
      )
    )
  }
}
