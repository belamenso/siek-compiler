import compiler._

class AssignHomes extends munit.FunSuite {
  test("assign homes 1") {
    val p = """(let [a 42] (let [b a] b))"""
    val x = (uniquify(readProgram(p)))
    val x1 = removeComplexOperands(x)
    val y = explicateControl(x1)
    val y1 = selectInstructions(y)
    val z = assignHomes(y1)

    val expected = Ax86Program(
      info = Map(
        "local-types" -> Map("a.1" -> "long", "b.2" -> "long"),
        "used-callee" -> Set[AReg](),
        "spilled-fields" -> 2
      ),
      blocks = Map(
        "start" -> ABlock(
          info = Map(),
          body = List(
            AMovq(src = ImmAsmArg(value = 42L), dst = DerefAsmArg(reg = AReg.RBP, offset = -8L)),
            AMovq(
              src = DerefAsmArg(reg = AReg.RBP, offset = -8L),
              dst = DerefAsmArg(reg = AReg.RBP, offset = -16L)
            ),
            AMovq(src = DerefAsmArg(reg = AReg.RBP, offset = -16L), dst = RegAsmArg(reg = AReg.RAX)),
            AJmp(label = "conclusion")
          )
        )
      )
    )

    assertEquals(z, expected)
  }
}
