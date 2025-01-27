import compiler._

class PatchInstructions extends munit.FunSuite {
  test("patch instructions 1") {
    val p = """(let [a 1] (let [b a] b))"""
    val x = (uniquify(readProgram(p)))
    val x1 = removeComplexOperands(x)
    val y = explicateControl(x1)
    val y1 = selectInstructions(y)
    val z = patchInstructions(assignHomes(y1))

    val expected = Ax86Program(
      info = Map("local-types" -> Map("a.1" -> "long", "b.2" -> "long"), "stack-space" -> 16),
      blocks = Map(
        "start" -> ABlock(
          info = Map(),
          body = List(
            AMovq(src = ImmAsmArg(value = 1L), dst = DerefAsmArg(reg = AReg.RBP, offset = -8L)),
            AMovq(src = DerefAsmArg(reg = AReg.RBP, offset = -8L), dst = RegAsmArg(reg = AReg.RAX)),
            AMovq(src = RegAsmArg(reg = AReg.RAX), dst = DerefAsmArg(reg = AReg.RBP, offset = -16L)),
            AMovq(src = DerefAsmArg(reg = AReg.RBP, offset = -16L), dst = RegAsmArg(reg = AReg.RAX)),
            AJmp(label = "conclusion")
          )
        )
      )
    )

    assertEquals(z, expected)
  }

  // TODO add tests for eliminating trivial moves
}
