import compiler._

class SelectInstructions extends munit.FunSuite {
  test("select instructions 1") {
    assertEquals(
      selectInstructions(explicateControl(removeComplexOperands(uniquify(readProgram("""(let [y (read)] y)"""))))).butWithNoInfo,
      Ax86Program(
        info = Map(),
        blocks = Map(
          "start" -> ABlock(
            info = List(),
            body = List(
              ACallq(label = "read_int", v = 0L),
              AMovq(src = RegAsmArg(reg = AReg.RAX), dst = VarAsmArg(name = "y.1")),
              AMovq(src = VarAsmArg(name = "y.1"), dst = RegAsmArg(reg = AReg.RAX)),
              AJmp(label = "conclusion")
            )
          )
        )
      )
    )
  }

  test("select instructions 2") {
    assertEquals(
      selectInstructions(
        explicateControl(removeComplexOperands(uniquify(readProgram("""(let [x (+ 42 (- 10))] (+ x 10))"""))))
      ).butWithNoInfo,
      Ax86Program(
        info = Map(),
        blocks = Map(
          "start" -> ABlock(
            info = List(),
            body = List(
              AMovq(src = ImmAsmArg(value = 10L), dst = VarAsmArg(name = ".tmp.1")),
              ANegq(dst = VarAsmArg(name = ".tmp.1")),
              AMovq(src = ImmAsmArg(value = 42L), dst = VarAsmArg(name = "x.1")),
              AAddq(src = VarAsmArg(name = ".tmp.1"), dst = VarAsmArg(name = "x.1")),
              AMovq(src = VarAsmArg(name = "x.1"), dst = RegAsmArg(reg = AReg.RAX)),
              AAddq(src = ImmAsmArg(value = 10L), dst = RegAsmArg(reg = AReg.RAX)),
              AJmp(label = "conclusion")
            )
          )
        )
      )
    )
  }

  test("select instructions 3") {
    assertEquals(
      selectInstructions(explicateControl(
        removeComplexOperands(uniquify(readProgram("""(let [y (let [x 20] (+ x (let [x 22] x)))] y)""")))
      )).butWithNoInfo,
      Ax86Program(
        info = Map(),
        blocks = Map(
          "start" -> ABlock(
            info = List(),
            body = List(
              AMovq(src = ImmAsmArg(value = 20L), dst = VarAsmArg(name = "x.2")),
              AMovq(src = ImmAsmArg(value = 22L), dst = VarAsmArg(name = "x.3")),
              AMovq(src = VarAsmArg(name = "x.2"), dst = VarAsmArg(name = "y.1")),
              AAddq(src = VarAsmArg(name = "x.3"), dst = VarAsmArg(name = "y.1")),
              AMovq(src = VarAsmArg(name = "y.1"), dst = RegAsmArg(reg = AReg.RAX)),
              AJmp(label = "conclusion")
            )
          )
        )
      )
    )
  }
}
