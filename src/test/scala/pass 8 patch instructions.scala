import compiler._
import AReg._

class PatchInstructions extends munit.FunSuite {
  test("patch instructions 1") {
    val p = """(let [a 1] (let [b a] b))"""
    val x = (uniquify(readProgram(p)))
    val x1 = removeComplexOperands(x)
    val y = explicateControl(x1)
    val y1 = selectInstructions(y)
    val z = patchInstructions(assignHomes(y1))

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

  test("example from page 49") {
    given KAllocationBound = KAllocationBound(2)

    val prog = Ax86Program(
      Map(),
      Map("start" -> ABlock(
        Map(),
        Seq(
          AMovq(ImmAsmArg(1), VarAsmArg("v")),
          AMovq(ImmAsmArg(42), VarAsmArg("w")),
          AMovq(VarAsmArg("v"), VarAsmArg("x")),
          AAddq(ImmAsmArg(7), VarAsmArg("x")),
          AMovq(VarAsmArg("x"), VarAsmArg("y")),
          AMovq(VarAsmArg("x"), VarAsmArg("z")),
          AAddq(VarAsmArg("w"), VarAsmArg("z")),
          AMovq(VarAsmArg("y"), VarAsmArg("t")),
          ANegq(VarAsmArg("t")),
          AMovq(VarAsmArg("z"), RegAsmArg(AReg.RAX)),
          AAddq(VarAsmArg("t"), RegAsmArg(AReg.RAX)),
          AJmp("conclusion")
        )
      ))
    )

    val gotBlock =
      patchInstructions(allocateRegisters(buildInterference(uncoverLive(prog)))).blocks("start").butWithNoInfo
    assertEquals(
      gotBlock.body,
      List(
        AMovq(src = ImmAsmArg(value = 1L), dst = RegAsmArg(reg = RCX)),
        AMovq(src = ImmAsmArg(value = 42L), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AAddq(src = ImmAsmArg(value = 7L), dst = RegAsmArg(reg = RCX)),
        AMovq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RDX)),
        AAddq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RDX)),
        ANegq(dst = RegAsmArg(reg = RCX)),
        AMovq(src = RegAsmArg(reg = RDX), dst = RegAsmArg(reg = RAX)),
        AAddq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RAX)),
        AJmp(label = "conclusion")
      )
    )
  }

  test("example from page 49 less registers") {
    given KAllocationBound = KAllocationBound(0)

    val prog = Ax86Program(
      Map(),
      Map("start" -> ABlock(
        Map(),
        Seq(
          AMovq(ImmAsmArg(1), VarAsmArg("v")),
          AMovq(ImmAsmArg(42), VarAsmArg("w")),
          AMovq(VarAsmArg("v"), VarAsmArg("x")),
          AAddq(ImmAsmArg(7), VarAsmArg("x")),
          AMovq(VarAsmArg("x"), VarAsmArg("y")),
          AMovq(VarAsmArg("x"), VarAsmArg("z")),
          AAddq(VarAsmArg("w"), VarAsmArg("z")),
          AMovq(VarAsmArg("y"), VarAsmArg("t")),
          ANegq(VarAsmArg("t")),
          AMovq(VarAsmArg("z"), RegAsmArg(AReg.RAX)),
          AAddq(VarAsmArg("t"), RegAsmArg(AReg.RAX)),
          AJmp("conclusion")
        )
      ))
    )

    val gotBlock =
      patchInstructions(allocateRegisters(buildInterference(uncoverLive(prog)))).blocks("start").butWithNoInfo
    assertEquals(
      gotBlock.body,
      List(
        AMovq(src = ImmAsmArg(value = 1L), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AMovq(src = ImmAsmArg(value = 42L), dst = DerefAsmArg(reg = RBP, offset = -24L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RAX)),
        AMovq(src = RegAsmArg(reg = RAX), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AAddq(src = ImmAsmArg(value = 7L), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RAX)),
        AMovq(src = RegAsmArg(reg = RAX), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RAX)),
        AMovq(src = RegAsmArg(reg = RAX), dst = DerefAsmArg(reg = RBP, offset = -16L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -24L), dst = RegAsmArg(reg = RAX)),
        AAddq(src = RegAsmArg(reg = RAX), dst = DerefAsmArg(reg = RBP, offset = -16L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RAX)),
        AMovq(src = RegAsmArg(reg = RAX), dst = DerefAsmArg(reg = RBP, offset = -8L)),
        ANegq(dst = DerefAsmArg(reg = RBP, offset = -8L)),
        AMovq(src = DerefAsmArg(reg = RBP, offset = -16L), dst = RegAsmArg(reg = RAX)),
        AAddq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RAX)),
        AJmp(label = "conclusion")
      )
    )
  }
}
