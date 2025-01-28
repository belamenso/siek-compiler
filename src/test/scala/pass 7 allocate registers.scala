import compiler._
import compiler.given
import AReg._

class AllocateRegisters extends munit.FunSuite {
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

    val gotBlock = allocateRegisters(buildInterference(uncoverLive(prog))).blocks("start").butWithNoInfo
    assertEquals(gotBlock.body, List(
      AMovq(src = ImmAsmArg(value = 1L), dst = RegAsmArg(reg = RCX)),
      AMovq(src = ImmAsmArg(value = 42L), dst = DerefAsmArg(reg = RBP, offset = -8L)),
      AMovq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RCX)),
      AAddq(src = ImmAsmArg(value = 7L), dst = RegAsmArg(reg = RCX)),
      AMovq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RCX)),
      AMovq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RDX)),
      AAddq(src = DerefAsmArg(reg = RBP, offset = -8L), dst = RegAsmArg(reg = RDX)),
      AMovq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RCX)),
      ANegq(dst = RegAsmArg(reg = RCX)),
      AMovq(src = RegAsmArg(reg = RDX), dst = RegAsmArg(reg = RAX)),
      AAddq(src = RegAsmArg(reg = RCX), dst = RegAsmArg(reg = RAX)),
      AJmp(label = "conclusion")
    ))
  }
}
