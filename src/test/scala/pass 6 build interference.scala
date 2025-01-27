import compiler._
import AReg._

class BuildInterference extends munit.FunSuite {
  test("3.8") {
    val prog = Ax86Program(Map(), Map("start" -> ABlock(Map(), Seq(
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
    ))))

    val expected = Map(
      RegAsmArg(reg = RAX) -> Set(RegAsmArg(reg = RSP), VarAsmArg(name = "t")),
      VarAsmArg(name = "t") -> Set(RegAsmArg(reg = RSP), VarAsmArg(name = "z"), RegAsmArg(reg = RAX)),
      RegAsmArg(reg = RSP) -> Set(
        RegAsmArg(reg = RAX),
        VarAsmArg(name = "t"),
        VarAsmArg(name = "x"),
        VarAsmArg(name = "y"),
        VarAsmArg(name = "v"),
        VarAsmArg(name = "w"),
        VarAsmArg(name = "z")
      ),
      VarAsmArg(name = "x") -> Set(RegAsmArg(reg = RSP), VarAsmArg(name = "w")),
      VarAsmArg(name = "y") -> Set(RegAsmArg(reg = RSP), VarAsmArg(name = "w"), VarAsmArg(name = "z")),
      VarAsmArg(name = "v") -> Set(RegAsmArg(reg = RSP), VarAsmArg(name = "w")),
      VarAsmArg(name = "w") -> Set(
        RegAsmArg(reg = RSP),
        VarAsmArg(name = "x"),
        VarAsmArg(name = "y"),
        VarAsmArg(name = "v"),
        VarAsmArg(name = "z")
      ),
      VarAsmArg(name = "z") -> Set(
        RegAsmArg(reg = RSP),
        VarAsmArg(name = "y"),
        VarAsmArg(name = "w"),
        VarAsmArg(name = "t")
      )
    )

    assertEquals(buildInterference(uncoverLive(prog)).info("conflicts").asInstanceOf[UndirectedGraph[AsmArg]].edges, expected)
  }
}
