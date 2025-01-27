import compiler._
import AReg._

class UncoverLive extends munit.FunSuite {
  test("figure 3.4") {
    val prog = Ax86Program(
      Map(),
      Map("start" -> ABlock(
        Map(),
        Seq(
          AMovq(ImmAsmArg(5), VarAsmArg("a")),
          AMovq(ImmAsmArg(30), VarAsmArg("b")),
          AMovq(VarAsmArg("a"), VarAsmArg("c")),
          AMovq(ImmAsmArg(10), VarAsmArg("b")),
          AAddq(VarAsmArg("b"), VarAsmArg("c"))
        )
      ))
    )

    val expected = List(
      Set(),
      Set(VarAsmArg("a")),
      Set(VarAsmArg("a")),
      Set(VarAsmArg("c")),
      Set(VarAsmArg("b"), VarAsmArg("c")),
      Set()
    )

    assertEquals(uncoverLive(prog).blocks("start").info("live-after").asInstanceOf[List[Any]], expected)
  }

  test("figure 3.5") {
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

    val expected = List(
      Set(RegAsmArg(RSP)),
      Set(RegAsmArg(RSP), VarAsmArg("v")),
      Set(RegAsmArg(RSP), VarAsmArg("w"), VarAsmArg("v")),
      Set(RegAsmArg(RSP), VarAsmArg("w"), VarAsmArg("x")),
      Set(RegAsmArg(RSP), VarAsmArg("w"), VarAsmArg("x")),
      Set(RegAsmArg(RSP), VarAsmArg("y"), VarAsmArg("w"), VarAsmArg("x")),
      Set(RegAsmArg(RSP), VarAsmArg("y"), VarAsmArg("w"), VarAsmArg("z")),
      Set(RegAsmArg(RSP), VarAsmArg("z"), VarAsmArg("y")),
      Set(RegAsmArg(RSP), VarAsmArg("t"), VarAsmArg("z")),
      Set(RegAsmArg(RSP), VarAsmArg("t"), VarAsmArg("z")),
      Set(RegAsmArg(RSP), VarAsmArg("t"), RegAsmArg(RAX)),
      Set(RegAsmArg(RAX), RegAsmArg(RSP)),
      Set()
    )

    assertEquals(uncoverLive(prog).blocks("start").info("live-after").asInstanceOf[List[Any]], expected)
  }
}
