package compiler

def selectInstructions(program: CProgram): Ax86Program = {
  def selectInstructionsAtom(atom: CAtom): AsmArg = atom match {
    case CInt(value) => ImmAsmArg(value)
    case CVar(name)  => VarAsmArg(name)
  }

  def selectInstructionsStmt(s: CStmt, shouldAssignToRAXInstead: Boolean = false): Seq[AsmInstr] =
    val dest =
      if shouldAssignToRAXInstead
      then RegAsmArg(AReg.RAX)
      else VarAsmArg(s match { case AssignCStmt(name, _) => name })
    s match {
      case AssignCStmt(x, CInt(n))    => Seq(AMovq(ImmAsmArg(n), dest))
      case AssignCStmt(x, CVar(name)) => Seq(AMovq(VarAsmArg(name), dest))
      case AssignCStmt(x, PrimCExpr("-", Seq(arg))) =>
        val src = selectInstructionsAtom(arg)
        Seq(AMovq(src, dest), ANegq(dest))
      case AssignCStmt(x, PrimCExpr("+", Seq(a, b))) =>
        val src1 = selectInstructionsAtom(a)
        val src2 = selectInstructionsAtom(b)
        (src1, src2) match {
          case (CVar(x1), _) if x == x1 && !shouldAssignToRAXInstead => Seq(AAddq(src2, dest))
          case (_, CVar(x1)) if x == x1 && !shouldAssignToRAXInstead => Seq(AAddq(src1, dest))
          case _                                                     => Seq(AMovq(src1, dest), AAddq(src2, dest))
        }
      case AssignCStmt(x, PrimCExpr("-", Seq(a, b))) => // do not modify src1, src2's locations!
        val src1 = selectInstructionsAtom(a)
        val src2 = selectInstructionsAtom(b)
        Seq(AMovq(src1, dest), ASubq(src2, dest))
      case AssignCStmt(x, PrimCExpr("read", Seq())) =>
        Seq(ACallq("read_int", 0), AMovq(RegAsmArg(AReg.RAX), dest))
    }

  def selectInstructionsTail(t: CTail): Seq[AsmInstr] = t match {
    // XXX what guarantees that "conclusion" will not be overriden?
    case ReturnCTail(expr)    => selectInstructionsStmt(AssignCStmt("", expr), true) ++ Seq(AJmp("conclusion"))
    case SeqCTail(stmt, tail) => selectInstructionsStmt(stmt) ++ selectInstructionsTail(tail)
  }

  Ax86Program(
    program.info,
    program.body.map { case (name, block) => (name, ABlock(List(), selectInstructionsTail(block))) }
  )
}
