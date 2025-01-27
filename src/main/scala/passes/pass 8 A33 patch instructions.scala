package compiler

def patchInstructions(program: Ax86Program): Ax86Program = {
  def assignHomesInInstr(i: AsmInstr): Seq[AsmInstr] = i match {
    case AAddq(z1 @ DerefAsmArg(_, _), z2 @ DerefAsmArg(_, _)) =>
      Seq(AMovq(z1, RegAsmArg(AReg.RAX)), AAddq(RegAsmArg(AReg.RAX), z2))
    case ASubq(z1 @ DerefAsmArg(_, _), z2 @ DerefAsmArg(_, _)) =>
      Seq(AMovq(z1, RegAsmArg(AReg.RAX)), ASubq(RegAsmArg(AReg.RAX), z2))
    case AMovq(z1 @ DerefAsmArg(_, _), z2 @ DerefAsmArg(_, _)) =>
      Seq(AMovq(z1, RegAsmArg(AReg.RAX)), AMovq(RegAsmArg(AReg.RAX), z2))
    case _ => Seq(i)
  }

  extension (i: AsmInstr) def isMoveTrivial: Boolean = i match {
    case AMovq(a, b) if a == b => true
    case _ => false
  }

  Ax86Program(
    program.info,
    program.blocks.map((n, block) => (n -> ABlock(block.info, block.body.flatMap(assignHomesInInstr).filter(!_.isMoveTrivial))))
  )
}
