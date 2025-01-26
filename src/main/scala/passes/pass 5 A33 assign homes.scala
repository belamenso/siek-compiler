package compiler

def assignHomes(program: Ax86Program): Ax86Program = {
  val localTypes = program.info("local-types").asInstanceOf[Map[String, String]]
  assert(localTypes.values.toSet == Set("long"))
  val localTypesOrdered = localTypes.keys.toSeq

  def assignHomesInAsmArg(a: AsmArg): AsmArg = a match {
    case RegAsmArg(reg) => RegAsmArg(reg)
    case DerefAsmArg(reg, offset) => DerefAsmArg(reg, offset)
    case ImmAsmArg(value) => ImmAsmArg(value)
    case VarAsmArg(name) => DerefAsmArg(AReg.RBP, (localTypesOrdered.indexOf(name) + 1) * -8)
  }

  def assignHomesInInstr(i: AsmInstr): AsmInstr = i match {
    case AAddq(src, dst) => AAddq(assignHomesInAsmArg(src), assignHomesInAsmArg(dst))
    case ASubq(src, dst) => ASubq(assignHomesInAsmArg(src), assignHomesInAsmArg(dst))
    case AMovq(src, dst) => AMovq(assignHomesInAsmArg(src), assignHomesInAsmArg(dst))
    case ANegq(dst) => ANegq(assignHomesInAsmArg(dst))
    case APushq(src) => APushq(assignHomesInAsmArg(src))
    case APopq(dst) => APopq(assignHomesInAsmArg(dst))
    case ACallq(label, v) => ACallq(label, v)
    case AJmp(label) => AJmp(label)
    case ARetq() => ARetq()
  }

  var stackSpace = localTypesOrdered.size * 8
  if 0 != stackSpace % 16 then
    stackSpace += 8
  Ax86Program(program.info + ("stack-space" -> stackSpace), program.blocks.map((n, block) => (n -> ABlock(block.info, block.body.map(assignHomesInInstr)))))
}
