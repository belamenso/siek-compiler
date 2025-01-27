package compiler

extension (r: AReg)
  def isCalleeSaved: Boolean = !r.isCallerSaved
  def isCallerSaved: Boolean = r match
    case AReg.RAX | AReg.RCX | AReg.RDX | AReg.RSI | AReg.RDI | AReg.R8 | AReg.R9 | AReg.R10 | AReg.R11 => true
    case _ => false

val argumentPassingRegisters = Seq(AReg.RDI, AReg.RSI, AReg.RDX, AReg.RCX, AReg.R8, AReg.R9)

extension (i: AsmInstr)
  def readLocations(liveInLabels: Map[String, Set[AsmArg]]): Set[AsmArg] = (i match
    case AAddq(src, dst) => Set(src, dst)
    case ASubq(src, dst) => Set(src, dst)
    case AMovq(src, dst) => Set(src)
    case ANegq(dst) => Set(dst)
    case APushq(src) => Set(src, RegAsmArg(AReg.RSP))
    case APopq(dst) => Set(RegAsmArg(AReg.RSP))
    case ACallq(label, arity) => argumentPassingRegisters.take(arity).map(RegAsmArg(_)).toSet
    case AJmp(label) => liveInLabels(label)
    case ARetq() => Set()).filter{
      case RegAsmArg(_) | DerefAsmArg(_, _) | VarAsmArg(_) => true
      case ImmAsmArg(_) => false
    }

  def writtenLocations: Set[AsmArg] = (i match
    case AAddq(src, dst) => Set(dst)
    case ASubq(src, dst) => Set(dst)
    case AMovq(src, dst) => Set(dst)
    case ANegq(dst) => Set(dst)
    case APushq(src) => Set(RegAsmArg(AReg.RSP))
    case APopq(dst) => Set(dst, RegAsmArg(AReg.RSP))
    case ACallq(label, arity) => AReg.values.filter(_.isCallerSaved).map(RegAsmArg(_)).toSet + RegAsmArg(AReg.RSP)
    case AJmp(label) => Set()
    case ARetq() => Set(RegAsmArg(AReg.RSP))).filter{
      case RegAsmArg(_) | DerefAsmArg(_, _) | VarAsmArg(_) => true
      case ImmAsmArg(_) => false
    }

def computeLiveAfterSets(instructions: Seq[AsmInstr], lastOne: Set[AsmArg], liveInLabels: Map[String, Set[AsmArg]]): Seq[Set[AsmArg]] = {
  val liveAfterSets = instructions.scanRight(lastOne){ case (i, liveAfter) =>
    (liveAfter -- i.writtenLocations) ++ i.readLocations(liveInLabels)
  }
  liveAfterSets
}

def uncoverLive(program: Ax86Program): Ax86Program = {
  assert(program.blocks.size == 1 && program.blocks.contains("start"))
  val liveInLabels = Map("conclusion" -> Set[AsmArg](RegAsmArg(AReg.RAX), RegAsmArg(AReg.RSP)))
  val liveAfterSetsForStart = computeLiveAfterSets(program.blocks("start").body, Set(), liveInLabels)
  val onlyBlock = program.blocks("start")
  val newBlock = ABlock(
    info = onlyBlock.info ++ Map("live-after" -> liveAfterSetsForStart),
    body = onlyBlock.body)
  Ax86Program(
    info = program.info ++ Map("label->live" -> liveInLabels),
    blocks = Map("start" -> newBlock))
}
