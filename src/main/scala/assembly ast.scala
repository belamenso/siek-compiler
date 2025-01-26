enum AReg:
  case RAX, RBX, RCX, RDX, RBP, RSP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15

trait AsmArg
case class ImmAsmArg(value: Long) extends AsmArg
case class RegAsmArg(reg: AReg) extends AsmArg
case class DerefAsmArg(reg: AReg, offset: Long) extends AsmArg
case class VarAsmArg(name: String) extends AsmArg // from the langauge x86_var

trait AsmInstr // in x86_var you can have arbitratry arguments, but in x86 at most 1 memory reference
case class AAddq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class ASubq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class AMovq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class ANegq(dst: AsmArg) extends AsmInstr
case class APushq(src: AsmArg) extends AsmInstr
case class APopq(dst: AsmArg) extends AsmInstr
case class ACallq(label: String, v: Long) extends AsmInstr
case class AJmp(label: String) extends AsmInstr
case class ARetq() extends AsmInstr

case class ABlock(info: Seq[Nothing], body: Seq[AsmInstr])
case class Ax86Program(info: Seq[Nothing], blocks: Map[String, ABlock])
