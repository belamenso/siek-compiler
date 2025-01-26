enum Reg:
  case RAX, RBX, RCX, RDX, RBP, RSP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15

trait AsmArg
case class Imm(value: Long) extends AsmArg
case class RegArg(reg: Reg) extends AsmArg
case class Deref(reg: Reg, offset: Long) extends AsmArg
case class AsmVar(name: String) extends AsmArg // from the langauge x86_var

trait AsmInstr // in x86_var you can have arbitratry arguments, but in x86 at most 1 memory reference
case class InstrAddq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class InstrSubq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class InstrMovq(src: AsmArg, dst: AsmArg) extends AsmInstr
case class InstrNegq(dst: AsmArg) extends AsmInstr
case class InstrPushq(src: AsmArg) extends AsmInstr
case class InstrPopq(dst: AsmArg) extends AsmInstr
case class Callq(label: String, v: Long) extends AsmInstr
case class Jmp(label: String) extends AsmInstr
case class Retq() extends AsmInstr

case class Block(info: Seq[Nothing], body: Seq[AsmInstr])
case class x86Program(info: Seq[Nothing], blocks: Map[String, Block])
