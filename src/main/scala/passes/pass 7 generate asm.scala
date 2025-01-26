package compiler

def generateAsm(program: Ax86Program): String = program.str
def compileAndRun(program: Ax86Program): Long =
  val asm = generateAsm(program)
  val path = "out.s"
  val writer = java.io.PrintWriter(path)
  writer.write(asm)
  writer.close()
  // execute the command `gcc -o out out.s` and return the exit code
  val process = new ProcessBuilder("gcc", "-o", "out", path).start()
  process.waitFor()
  val process2 = new ProcessBuilder("./out").start()
  process2.waitFor()
  process2.exitValue()

private def conclusion(stackSpace: Long) = """
  .globl main
main:
  pushq %rbp
  movq %rsp, %rbp
  subq $""" + stackSpace + """, %rsp
  jmp start

conclusion:
  addq $""" + stackSpace + """, %rsp
  popq %rbp
  retq
"""

extension (a: AReg)
  def str: String = a match {
    case AReg.RAX => "%rax"
    case AReg.RBX => "%rbx"
    case AReg.RCX => "%rcx"
    case AReg.RDX => "%rdx"
    case AReg.RBP => "%rbp"
    case AReg.RSP => "%rsp"
    case AReg.RSI => "%rsi"
    case AReg.RDI => "%rdi"
    case AReg.R8 => "%r8"
    case AReg.R9 => "%r9"
    case AReg.R10 => "%r10"
    case AReg.R11 => "%r11"
    case AReg.R12 => "%r12"
    case AReg.R13 => "%r13"
    case AReg.R14 => "%r14"
    case AReg.R15 => "%r15"
  }

extension (a: AsmArg)
  def str: String = a match {
    case ImmAsmArg(value) => "$" + value
    case RegAsmArg(reg) => reg.str
    case DerefAsmArg(reg, offset) => s"$offset(${reg.str})"
    case VarAsmArg(name) => assert(false, "VarAsmArg should have been replaced by now")
  }

extension (i: AsmInstr)
  def str: String = i match {
    case AAddq(src, dst) => s"addq ${src.str}, ${dst.str}"
    case ASubq(src, dst) => s"subq ${src.str}, ${dst.str}"
    case AMovq(src, dst) => s"movq ${src.str}, ${dst.str}"
    case ANegq(dst) => s"negq ${dst.str}"
    case APushq(src) => s"pushq ${src.str}"
    case APopq(dst) => s"popq ${dst.str}"
    case ACallq(label, v) => s"callq $label"
    case AJmp(label) => s"jmp $label"
    case ARetq() => "retq"
  }

extension (b: ABlock)
  def str: String = b.body.map("  " + _.str).mkString("\n")

extension (p: Ax86Program)
  def str: String = {
    val blocks = p.blocks.map { case (name, block) => s"$name:\n${block.str}" }.mkString("\n")
    s"$blocks\n${conclusion(p.info("stack-space").asInstanceOf[Long])}"
  }
