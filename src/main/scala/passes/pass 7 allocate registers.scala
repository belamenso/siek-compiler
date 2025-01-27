package compiler
import AReg._

private val k = 11

val registerColorCorrespondence: Map[Int, AReg] = Map(
  -1 -> RAX, -2 -> RSP, -3 -> RBP, -4 -> R11, -5 -> R15,
  0 -> RCX, 1 -> RDX, 2 -> RSI, 3 -> RDI, 4 -> R8, 5 -> R9, 6 -> R10, 7 -> RBX, 8 -> R12, 9 -> R13, 10 -> R14
)

def allocateRegisters(program: Ax86Program): Ax86Program = {
  assert(registerColorCorrespondence.size == AReg.values.size)
  assert(registerColorCorrespondence.size == registerColorCorrespondence.values.toSet.size)

  Ax86Program(program.info, program.blocks)
}
