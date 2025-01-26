package compiler

import pprint.pprintln

def compileAndRunProgram(program: String): Long =
  val x = (uniquify(readProgram(program)))
  val x1 = removeComplexOperands(x)
  val y = explicateControl(x1)
  val y1 = selectInstructions(y)
  val z = assignHomes(y1)
  val z1 = patchInstructions(z)
  compileAndRun(z1)

@main def main(): Unit = {
  println(compileAndRunProgram("""(let [a 10] (let [b 23] (+ a (- b 19))))"""))
}
