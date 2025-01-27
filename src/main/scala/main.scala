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

def helper(program: String): Any =
  val x = (uniquify(readProgram(program)))
  val x1 = removeComplexOperands(x)
  val y = explicateControl(x1)
  val y1 = selectInstructions(y)
  val xxx = uncoverLive(y1)
  val xxx1 = buildInterference(xxx)
  xxx1

@main def main(): Unit = {
  pprintln(helper("""(let [a 5] (let [b 30] (let [c a] (let [b 10] (+ b c)))))"""))
}
