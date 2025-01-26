package compiler

import pprint.pprintln

@main def main(): Unit = {
  val p = """(let [y (read)] y)"""
  val x = (uniquify(readProgram(p)))
  val x1 = removeComplexOperands(x)
  val y = explicateControl(x1)
  val y1 = selectInstructions(y)
  val z = assignHomes(y1)

  pprintln(x1)
  println()
  pprintln(y)
  println()
  pprintln(y1)
  println()
  pprintln(z)
}
