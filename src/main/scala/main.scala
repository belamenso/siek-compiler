package compiler

import pprint.pprintln

@main def main(): Unit = {
  val p = """(let [a 1] (let [b a] b))"""
  val x = (uniquify(readProgram(p)))
  val x1 = removeComplexOperands(x)
  val y = explicateControl(x1)
  val y1 = selectInstructions(y)
  val z = assignHomes(y1)
  val z1 = patchInstructions(z)

  pprintln(x1)
  println()
  pprintln(y)
  println()
  pprintln(y1)
  println()
  pprintln(z1)
}
