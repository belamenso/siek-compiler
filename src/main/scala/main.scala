package compiler

import pprint.pprintln

@main def main(): Unit = {
  val p = """(let [y (let [x 20] (+ x (let [x 22] x)))] y)"""
  val x = (uniquify(readProgram(p)))
  val x1 = removeComplexOperands(x)
  val y = explicateControl(x1)

  pprintln(x1)
  println()
  pprintln(y)
}
