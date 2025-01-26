package compiler

import pprint.pprintln

@main def main(): Unit = {
  pprintln(removeComplexOperands(
    readProgram("(let [x (+ 42 (- 10))] (+ x 10))")
  ))
}
