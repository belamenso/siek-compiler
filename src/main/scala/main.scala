@main def main(): Unit = {
  println(Interpreter.interpretProgram(
    readProgram("(let [x (+ 1 1)] (+ x x))")
  ))
}
