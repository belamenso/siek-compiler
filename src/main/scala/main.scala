@main def main(): Unit = {
  println(interpLint(
    readProgram("(+ -8 10)")
  ))

  println(peLint(
    readProgram("(+ (read) (+ 1 2 3))")
  ))
}
