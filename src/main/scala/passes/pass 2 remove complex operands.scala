package compiler

def removeComplexOperans(): Unit =
  uniquify(
    readProgram("(let [x (+ 1 1)] (+ (let [x 12] x) x))")
  )
