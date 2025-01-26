import pprint.pprintln

@main def main(): Unit = {
  pprintln(uniquify(
    readProgram("(let [x (+ 1 1)] (+ (let [x 12] x) x))")
  ))
}
