val Interpreter = new InterpreterLVar {}
type Env = Map[String, Long]

trait InterpreterLInt {
  def interpExp(env: Env)(e: ExprNode): Long = e match {
    case IntLit(value) => value
    case Prim("+", args) => args.map(interpExp(env)).sum
    case Prim("-", Seq(e)) => -interpExp(env)(e)
    case Prim("-", e1 +: es) => interpExp(env)(e1) - es.map(interpExp(env)).sum
    case Prim("read", Seq()) => scala.io.StdIn.readLine().toInt
    case _ => throw new IllegalArgumentException(s"interpExp: $e")
  }

  def interpretProgram(p: Program): Long = interpExp(Map())(p.body)
}

trait InterpreterLVar extends InterpreterLInt {
  override def interpExp(env: Env)(e: ExprNode): Long = e match {
    case Var(name) => env(name)
    case Let(name, value, body) =>
      val newEnv = env + (name -> interpExp(env)(value))
      interpExp(newEnv)(body)
    case _ => super.interpExp(env)(e)
  }
}
