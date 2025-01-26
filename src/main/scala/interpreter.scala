package compiler

val Interpreter = new InterpreterLVar {}
type Env = Map[String, Long]

trait InterpreterLInt {
  def interpExp(env: Env)(e: LExpr): Long = e match {
    case IntL(value) => value
    case PrimL("+", args) => args.map(interpExp(env)).sum
    case PrimL("-", Seq(e)) => -interpExp(env)(e)
    case PrimL("-", e1 +: es) => interpExp(env)(e1) - es.map(interpExp(env)).sum
    case PrimL("read", Seq()) => scala.io.StdIn.readLine().toInt
    case _ => throw new IllegalArgumentException(s"interpExp: $e")
  }

  def interpretProgram(p: ProgramL): Long = interpExp(Map())(p.body)
}

trait InterpreterLVar extends InterpreterLInt {
  override def interpExp(env: Env)(e: LExpr): Long = e match {
    case VarL(name) => env(name)
    case LetL(name, value, body) =>
      val newEnv = env + (name -> interpExp(env)(value))
      interpExp(newEnv)(body)
    case _ => super.interpExp(env)(e)
  }
}
