def uniquify(prog: ProgramL): ProgramL = {
  var counter = 1

  def uniquifyExpr(env: Map[String, String])(expr: LExpr): LExpr = expr match {
    case IntL(value) => IntL(value)
    case PrimL(op, args) => {
      PrimL(op, args.map(uniquifyExpr(env)))
    }
    case VarL(name) => VarL(env(name))
    case LetL(name, value, body) => {
      val newName = name + "." + counter
      counter += 1
      val newValue = uniquifyExpr(env)(value)
      val newBody = uniquifyExpr(env + (name -> newName))(body)
      LetL(newName, newValue, newBody)
    }
  }
  ProgramL(prog.info, uniquifyExpr(Map())(prog.body))
}
