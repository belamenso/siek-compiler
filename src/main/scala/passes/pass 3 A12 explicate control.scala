package compiler

def explicateControl(prog: ProgramL): CProgram = {

  def explicateControlTail(e: LExpr): CTail = e match
    case IntL(n)            => ReturnCTail(CInt(n))
    case VarL(x)            => ReturnCTail(CVar(x))
    case LetL(x, rhs, body) => explicateControlAssign(rhs, x, explicateControlTail(body))
    case PrimL(op, args) => ReturnCTail(PrimCExpr(
        op,
        args.map {
          case IntL(value) => CInt(value)
          case VarL(name)  => CVar(name)
        }
      ))

  def explicateControlAssign(e: LExpr, x: String, cont: CTail): CTail = e match
    case IntL(n)            => SeqCTail(AssignCStmt(x, CInt(n)), cont)
    case VarL(y)            => SeqCTail(AssignCStmt(x, CVar(y)), cont)
    case LetL(y, rhs, body) => explicateControlAssign(rhs, y, explicateControlAssign(body, x, cont))
    case PrimL(op, args) => SeqCTail(
        AssignCStmt(
          x,
          PrimCExpr(
            op,
            args.map {
              case IntL(value) => CInt(value)
              case VarL(name)  => CVar(name)
            }
          )
        ),
        cont
      )

  val body = explicateControlTail(prog.body)
  CProgram(Map("local-types" -> extractVariableNamesTail(body).map(n => (n, "long")).toMap), Map("start" -> body))
}

private def extractVariableNamesTail(e: CTail): Set[String] = e match
  case ReturnCTail(expr)    => extractVariableNamesExpr(expr)
  case SeqCTail(stmt, tail) => extractVariableNamesStmt(stmt) ++ extractVariableNamesTail(tail)

private def extractVariableNamesExpr(e: CExpr): Set[String] = e match
  case CInt(_)            => Set()
  case CVar(name)         => Set(name)
  case PrimCExpr(_, args) => args.flatMap(extractVariableNamesExpr).toSet

private def extractVariableNamesStmt(e: CStmt): Set[String] = e match
  case AssignCStmt(name, expr) => Set(name) ++ extractVariableNamesExpr(expr)
