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

  CProgram(Seq(), Map("start" -> explicateControlTail(prog.body)))
}
