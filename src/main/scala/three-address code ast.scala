trait CAtom
case class IntAtomExpr(value: Long) extends CAtom
case class VarAtomExpr(name: String) extends CAtom

trait CExpr
case class AtomExprCExpr(atom: CAtom) extends CExpr
case class PrimCExpr(op: String, args: Seq[CAtom]) extends CExpr {
  assert(Seq("read", "+", "-").contains(op))
  if (op == "-") assert(Seq(1, 2).contains(args.length))
  if (op == "+") assert(args.length == 2)
  if (op == "read") assert(args.isEmpty)
}

trait CStmt
case class AssignStmt(name: String, expr: CExpr) extends CStmt

trait CTail
case class ReturnTail(expr: CExpr) extends CTail
case class SeqTail(stmt: CStmt, tail: CTail) extends CTail

case class CProgram(info: Seq[Nothing], body: Map[String, CTail])
