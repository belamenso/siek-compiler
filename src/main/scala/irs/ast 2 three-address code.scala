package compiler

trait CAtom extends CExpr
case class CInt(value: Long) extends CAtom
case class CVar(name: String) extends CAtom

trait CExpr
case class PrimCExpr(op: String, args: Seq[CAtom]) extends CExpr {
  assert(Seq("read", "+", "-").contains(op))
  if (op == "-") assert(Seq(1, 2).contains(args.length))
  if (op == "+") assert(args.length == 2)
  if (op == "read") assert(args.isEmpty)
}

trait CStmt
case class AssignCStmt(name: String, expr: CExpr) extends CStmt

trait CTail
case class ReturnCTail(expr: CExpr) extends CTail
case class SeqCTail(stmt: CStmt, tail: CTail) extends CTail

case class CProgram(info: Seq[Nothing], body: Map[String, CTail])
