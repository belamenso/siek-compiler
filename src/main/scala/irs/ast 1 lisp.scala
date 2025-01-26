package compiler

trait LExpr
case class IntL(value: Long) extends LExpr
case class VarL(name: String) extends LExpr
case class PrimL(op: String, args: Seq[LExpr]) extends LExpr {
  assert(Seq("read", "+", "-").contains(op))
  if (op == "-") assert(Seq(1, 2).contains(args.length))
  if (op == "+") assert(args.length == 2)
  if (op == "read") assert(args.isEmpty)
}
case class LetL(name: String, value: LExpr, body: LExpr) extends LExpr

case class ProgramL(info: Unit, body: LExpr)
