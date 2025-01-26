trait ExprNode
case class IntLit (value: Long) extends ExprNode
case class Prim (op: String, args: Seq[ExprNode]) extends ExprNode {
  assert(Seq("read", "+", "-").contains(op))
  if (op == "-") assert(Seq(1, 2).contains(args.length))
  if (op == "+") assert(args.length == 2)
  if (op == "read") assert(args.isEmpty)
}
case class Var (name: String) extends ExprNode
case class Let (name: String, value: ExprNode, body: ExprNode) extends ExprNode

case class Program(info: Unit, body: ExprNode)
