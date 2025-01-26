trait ExprNode
case class IntLit (value: Long) extends ExprNode
case class Prim (op: String, args: Seq[ExprNode]) extends ExprNode
case class Var (name: String) extends ExprNode
case class Let (name: String, value: ExprNode, body: ExprNode) extends ExprNode

case class Program(info: Unit, body: ExprNode)
