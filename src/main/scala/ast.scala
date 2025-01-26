trait AstNode
case class IntLit (value: Long) extends AstNode
case class Prim (op: String, args: Seq[AstNode]) extends AstNode
case class Program(info: Unit, body: AstNode)
