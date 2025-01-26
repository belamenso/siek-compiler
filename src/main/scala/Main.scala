import scala.compiletime.ops.int
trait AstNode
case class IntLit (value: Long) extends AstNode
case class Prim (op: String, args: Seq[AstNode]) extends AstNode
case class Program(info: Unit, body: AstNode)

def interpExp(e: AstNode): Long = e match {
  case IntLit(value) => value
  case Prim("+", args) => args.map(interpExp).sum
  case Prim("-", Seq(e)) => -interpExp(e)
  case Prim("-", e1 +: es) => interpExp(e1) - es.map(interpExp).sum
  case Prim("read", Seq()) => scala.io.StdIn.readLine().toInt
  case _ => throw new IllegalArgumentException(s"interpExp: $e")
}

def interpLint(p: Program): Long = interpExp(p.body)

def peNeg(r: AstNode): AstNode = r match {
  case IntLit(value) => IntLit(-value)
  case _ => Prim("-", Seq(r))
}

def peAdd(r1: AstNode, r2: AstNode): AstNode = (r1, r2) match {
  case (IntLit(v1), IntLit(v2)) => IntLit(v1 + v2)
  case (IntLit(0), r) => r
  case (r, IntLit(0)) => r
  case _ => Prim("+", Seq(r1, r2))
}

def peSub(r1: AstNode, r2: AstNode): AstNode = (r1, r2) match {
  case (IntLit(v1), IntLit(v2)) => IntLit(v1 - v2)
  case (r, IntLit(0)) => r
  case (IntLit(0), r) => peNeg(r)
  case _ => Prim("-", Seq(r1, r2))
}

def peExpr(e: AstNode): AstNode = e match {
  case IntLit(n) => IntLit(n)
  case Prim("read", Seq()) => Prim("read", Seq())
  case Prim("-", Seq(r)) => peNeg(peExpr(r))
  case Prim("+", rs) => rs.map(peExpr).reduce(peAdd)
  case Prim("-", Seq(r1, r2)) => peSub(peExpr(r1), peExpr(r2))
  case _ => e
}

def peLint(p: Program): Program = Program((), peExpr(p.body))

@main def main(): Unit = {
  println(interpLint(
    readProgram("(+ -8 10)")
  ))

  println(peLint(
    readProgram("(+ (read) (+ 1 2 3))")
  ))
}
