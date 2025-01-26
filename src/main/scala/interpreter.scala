def interpExp(e: AstNode): Long = e match {
  case IntLit(value) => value
  case Prim("+", args) => args.map(interpExp).sum
  case Prim("-", Seq(e)) => -interpExp(e)
  case Prim("-", e1 +: es) => interpExp(e1) - es.map(interpExp).sum
  case Prim("read", Seq()) => scala.io.StdIn.readLine().toInt
  case _ => throw new IllegalArgumentException(s"interpExp: $e")
}

def interpLint(p: Program): Long = interpExp(p.body)
