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
