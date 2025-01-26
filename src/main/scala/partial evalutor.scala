package compiler

def peNeg(r: LExpr): LExpr = r match {
  case IntL(value) => IntL(-value)
  case _           => PrimL("-", Seq(r))
}

def peAdd(r1: LExpr, r2: LExpr): LExpr = (r1, r2) match {
  case (IntL(v1), IntL(v2)) => IntL(v1 + v2)
  case (IntL(0), r)         => r
  case (r, IntL(0))         => r
  case _                    => PrimL("+", Seq(r1, r2))
}

def peSub(r1: LExpr, r2: LExpr): LExpr = (r1, r2) match {
  case (IntL(v1), IntL(v2)) => IntL(v1 - v2)
  case (r, IntL(0))         => r
  case (IntL(0), r)         => peNeg(r)
  case _                    => PrimL("-", Seq(r1, r2))
}

def peExpr(e: LExpr): LExpr = e match {
  case IntL(n)                 => IntL(n)
  case PrimL("read", Seq())    => PrimL("read", Seq())
  case PrimL("-", Seq(r))      => peNeg(peExpr(r))
  case PrimL("+", rs)          => rs.map(peExpr).reduce(peAdd)
  case PrimL("-", Seq(r1, r2)) => peSub(peExpr(r1), peExpr(r2))
  case _                       => e
}

def peLint(p: ProgramL): ProgramL = ProgramL((), peExpr(p.body))
