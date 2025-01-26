import scala.util.parsing.combinator._

object LispParser extends RegexParsers {
  override def skipWhitespace = true
  def integer: Parser[LExpr] = """[-]?\d+""".r ^^ { str => IntL(str.toLong) }
  def variableRef: Parser[LExpr] = """[\.a-zA-Z_-][\.a-zA-Z0-9_-]*""".r ^^ { str => VarL(str) }
  def operator: Parser[String] = "+" | "-"
  def read: Parser[LExpr] = "(read)" ^^ { _ =>
    PrimL("read", Seq())
  }
  def expression: Parser[LExpr] = (
    integer
    | read
    | "(" ~> operator ~ rep1(expression) <~ ")" ^^ {
        case op ~ args => PrimL(op, args)
      }
    | "(" ~> "-" ~ expression <~ ")" ^^ {
        case "-" ~ expr => PrimL("-", Seq(expr))
      }
    | ("(" ~> "let" ~> "[" ~> variableRef ~ expression ~ ("]" ~> expression) <~ ")") ^^ {
        case VarL(name) ~ value ~ body => LetL(name, value, body)
        case _ => assert(false)
      }
    | variableRef
  )
  def program: Parser[ProgramL] = expression ^^ { expr =>
    ProgramL((), expr)
  }
  def parse(input: String): Either[String, ProgramL] = {
    parse(program, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
      case _ => Left("???")
    }
  }
}

def readProgram(input: String): ProgramL = {
  LispParser.parse(input) match {
    case Right(program) => program
    case Left(msg) => throw new IllegalArgumentException(msg)
  }
}
