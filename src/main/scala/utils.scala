import scala.util.parsing.combinator._

object LispParser extends RegexParsers {
  override def skipWhitespace = true
  def integer: Parser[AstNode] = """[-]?\d+""".r ^^ { str => IntLit(str.toLong) }
  def operator: Parser[String] = "+" | "-"
  def read: Parser[AstNode] = "(read)" ^^ { _ =>
    Prim("read", Seq())
  }
  def expression: Parser[AstNode] = (
    integer
    | read
    | "(" ~> operator ~ rep1(expression) <~ ")" ^^ {
        case op ~ args => Prim(op, args)
      }
    | "(" ~> "-" ~ expression <~ ")" ^^ {
        case "-" ~ expr => Prim("-", Seq(expr))
      }
  )
  def program: Parser[Program] = expression ^^ { expr =>
    Program((), expr)
  }
  def parse(input: String): Either[String, Program] = {
    parse(program, input) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(msg)
      case _ => Left("???")
    }
  }
}

def readProgram(input: String): Program = {
  LispParser.parse(input) match {
    case Right(program) => program
    case Left(msg) => throw new IllegalArgumentException(msg)
  }
}
