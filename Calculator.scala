import scala.util.parsing.combinator._

abstract class Term

case class Variable(name: String) extends Term
case class Numeric(value: Double) extends Term
case class Addition(left: Term, right: Term) extends Term
case class Multiplication(left: Term, right: Term) extends Term
case class Brackets(term: Term) extends Term

trait Parser extends RegexParsers {
  def numeric: Parser[Term] = """\d+""".r ^^ {s => new Numeric(s.toInt)}
  def variable: Parser[Term] = """\w""".r ^^ {s => new Variable(s)}
  val atomic = numeric | variable
  lazy val brackets: Parser[Term] = "(" ~ term ~")" ^^ { case "(" ~ term ~ ")" => Brackets(term) }
  lazy val brackets2: Parser[Term] = "(" ~ term ~")" ~ atomic ^^ { case "(" ~ term ~ ")" ~ right => Brackets(term) }
  lazy val brackets3: Parser[Term] = atomic ~ "(" ~ term ~")" ~ atomic ^^ { case left ~ "(" ~ term ~ ")" ~ right => Brackets(term) }
  lazy val puVoSt: Parser[Term] =  atomic ~ "*" ~ atomic ~ "+" ~ atomic ^^ { case atomic1 ~ "*" ~ atomic2 ~ "+" ~ atomic3 => Addition(Multiplication(atomic1,atomic2),atomic3) }
  lazy val addition: Parser[Term] = atomic ~ "+" ~ term ^^ {case left ~ "+" ~ right => Addition(left, right)}
  lazy val multiplication: Parser[Term] = atomic ~ "*" ~ term ^^ {case left ~ "*" ~ right => Multiplication(left, right)}
  val term = puVoSt | brackets | brackets2 | brackets3 | addition | multiplication | atomic
}

object Calculator extends Parser {

  def eval(t: Term): Double = t match {
    case Brackets(term) => eval(term)
    case Addition(l, r) => eval(l) + eval(r)
    case Multiplication(l, r) => eval(l) * eval(r)
    case Numeric(i) => i
    case Variable("X") => 3
  }

  def calculate(arg: String): Double = {
    return eval(parseAll(term, arg).get)
  }

  def main(args: Array[String]) {
    println(calculate("2 * (7 + 2) "))
  }
}