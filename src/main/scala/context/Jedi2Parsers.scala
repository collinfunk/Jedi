package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(con ~ Nil)   => List(con)
    case Some(con ~ next) => con :: next
    case _ => Nil
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ params ~ expression => Lambda(params, expression)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ expression ~ Nil ~ "}" => Block(List(expression))
    case "{" ~ expression ~ nextExpression ~ "}" => Block(expression::nextExpression)
  }

  // freeze parser
  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a MakeThunk expression
  def freeze: Parser[MakeThunk] = "freeze" ~ "(" ~ expression ~ ")" ^^ {
    case "freeze" ~ "(" ~ expression ~ ")" => MakeThunk(expression)
  }

  // delay parser
  // delay ::= "delay" ~ "(" ~ expression ~ ")" // makes a MakeText expression
  def delay: Parser[MakeText] = "delay" ~ "(" ~ expression ~ ")" ^^ {
    case "delay" ~ "(" ~ expression ~ ")" => MakeText(expression)
  }


  // override of term parser
  override def term: Parser[Expression]  = lambda | freeze | delay | funCall | block | literal | "("~>expression<~")"


}
