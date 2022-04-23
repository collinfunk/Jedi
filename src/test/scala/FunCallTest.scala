import context.Enviornment
import expression.{FunCall, Identifier}
import value.Exact

object FunCallTest extends App {
  val globalEnvironment = new Enviornment
  val operands = List(Exact(6), Exact(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment))
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment))
}
