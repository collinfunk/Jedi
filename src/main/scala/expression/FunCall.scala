package expression

import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression  {
  def execute(env: Enviornment): Value = {
    var args: List[Value] = Nil

    if (env.contains(operator)) {
      args = operands.map(_.execute(env))
      operator.execute(env) match {
        case closure: Closure => closure.apply(args)
      }
    } else {
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }
  }
}
