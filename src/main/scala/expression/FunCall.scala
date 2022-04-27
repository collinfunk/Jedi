package expression

import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression  {
  def execute(env: Enviornment): Value = {
    var args: List[Value] = Nil

    if (env.contains(operator)) {
      if (flags.paramPassing == flags.BY_NAME) {
        args = operands.map(MakeThunk(_).execute(env))
      } else {
        args = operands.map(_.execute(env))
      }
      operator.execute(env) match {
        case thunk: Thunk => thunk.apply()
        case closure: Closure => closure.apply(args, env)
      }
    } else {
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }
  }
}
