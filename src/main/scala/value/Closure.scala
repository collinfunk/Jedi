package value

import expression.{Expression, Identifier}
import context.{Enviornment, TypeException, flags}

class Closure(parameters: List[Identifier], defEnv: Enviornment, body: Expression) extends Value {
  def apply(args: List[Value], callingEnviornment: Enviornment): Value =
    var tempEnviornment = new Enviornment(defEnv)
    if (flags.staticScope == false)
      tempEnviornment = new Enviornment(callingEnviornment)
    if (parameters.length != args.length) throw new TypeException("Args is not equal to parameters")
    for (i <- 0 until parameters.length)
      tempEnviornment.bulkPut(parameters, args)
    body.execute(tempEnviornment)
}
