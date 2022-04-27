package expression
import context.*
import value.{Boole, Notification, Value}

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole]) throw new TypeException("Condition must return Boole")
    while (condition.execute(env).asInstanceOf[Boole].value) {
      body.execute(env)
    }
    Notification.DONE
  }
}
