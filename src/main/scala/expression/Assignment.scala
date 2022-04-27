package expression
import context._
import value.{Notification, Value, Variable}

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value = {
    var temp = vbl.execute(env)
    if (!temp.isInstanceOf[Variable]) throw new TypeException("Only Variable objects can be updated")
    temp.asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}
