package expression
import context._
import value._

case class Disjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Enviornment): Value =
    var returnVal = false
    if (operands.size < 2) throw new TypeException("Conjunctions must have 2 or more operands")
    for (operand <- operands if !returnVal)
      val executeVal = operand.execute(env)
      if (!executeVal.isInstanceOf[Boole])
        throw new TypeException("Operand must be of type Boole")
      returnVal = executeVal.asInstanceOf[Boole].value
    Boole(returnVal)

}