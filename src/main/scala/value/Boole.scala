package value
import expression.Literal
import context._

case class Boole(value: Boolean) extends Literal{
  def &&(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  def ||(other: Value): Boole =
    other match {
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  def unary_! = Boole(!this.value)

  override def toString: String = value.toString

  override def hashCode = this.toString.hashCode

}

object Boole {
  def FALSE = Boole(false)
  def TRUE = Boole(true)
}
