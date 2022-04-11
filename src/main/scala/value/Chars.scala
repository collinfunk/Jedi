package value
import context._

case class Chars(value: String) extends Addable with Ordered[Value] {
  def +(other: Value): Addable =
    other match {
      case x: Chars => Chars(this.value + x.value)
      case x: Exact => Chars(this.value + x.value)
      case x: Inexact => Chars(this.value + x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  def subChars(to: Exact, from: Exact): Chars =
    Chars(this.value.substring(to.value, from.value))

  def subChars(to: Int, from: Int): Chars =
    this.subChars(Exact(to), Exact(from))

  def compare(other: Value): Int =
    other match {
      case x: Chars => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Chars => x.isInstanceOf[Chars] && x.value == this.value
      case _ => false
    }

  override def toString: String = this.value
  override def hashCode = this.toString.hashCode
}
