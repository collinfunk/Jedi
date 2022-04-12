package context
import expression.Identifier
import value.Value
import scala.collection.mutable.*

class Enviornment(var extension: Enviornment = null) extends HashMap[Identifier, Value] with Value {
  def bulkPut(params: List[Identifier], args: List[Value]) =
    if (params.length != args.length) throw TypeException("# arguments != #parameters")
    for(i <- 0 until params.length) this.put(params(i), args(i))


  override def contains(name: Identifier): Boolean =
    super.contains(name) || (extension != null && extension.contains(name))


  override def apply(name: Identifier): Value =
    if (super.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw UndefinedException(name)
}
