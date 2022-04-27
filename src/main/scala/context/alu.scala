package context

import expression._
import value._

object alu:
  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match
      case "add" => add(args)            // n-ary
      case "mul" => mul(args)            // n-ary
      case "sub" => sub(args)            // n-ary
      case "div" => div(args)            // n-ary
      case "less" => less(args)          // binary
      case "equals" => same(args)        // binary
      case "more" => more(args)          // binary
      case "unequals" => unequals(args)  // binary
      case "not" => not(args)            // unary

      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)

      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)

      case "car" => car(args)
      case "cdr" => cdr(args)
      case "cons" => cons(args)
      case "nil" => getEmpty()
      case "list" => list(args)

      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
  // TBC

  private def add(args: List[Value]): Value =
    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match
      case n: Addable => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")

  private def sub(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Addable =
      if (unseen.isEmpty) result
      else unseen.head match {
        case h: value.Numeric => helper(result - h, unseen.tail)
        case _ => throw TypeException("Inputs to - must be numeric")
      }

    if(args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to - must be Numeric")
    }

  private def mul(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by *")
    args(0) match
      case n: value.Numeric => helper(n, args.tail )
      case _ => throw new TypeException("Inputs to * must be numeric")

  private def div(args: List[Value]): Value =
    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen.isEmpty) result
      else unseen.head match
        case h: value.Numeric => helper(result / h, unseen.tail)
        case _ => throw TypeException("Inputs to / must be numeric")

    if(args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match
      case n: value.Numeric => helper(n, args.tail)
      case _ => throw new TypeException("Inputs to / must be numeric")

  private def less(args: List[Value]): Value =
    if (args.size != 2) throw new TypeException("2 inputs required by <")
    args(0) match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def same(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required for equals")
    Boole(args(0) == args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required for unequals")
    Boole(args(0) != args(1))
  }

  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw new TypeException("2 inputs required by >")
    args(0) match
      case x: Ordered[Value] => Boole(x > args(1))
      case _ => throw TypeException("Inputs to > must be orderable")
  }

  private def not(args: List[Value]): Value = {
    if (args.size != 1) throw new TypeException("1 input required by !")
    args(0) match {
      case x: Boole => Boole(!x.value)
      case _ => throw TypeException("Inputs to ! must be Boole")
    }
  }

  private def write(args: List[Value]): Value =
    println(args(0))
    Notification.DONE

  private def read(args: List[Value]): Value = {
    val result = io.StdIn.readDouble();
    Inexact(result)
  }

  private def prompt(args: List[Value]): Value = {
    print("=> ");
    Notification.DONE
  }

  private def car(args: List[Value]): Value = args(0).asInstanceOf[Pair].first
  private def cdr(args: List[Value]): Value = args(0).asInstanceOf[Pair].second
  private def cons(args: List[Value]): Value = Pair(args(0), args(1))
  private def getEmpty(): Value = empty
  private def list(args: List[Value]): Value =
    if (args == Nil)
      getEmpty()
    else
      Pair(args.head, list(args.tail))

  private def dereference(args: List[Value]) = {
    if (args.size != 1 || !args(0).isInstanceOf[Variable]) throw new TypeException("Derefrence expects a single variable")
    args(0).asInstanceOf[Variable].content
  }

  private def makeVar(args: List[Value]) = {
    if (args.size != 1) throw new TypeException("1 input required by makeVar")
    new Variable(args.head)
  }

  private def store(args: List[Value]): Value = {
    val returnStore = new Store
    for (item <- args)
      returnStore.add(item)
    returnStore
  }

  // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
  private def put(args: List[Value]) = {
    if (args.size != 3)
      throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
    if(!args(1).isInstanceOf[Exact] || !args(2).isInstanceOf[Store])
      throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
    args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Exact].value)
    Notification.DONE
  }

  // rem(p: Integer, s: Store) calls s.rem(p)
  private def rem(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: rem(p: Integer, s: Store)")
    args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
    Notification.DONE
  }

  // get(p: Integer, s: Store) calls s.get(p)
  private def get(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Exact] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: get(p: Integer, s: Store)")
    args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Exact].value)
  }

  // map(f: Closure, s: Store) calls s.map(f)
  private def map(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: map(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
  }

  // filter(f: Closure, s: Store) calls s.filter(f)
  private def filter(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: filter(f: Closure, s: Store)")
    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
  }

  // contains(v: Value, s: Store) calls s.contains(v)
  private def contains(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: contains(v: Value, s: Store)")
    args(1).asInstanceOf[Store].contains(args(0).asInstanceOf[Value])
  }

  // addLast(v: Value, s: Store) calls s.add(v)
  private def addLast(args: List[Value]): Value = {
    if (args.size != 2 || !args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store]) throw new TypeException("Must be in form: addLast(v: Value, s: Store)")
    args(1).asInstanceOf[Store].add(args(0).asInstanceOf[Value])
    Notification.DONE
  }

  // size(s: Store) calls s.size
  private def size(args: List[Value]): Value = {
    if (args.size != 1 || !args(0).isInstanceOf[Store]) throw new TypeException("Must be in form: size(s: Store)")
    Exact(args(0).asInstanceOf[Store].size)
  }

// etc.