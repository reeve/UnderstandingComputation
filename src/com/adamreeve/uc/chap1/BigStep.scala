package com.adamreeve.uc.chap1

// --- VALUE EXPRESSIONS
case class SimpleNumber(value: Int) extends ValueExpression

case class SimpleBoolean(value: Boolean) extends ValueExpression

// --- COMPLEX EXPRESSIONS
case class Add(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleNumber] {
  def perform(left: SimpleNumber, right: SimpleNumber): SimpleNumber = SimpleNumber(left.value + right.value)
}

case class Subtract(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleNumber] {
  def perform(left: SimpleNumber, right: SimpleNumber): SimpleNumber = SimpleNumber(left.value - right.value)
}

case class Multiply(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleNumber] {
  def perform(left: SimpleNumber, right: SimpleNumber): SimpleNumber = SimpleNumber(left.value * right.value)
}

case class Divide(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleNumber] {
  def perform(left: SimpleNumber, right: SimpleNumber): SimpleNumber = SimpleNumber(Math.round(left.value / right.value))
}

case class And(left: Expression, right: Expression) extends BinaryOp[SimpleBoolean, SimpleBoolean] {
  def perform(left: SimpleBoolean, right: SimpleBoolean): SimpleBoolean = SimpleBoolean(left.value && right.value)
}

case class Or(left: Expression, right: Expression) extends BinaryOp[SimpleBoolean, SimpleBoolean] {
  def perform(left: SimpleBoolean, right: SimpleBoolean): SimpleBoolean = SimpleBoolean(left.value || right.value)
}

case class LessThan(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleBoolean] {
  def perform(left: SimpleNumber, right: SimpleNumber): SimpleBoolean = SimpleBoolean(left.value < right.value)
}

case class GreaterThan(left: Expression, right: Expression) extends BinaryOp[SimpleNumber, SimpleBoolean] {
  override def perform(left: SimpleNumber, right: SimpleNumber): SimpleBoolean = SimpleBoolean(left.value > right.value)
}

case class Variable(name: String) extends Expression {
  override def evaluate(environment: Environment): ValueExpression = environment(name)
}

// --- STATEMENTS
case class DoNothing() extends Statement {
  override def evaluate(environment: Environment): Environment = environment
}

case class Assign(name: String, expr: Expression) extends Statement {
  override def evaluate(environment: Environment): Environment = environment + (name -> expr.evaluate(environment))
}

case class If(cond: Expression, consequence: Statement, alternative: Statement = DoNothing()) extends Statement {
  override def evaluate(environment: Environment): Environment = {
    cond.evaluate(environment) match {
      case SimpleBoolean(true) => consequence.evaluate(environment)
      case SimpleBoolean(false) => alternative.evaluate(environment)
    }
  }
}

case class While(cond: Expression, body: Statement) extends Statement {
  override def evaluate(environment: Environment): Environment = {
    cond.evaluate(environment) match {
      case SimpleBoolean(true) => evaluate(body.evaluate(environment))
      case SimpleBoolean(false) => environment
    }

  }
}

case class Sequence(first: Statement, second: Statement) extends Statement {
  override def evaluate(environment: Environment): Environment = second.evaluate(first.evaluate(environment))
}

// --- TRAITS
trait Expression {
  def evaluate(environment: Environment): ValueExpression
}

trait ValueExpression extends Expression {
  override def evaluate(environment: Environment): ValueExpression = this
}

trait BinaryOp[A <: ValueExpression, B <: ValueExpression] extends Expression {
  val left: Expression
  val right: Expression

  def perform(left: A, right: A): B

  override def evaluate(environment: Environment): B = {
    val lEval = left.evaluate(environment)
    val rEval = right.evaluate(environment)

    // we have to do this fugliness to allow expressions to be of unknown type at compile time
    // but still checked at runtime (e.g. we'll get a runtime exception if we try to do `1 + false`)
    lEval match {
      case l: A =>
        rEval match {
          case r: A => perform(l, r)
        }
    }
  }
}

trait Statement {
  def evaluate(environment: Environment): Environment
}

object Main {
  def main(args: Array[String]): Unit = {
    println(Add(SimpleNumber(1), SimpleNumber(5)).evaluate(emptyEnvironment))
    println(Multiply(SimpleNumber(2), SimpleNumber(5)).evaluate(emptyEnvironment))
    println(And(SimpleBoolean(false), SimpleBoolean(true)).evaluate(emptyEnvironment))
    println(And(SimpleBoolean(true), SimpleBoolean(true)).evaluate(emptyEnvironment))
    println(LessThan(SimpleNumber(2), SimpleNumber(5)).evaluate(emptyEnvironment))
    println(GreaterThan(Add(SimpleNumber(2), SimpleNumber(5)), Multiply(SimpleNumber(5), SimpleNumber(2))).evaluate(emptyEnvironment))
    println(Divide(SimpleNumber(3), SimpleNumber(2)).evaluate(emptyEnvironment))

    println(Sequence(Assign("a", SimpleNumber(0)), While(LessThan(Variable("a"), SimpleNumber(5)), Assign("a", Add(Variable("a"), SimpleNumber(1))))).evaluate(emptyEnvironment))
  }
}