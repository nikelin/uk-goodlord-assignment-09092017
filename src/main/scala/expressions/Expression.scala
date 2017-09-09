package expressions

sealed trait Expression
object Expression {
  case class Literal(x: Int) extends Expression
  case class Mul(x: Expression, y: Expression) extends Expression
  case class Add(x: Expression, y: Expression) extends Expression
  case class Sub(x: Expression, y: Expression) extends Expression

  def printExpression(program: Expression): String =
    program match {
      case Expression.Literal(x) => x.toString
      case Expression.Mul(x, y) => "(" + printExpression(x) + " * " + printExpression(y) + ")"
      case Expression.Add(x, y) => "(" + printExpression(x) + " + " + printExpression(y) + ")"
      case Expression.Sub(x, y) => "(" + printExpression(x) + " - " + printExpression(y) + ")"
    }

  def evaluateExpression(program: Expression): Int =
    program match {
      case Expression.Literal(x) => x
      case Expression.Add(x, y) => evaluateExpression(x) + evaluateExpression(y)
      case Expression.Mul(x, y) => evaluateExpression(x) * evaluateExpression(y)
      case Expression.Sub(x, y) => evaluateExpression(x) - evaluateExpression(y)
    }
}
