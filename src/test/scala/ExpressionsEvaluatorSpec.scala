import org.scalatest.{Matchers, WordSpec}

class ExpressionsEvaluatorSpec extends WordSpec with Matchers {
  import expressions._
  import Expression._

  "Expressions evaluator" must {

    "support each operation available" in {
      List(Expression.Mul -> ((_: Int) * (_: Int)), Expression.Add -> ((_: Int) + (_: Int)), Expression.Sub -> ((_: Int) - (_: Int))) foreach { case (op, fn) =>
        evaluateExpression(op(Expression.Literal(2), Expression.Literal(3))) should be (fn(2, 3))
      }
    }

    "correctly evaluate nested expression with groups on both sides" in {
      evaluateExpression(
        Expression.Mul(
          Expression.Add(Expression.Literal(2), Expression.Literal(3)),
          Expression.Add(Expression.Literal(2), Expression.Literal(3))
        )
      ) should be ((2 + 3) * (2 + 3))
    }

    "correctly evaluate nested expression" in {
      evaluateExpression(Expression.Mul(Expression.Literal(2), Expression.Add(Expression.Literal(2), Expression.Literal(3)))) should be (2 * (2 + 3))
      evaluateExpression(Expression.Mul(Expression.Literal(2), Expression.Mul(Expression.Literal(2), Expression.Literal(3)))) should be (2 * (2 * 3))
    }

    "correctly evaluate simple expressions" in {
      evaluateExpression(Expression.Literal(2)) should be (2)
      evaluateExpression(Expression.Mul(Expression.Literal(2), Expression.Literal(2))) should be (2 * 2)
    }
  }
}
