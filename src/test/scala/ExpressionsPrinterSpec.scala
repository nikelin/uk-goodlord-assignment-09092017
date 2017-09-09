import org.scalatest.{Matchers, WordSpec}

class ExpressionsPrinterSpec extends WordSpec with Matchers {
  import expressions._
  import Expression._

  "Expression printer" must {

    "support each operation available" in {
      List(Expression.Mul -> "*", Expression.Add -> "+", Expression.Sub -> "-") foreach { case (op, symbol) =>
        printExpression(op(Expression.Literal(2), Expression.Literal(3))) should be (s"(2 $symbol 3)")
      }
    }

    "correctly print nested expression with groups on both sides" in {
      printExpression(
        Expression.Mul(
          Expression.Add(Expression.Literal(2), Expression.Literal(3)),
          Expression.Add(Expression.Literal(2), Expression.Literal(3))
        )
      ) should be("((2 + 3) * (2 + 3))")
    }

    "correctly print nested expression" in {
      printExpression(Expression.Mul(Expression.Literal(2), Expression.Add(Expression.Literal(2), Expression.Literal(3)))) should be("(2 * (2 + 3))")
      printExpression(Expression.Mul(Expression.Literal(2), Expression.Mul(Expression.Literal(2), Expression.Literal(3)))) should be("(2 * (2 * 3))")
    }

    "correctly print simple expressions" in {
      printExpression(Expression.Literal(2)) should be ("2")
      printExpression(Expression.Mul(Expression.Literal(2), Expression.Literal(2))) should be ("(2 * 2)")
    }
  }

}
