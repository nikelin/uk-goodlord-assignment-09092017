import org.scalatest.{Matchers, WordSpec}

class EquationDeriverSpec extends WordSpec with Matchers {
  import expressions._
  import Expression._

  "Equation deriver" must {

    "produce a correct equation for a given solvable problem" in {
      val result = findSolution(List(2, 3, 5, 6), 42)
      result should not be empty
      result.get should be (Expression.Mul(Expression.Literal(6), Expression.Add(Expression.Literal(2), Expression.Literal(5))))
    }

  }

}
