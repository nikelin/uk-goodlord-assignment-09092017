package object lcs {
  import scalaz.Memo

  def findCommonSequence(left: String, right: String): String = {
    def max(first: String, second: String) =
      if ( first.length > second.length ) first
      else if ( first.length == second.length ) first
      else second

    lazy val memoize: ((Int, Int)) => String = Memo.immutableHashMapMemo {
      case (0, _) => ""
      case (_, 0) => ""
      case (i, j) if left(i - 1) == right(j - 1) =>
        memoize((i - 1, j - 1)) + left(i - 1)
      case (i, j) => max(memoize((i, j - 1)), memoize((i - 1, j)))
    }

    memoize((left.length, right.length))
  }
}
