package object lcs {
  import scalaz.Memo

  def lcs(s1: String, s2: String): String = {
    def longest(a: String, b: String) =
      if ( a.length > b.length ) a
      else if ( a.length == b.length ) a
      else b

    lazy val memoize: ((Int, Int)) => String = Memo.weakHashMapMemo[(Int, Int), String] {
      case (0, _) => ""
      case (_, 0) => ""
      case (i, j) if s1(i - 1) == s2(j - 1) =>
        memoize((i - 1, j - 1)) + s1(i - 1)
      case (i, j) =>
        longest(memoize((i, j - 1)), memoize((i - 1, j)))
    }

    memoize((s1.length, s2.length))
  }
}
