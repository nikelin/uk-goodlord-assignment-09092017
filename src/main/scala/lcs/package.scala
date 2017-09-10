package object lcs {
  import scalaz.Memo

  implicit val ord: Ordering[String] = Ordering by {_.length}

  def findCommonSequenceStacksafe(left: String, right: String): String = {
    import scalaz.Free._
    import scalaz.Scalaz._

    def loop(a: List[Char], b: List[Char], common: List[Char]): Trampoline[List[Char]] =
      if (a.isEmpty || b.isEmpty) return_(common)
      else if (a.head == b.head) suspend(loop(a.tail, b.tail, a.head :: common))
      else {
        for {
          l <- suspend(loop(a.tail, b, common))
          r <- suspend(loop(a, b.tail, common))
          gt = if (l.length > r.length) l else r
        } yield gt
      }

    loop(left.reverse.toList, right.reverse.toList, List.empty).run.mkString
  }

  def findCommonSequenceMemo(left: String, right: String): String = {
    lazy val memoize: ((Int, Int)) => String = Memo.immutableHashMapMemo {
      case (0, _) => ""
      case (_, 0) => ""
      case (i, j) if left(i - 1) == right(j - 1) =>
        memoize((i - 1, j - 1)) + left(i - 1)
      case (i, j) => ord.max(memoize((i, j - 1)), memoize((i - 1, j)))
    }

    memoize((left.length, right.length))
  }
}
