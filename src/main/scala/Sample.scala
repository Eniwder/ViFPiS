object Sample {

  def main(args: Array[String]) {
    val start = 1
    val end = 3
    val list = List("a", "b", "c", "d", "e")

    val f = (x: String) => x * 3

    list map f

    sliceRecursive(start, end, list)

    def tte(ff: String => List[String]): List[_] = {
      list flatMap ff
      ff("c")
    }

    def sss(s: String, n: Int): List[String] = List(s * n)

    val n = 4
    tte({ x => sss(x, n) })

  }

  // caseの後は改行必須…
  def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = (start, end, ls) match {
    case (_, _, Nil)                 =>
      Nil
    case (_, e, _) if e <= 0         =>
      Nil
    case (s, e, h :: tail) if s <= 0 =>
      h :: sliceRecursive(0, e - 1, tail)
    case (s, e, h :: tail)           =>
      sliceRecursive(s - 1, e - 1, tail)
  }

}
