object Test {
  def main(args: Array[String]) {
    def pack[A](ls: List[A]): List[List[A]] = {
      if (ls.isEmpty)
        List(List())
      else {
        val (packed, next) = ls span { _ == ls.head }
        if (next == Nil)
          List(packed)
        else
          packed :: pack(next)
      }
    }
    pack(List("a", "b", "c", "c", "d", "d", "d"))
  }
}