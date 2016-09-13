/**
  * Created by slab on 2016/08/22.
  */
object DebugTarget {

  def main(args: Array[String]) {
    val start = 1
    val end = 3
    val list = List("a", "b", "c", "d", "e")

//    println(list map {x => x*2})

    val sliced = sliceRecursive(start, end, list)
    println(sliced)

  }


  def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = (start, end, ls) match {
    case (_, _, Nil) => Nil
    case (_, e, _) if e <= 0 => Nil
    case (s, e, h :: tail) if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
    case (s, e, h :: tail) => sliceRecursive(s - 1, e - 1, tail)
  }


}
