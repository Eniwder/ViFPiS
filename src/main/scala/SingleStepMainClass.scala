// NOTE: Make sure that the line marked as breakpoint is ACTUALLY on line 7

object SingleStepMainClass {
  def main(args: Array[String]): Unit = {
    def noop(s: String):Int = {println(s);1}
    while (true) {
      noop("f") // Breakpoint line is 7
      noop("s") // Step over should reach here
    }
  }
}