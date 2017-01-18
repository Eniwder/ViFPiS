import java.io.{File, FileOutputStream, OutputStreamWriter}
import scala.concurrent._
import ExecutionContext.Implicits.global

object MyLogger {
  val Path = s"./logs/${System.currentTimeMillis()}log.txt"
  val dir = new File("./logs")
  if (!dir.exists()) dir.mkdir()

  // 文字列を受け取ったらロギングを別のスレッドにまかせてすぐ帰る
  def log(str: String) {
    // 実際にログを取る関数、排他制御
    def prvLog(str: String): Any = synchronized {
      val writer = new OutputStreamWriter(new FileOutputStream(Path, true), "utf-8")
      writer.write(s"TIME:${System.currentTimeMillis()}:$str\r\n")
      writer.close
    }
    println(str)
    Future { prvLog(str) }
  }

}
