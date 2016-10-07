import scala.collection.mutable

trait myUtil {

  // 三項演算子っぽいやつ
  def eitherOneIfLeft[T](v: T, defaultValue: T)(p: T => Boolean): T = if (p(v)) v else defaultValue

  // 三項演算子っぽいやつ
  def ifMap[T](v: T)(f: T => T)(p: T => Boolean): T = if (p(v)) f(v) else v


  // タプルを入れ替える
  def flipTuple[A, B](ab: (A, B)): (B, A) = (ab._2, ab._1)

  // 制限範囲内の値を取得する
  def limitedRange(minX: Double, x: Double, maxX: Double) = x max minX min maxX


  // ----- implicitクラス -----
  // Bufferからオブジェクトを削除する
  implicit class MyBuf[T](val buf: mutable.Buffer[T]) {
    def remObj(obj: T) = buf.remove(buf.indexOf(obj))
  }

  //指定文字で区切る、その文字は含まない
  implicit class MyString[T](val str: String) {
    def splitChar(c: Char): (String, String) = {
      val (in, to) = str.splitAt(str.indexOf(c))
      (in, to.tail)
    }
  }

  implicit class MyTouple[A, B](val tp: (A, A)) {
    def map(f: A => B): (B, B) = (f(tp._1), f(tp._2))
  }
  // ----- implicitクラスここまで -----

  // ローンパターン // d.hatena.ne.jp/xuwei/20101216/1292520269
  def using[A <: {def close()}, B](resource: A)(func: A => B): Option[B] =
  try {
    Some(func(resource)) //成功したら、Someに包んで返す
  } catch {
    case e: Exception => e.printStackTrace()
      None //失敗したら、ログ吐いて、None返す
  } finally {
    if (resource != null) resource.close()
  }

}