case class CollectionMethodItem() {
  var name: String = _
  var variable: String = ""
  var iterateCount: Int = 0
  var exp: String = _
  var lambda: Lambda = _
  var stepResult: String = _
  var maxStepResultLen: Int = 0
  val process = scala.collection.mutable.Buffer.empty[String]

  private var _result: String = _
  private var _maxElemLen: Int = 0
  private var _list: List[String] = List()
  private var _actualResult: List[String] = List()

  private var _listString: String = _

  def listString_=(s: String) {
    _listString = s
    _list = CollectionMethodItem.stringList2List(s)
    _maxElemLen = list.map(_.length).max
  }

  def result_=(s: String): Unit = {
    _result = s
    _actualResult = CollectionMethodItem.stringList2List(s)
  }

  def listString = _listString
  def list = _list
  def maxElemLen = _maxElemLen
  def actualResult = _actualResult
  def result = _result

}

object CollectionMethodItem {
  def stringList2List(s: String) = s.dropWhile(_ != '(').tail.dropRight(1).split(',').map(_.trim).toList
}


// x map { y => y * 2 }
// arg : y
// exp : y * 2
case class Lambda(arg: String, exp: String)
