case class CollectionMethodItem() {
  var name: String = _
  var variable: String = ""
  var iterateCount: Int = 0
  var exp: String = _
  var lambda: CollectionLambda = _
  var stepResult: String = _

  private var _result: String = _
  private var _maxElemLen: Int = 0
  private var _list: List[String] = List()
  private var _actualResult: List[String] = List()
  private var _maxResultLen: Int = 0
  private var _listString: String = _

  def listString_=(s: String) {
    _listString = s
    _list = CollectionMethodItem.stringList2List(s)
    _maxElemLen = list.map(_.length).max
  }

  def result_=(s: String): Unit = {
    _result = s
    _actualResult = CollectionMethodItem.stringList2List(s)
    _maxResultLen = actualResult.map(_.length).max
  }

  def listString = _listString
  def list = _list
  def maxElemLen = _maxElemLen
  def actualResult = _actualResult
  def result = _result
  def maxResultLen = _maxResultLen

}

object CollectionMethodItem {
  def stringList2List(s: String) = s.dropWhile(_ != '(').tail.dropRight(1).split(',').toList
}


// x map { y => y * 2 }
// arg : y
// exp : y * 2
case class CollectionLambda(arg: String, exp: String)
