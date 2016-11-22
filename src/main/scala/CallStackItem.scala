case class CallStackItem(depth: Int, offsetX: Int = CallStackItem.DefaultX, offsetY: Int = CallStackItem.DefaultY) {
  var width = 0
  var height = 0
  var maxIndex = 0

  private var callStartIndex = -1
  private var callEndIndex = -1

  private var now = ""
  private var next = ""

  def padding: Int = {
    Math.abs(depth - ConnectTest.callStack.size) * CallStackItem.DefaultPadding
  }

  def updateNext(replace: String): Unit = {
    next = replace
  }

  def update(): String = {
    if (maxIndex < next.length) {
      maxIndex = next.length
    }
    now = next
    next = ""
    now
  }

  def updateSize(wh: (Int, Int)): Unit = {
    width = width max wh._1
    height = height max wh._2
  }

  def setMethodCallIndex(s: Int, e: Int): Unit = {
    callStartIndex = s
    callEndIndex = e
  }

  def methodCallIndex: (Int, Int) = (callStartIndex, callEndIndex)

  def text = now
  def nextText = next
}

object CallStackItem {
  var state: AnimationState = Fixed

  val DefaultPadding = 12
  val DefaultX = 100
  val DefaultY = 100


  def dupe(from: CallStackItem): CallStackItem = {
    val ret = from.copy()
    ret.width = from.width
    ret.maxIndex = from.maxIndex
    ret.height = from.height
    ret.callStartIndex = from.callStartIndex
    ret.callEndIndex = from.callEndIndex
    ret.next = from.next
    ret.now = from.now
    ret
  }

}