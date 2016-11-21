case class CallStackItem(depth: Int, offsetX: Int = CallStackItem.DefaultX, offsetY: Int = CallStackItem.DefaultY) {
  var width = 0
  var height = 0

  private var now = ""
  private var next = ""

  def padding: Int = {
    Math.abs(depth - ConnectTest.callStack.size) * CallStackItem.DefaultPadding
  }

  def updateNext(replace: String): Unit = {
    next = replace
  }

  def update(): String = {
    now = next
    next = ""
    now
  }

  def updateSize(wh: (Int, Int)): Unit = {
    width = width max wh._1
    height = height max wh._2
  }

  def text = now
  def nextText = next
}

object CallStackItem {
  var state: AnimationState = Fixed

  val DefaultPadding = 12
  val DefaultX = 100
  val DefaultY = 100

}