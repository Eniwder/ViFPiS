case class History(item:CallStackItem,
  qIndex: Int, // この時点でexecQueueが何番目を指しているか
  state: List[CallStackItem],
  line:Int,
  display:Boolean = true) {

  val x = item.x
  val y = item.y
  val w = item.width
  val h = item.height
  val padding = item.padding
  val text = item.text
  val nextText = item.nextText

  override def toString:String = {
    s"History:$text\n" +
      s"\tx=$x y=$y w=$w h=$h qIndex=$qIndex\n" +
      s"${state.map(x => ("\ttext:" + x.text,"\tnext:"+x.nextText)).mkString("\n")}"
  }

}
