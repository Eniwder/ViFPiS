import javafx.application.Application

import com.sun.javafx.tk.Toolkit

import scala.collection.mutable
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control._
import scalafx.scene.effect.{BoxBlur, DropShadow, Effect}
import scalafx.scene.input.{MouseEvent, ScrollEvent}
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage


object View {

  //  n倍速
  private var speed = 1

  type Command = String
  private var code: List[String] = _

  private val execQueue = new mutable.Queue[Command]
  private val historyQueue = new mutable.Queue[Command]
  val callStack = new mutable.Stack[CallStackItem]
  private val history = new mutable.Stack[History]
  private val historyIndexMap = new mutable.HashMap[Int, Int]()
  private var historyMaxIndex = 0

  private val CMItem = CollectionMethodItem()
  private val CMEnterIndexes = new mutable.Stack[Int]
  private var maxCMStepResult = 0
  val MethodEnter = "MEnter"
  val MethodExit = "MExit"
  val CollectionMethodEnter = "CMEnter"
  val CollectionMethodExit = "CMExit"
  val EndOfData = "End"
  val Sep = "<@>"
  // TODO ほんとはJSON形式などにすべきか？
  val KeepLine = -1

  private var hPane: ScrollPane = _
  private var ltArea: TextArea = _
  private var ctArea: TextArea = _
  private var autoTB: ToggleButton = _
  private var stepBtn: Button = _
  private var cmTB: ToggleButton = _
  private var ptnTB: ToggleButton = _


  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////
  // --- API start ///////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////

  def loadSourceCode(sourceCode: List[String]): Unit = {
    code = sourceCode
  }

  def push(next: String, line: Int) {
    execQueue += makeMsg(next, line)
  }

  def pushOneBefore(msg: String, line: Int) {
    val heads = execQueue.dropRight(1)
    val last = execQueue.last
    execQueue.clear()
    execQueue ++= heads
    val line2 = if (line == KeepLine) last.split(Sep).last.toInt else line // CMEnterの場合失敗する
    push(msg, line2)
    execQueue += last
  }

  def addCallStack(method: String = "", line: Int) {
    execQueue += makeMsg(MethodEnter, method, line)
  }

  // Unitには未対応
  def popCallStack(returnValue: String, line: Int) {
    execQueue += makeMsg(returnValue, line)
    execQueue += makeMsg(MethodExit, line)
  }

  def end(): Unit = {
    execQueue += EndOfData
    // 今までの内容を履歴キューにコピーしておく
    execQueue.foreach(historyQueue += _)
  }

  def enterCollectionMethod(list: String, funcName: String, lambda: Lambda, funcVar: String = " ", line: Int): Unit = {
    execQueue += makeMsg(CollectionMethodEnter, list, funcName, lambda.arg, lambda.exp, funcVar, line)
    CMEnterIndexes.push(execQueue.size)
  }

  def pushCMResult(sr: String) {
    maxCMStepResult = maxCMStepResult max sr.length
    execQueue += makeMsg(sr, KeepLine)
  }

  def exitCollectionMethod(result: String): Unit = {
    execQueue += CollectionMethodExit
    // 演算結果が先に欲しいので、直近の演算開始のところにぶち込む、現状履歴管理が楽
    val n = CMEnterIndexes.pop()
    val tmp = execQueue.take(n - 1)
    val tmp2 = execQueue.drop(n - 1)
    val addResultText = makeMsg(tmp2.dequeue(), result, maxCMStepResult)
    execQueue.clear()
    execQueue ++= tmp += addResultText ++= tmp2
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////
  // API end --- /////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////

  private def makeMsg(ops: Any*): String = ops.mkString(Sep)

  private def getExecCommand(): Command ={
    val cmd = execQueue.dequeue()
    MyLogger.log(s"VIEW:Auto:${autoTB.selected.value}:Dequeue:$cmd")
    cmd
  }

  private def addHistory(item: CallStackItem, line: Int, display: Boolean = true): Unit = {
    if (nowIndex > historyMaxIndex) {
      history.push(History(item, nowIndex, copyCallStack(callStack), line, display))
      historyIndexMap.put(nowIndex, history.size)
      View.hPane.vvalue = 1
    }
  }

  private def copyCallStack(cs: Seq[CallStackItem]): List[CallStackItem] = {
    def loop(stack: List[CallStackItem]): List[CallStackItem] = stack match {
      case h :: t => CallStackItem.dupe(h) :: loop(t)
      case Nil => Nil
    }
    loop(cs.toList)
  }

  private def nowIndex: Int = historyQueue.size - execQueue.size

  // 表示すべき履歴を返す
  private def showHistory: List[History] = {
    val dequeuePlanN =
      if (CallStackItem.state == Expanding ||
        CallStackItem.state == Contracting ||
        CallStackItem.state == SubEnd
      ) {
        1
      } else {
        0
      }
    // おまじない…
    val opt = if (CallStackItem.state == CollectionMethodStep || CallStackItem.state == CollectionMethodStart) 1 else 0

    val ni = nowIndex + dequeuePlanN
    val ret = (
      if (ni > historyMaxIndex) View.history
      else {
        val n: Int = historyIndexMap.getOrElse(ni, historyIndexMap.filter(kv => kv._1 < ni).maxBy(kv => kv._1)._2)
        View.history.drop(View.history.size - n + 1 - opt)
      }).toList
    if (ptnTB.selected.value) {
      ret.filter(_.display)
    } else {
      ret.filter(_.display).filter(!_.text.contains("case"))
    }
  }

  addCallStack("dummy", KeepLine)

  //  push("sliceRecursive(start, end, list)")
  //  // ListMapTes
  //  push("x :: list map { x => x * 2 } ::: List(a,b,c,d,e)")
  //    enterCollectionMethod( """List("a","b","c","d")""", "map", Lambda("x", "x*2"))
  //    pushCMResult("\"aa\"")
  //    pushCMResult("\"bb\"")
  //    pushCMResult("\"cc\"")
  //    pushCMResult("\"dd\"")
  //    exitCollectionMethod( """List("aa","bb","cc","dd")""")
  //    // ListMapTes
  //    push("(start, end, ls) match {")
  //    push("(1, 3, List(\"a\", \"b\", \"c\", \"d\", \"e\")) match {")
  //    push("case (_, _, Nil) => Nil")
  //    push("case (_, e, _) if e <= 0")
  //    push("case (s, e, h :: tail) if s <= 0")
  //    push("case (s, e, h :: tail)")
  //    push("case (1, 3, \"a\" :: List(\"b\", \"c\", \"d\", \"e\"))")
  //    push("sliceRecursive(s - 1, e - 1, tail)")
  //    push("sliceRecursive(0, 2, List(\"b\", \"c\", \"d\", \"e\"))")
  //    //addCallStack()
  //    push("(start, end, ls) match {")
  //    push("(0, 2, List(\"b\", \"c\", \"d\", \"e\")) match {")
  //    push("case (_, _, Nil) => Nil")
  //    push("case (_, e, _) if e <= 0")
  //    push("case (s, e, h :: tail) if s <= 0")
  //    push("case (0, 2, \"b\" :: List(\"c\", \"d\", \"e\")) if 0 <= 0")
  //    push("h :: sliceRecursive(0, e - 1, tail)")
  //    push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))")
  //    addCallStack("sliceRecursive")
  //    push("(start, end, ls) match {")
  //    push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  //    push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))") //
  //    addCallStack("sliceRecursive") //
  //    push("(start, end, ls) match {")
  //    push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  //    push("case (_, _, Nil) => Nil")
  //    //  push("case (_, e, _) if e <= 0")
  //    //  push("case (s, e, h :: tail) if s <= 0")
  //    //  push("case (0, 1, \"c\" :: List(\"d\", \"e\")) if 0 <= 0")
  //    //  push("h :: sliceRecursive(0, e - 1, tail)")
  //    //  push("\"c\" :: sliceRecursive(0, 0, List(\"d\", \"e\"))")
  //    //  push("(start, end, ls) match {")
  //    //  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  //    //  push("case (_, _, Nil) => Nil")
  //    push("case (_, e, _) if e <= 0")
  //    push("case (_, 0, _) if 0 <= 0")
  //    push("Nil")
  //    popCallStack()
  //    push("List(\"c\")")
  //    popCallStack()
  //    push("List(\"b\", \"c\")")
  //  end()

  callStack.push(CallStackItem(callStack.size))

  def searchMethodEnd(text: String, braceCount: Int = 0, index: Int = 1): Int = text.toList match {
    case '(' :: t => searchMethodEnd(t.mkString(""), braceCount + 1, index + 1)
    case ')' :: t if braceCount == 1 => index
    case ')' :: t => searchMethodEnd(t.mkString(""), braceCount - 1, index + 1)
    case h :: t => searchMethodEnd(t.mkString(""), braceCount, index + 1)
    case Nil =>
      sys.error(s"引数 $text が不正なテキストです")
      0 // ここには来ないはず
  }


  def main(args: Array[String]) = Application.launch(classOf[View])

}

sealed trait AnimationState
case object MoveDown extends AnimationState
case object Emphasis extends AnimationState
case object Shadow extends AnimationState
case object Fixed extends AnimationState
case object Expanding extends AnimationState
case object Contracting extends AnimationState
case object SubExp extends AnimationState
case object SubEnd extends AnimationState
case object CollectionMethodStart extends AnimationState
case object CollectionMethodEnd extends AnimationState
case object CollectionMethodStep extends AnimationState
case object EndAnimation extends AnimationState

class View extends Application with myUtil {

  import View._

  // use mouse event
  var hmx, hmy: Int = 0
  var restoreHistoryF = false

  def start(stage: javafx.stage.Stage): Unit = {
    val (canvasW, canvasH) = (1200, 700)
    val canvas = new Canvas(canvasW, canvasH)
    val gc: GraphicsContext = canvas.getGraphicsContext2D
    gc.setFont(Font("Consolas", 18))

    val historyPane = new ScrollPane()
    val (hCanvasW, hCanvasH) = (300, canvasH)
    val hCanvas = new Canvas(hCanvasW, hCanvasH)
    val hgc = hCanvas.graphicsContext2D
    historyPane.content = hCanvas
    historyPane.layoutX = canvasW - hCanvasW
    historyPane.layoutY = 0
    historyPane.maxHeight = hCanvasH
    historyPane.maxWidth = 300
    hPane = historyPane
    hCanvas.handleEvent(MouseEvent.MouseMoved) {
      m: MouseEvent =>
        hmx = m.x.toInt
        hmy = m.y.toInt
    }
    historyPane.handleEvent(MouseEvent.MouseClicked) {
      m: MouseEvent => restoreHistoryF = true
    }
    historyPane.handleEvent(MouseEvent.MouseExited) {
      m: MouseEvent =>
        hmx = -1
        hmy = -1
    }

    val lineArea = new TextArea("  1:")
    lineArea.layoutX = 0
    lineArea.layoutY = 0
    lineArea.prefRowCount = 20
    lineArea.font = Font.font("Consolas", 12)
    lineArea.disable = true
    lineArea.style = "-fx-opacity: 1.0;"
    ltArea = lineArea
    def writeLine(ta: TextArea)(lines: Int): Unit = {
      ta.clear()
      ta.text = (1 to lines).map(idx => "%3s".format(s"$idx") + ":").mkString("\n")
    }
    val write2lineArea = writeLine(lineArea) _

    val codeArea = new TextArea(code.mkString("\n"))
    codeArea.layoutX = 40
    codeArea.layoutY = 0
    codeArea.prefColumnCount = 124
    codeArea.prefRowCount = 20
    codeArea.font = Font.font("Consolas", 12)
    codeArea.style = "-fx-highlight-fill: #D99; -fx-highlight-text-fill: black;"
    codeArea.editable = false
    ctArea = codeArea
    write2lineArea(codeArea.text.value.lines.size)
    codeArea.text.onChange {
      write2lineArea(codeArea.text.value.lines.size)
      lineArea.scrollTop = codeArea.scrollTop.value
    }
    codeArea.filterEvent(ScrollEvent.Any) {
      se: ScrollEvent => lineArea.scrollTop = codeArea.scrollTop.value
    }
    codeArea.filterEvent(MouseEvent.Any) {
      se: MouseEvent => lineArea.scrollTop = codeArea.scrollTop.value
    }

    val autoToggleButton = new ToggleButton("Auto")
    autoTB = autoToggleButton
    autoToggleButton.layoutX = 40
    autoToggleButton.layoutY = 320 // TODO 暫定決め打ち
    autoToggleButton.style = "-fx-base: lightgreen"
    autoToggleButton.setOnAction {
      e: ActionEvent =>
        resetAutoToggleButtonColor()
    }
    def resetAutoToggleButtonColor(): Unit = {
      if (autoToggleButton.selected.value) {
        autoToggleButton.style = "-fx-base: mediumseagreen"
      } else {
        autoToggleButton.style = "-fx-base: lightgreen"
      }
    }

    val stepButton = new Button("Step")
    stepBtn = stepButton
    stepButton.layoutX = 40 + 60
    stepButton.layoutY = 320
    stepButton.style = "-fx-base: lightgreen"
    stepButton.setOnAction {
      e: ActionEvent =>
        stepButton.disable = true
        autoToggleButton.selected = false
        resetAutoToggleButtonColor()
    }


    val collectionMethodToggleButton = new ToggleButton("Collection Method")
    cmTB = collectionMethodToggleButton
    collectionMethodToggleButton.layoutX = 40 + 60 + 60
    collectionMethodToggleButton.layoutY = 320
    collectionMethodToggleButton.selected = true
    collectionMethodToggleButton.style = if (collectionMethodToggleButton.selected.value) "-fx-base: steelblue" else "-fx-base: lightblue"
    collectionMethodToggleButton.setOnAction {
      e: ActionEvent =>
        MyLogger.log(s"VIEW:Push CollectionMethod:${collectionMethodToggleButton.selected.value}")
        if (collectionMethodToggleButton.selected.value) {
          collectionMethodToggleButton.style = "-fx-base: steelblue"
        } else {
          collectionMethodToggleButton.style = "-fx-base: lightblue"
        }
    }

    val patternMatchToggleButton = new ToggleButton("Pattern Match")
    ptnTB = patternMatchToggleButton
    patternMatchToggleButton.layoutX = 40 + 60 + 60 + 140
    patternMatchToggleButton.layoutY = 320
    patternMatchToggleButton.selected = true
    patternMatchToggleButton.style = if (patternMatchToggleButton.selected.value) "-fx-base: steelblue" else "-fx-base: lightblue"
    patternMatchToggleButton.setOnAction {
      e: ActionEvent =>
        MyLogger.log(s"VIEW:Push PatternMatch:${patternMatchToggleButton.selected.value}")
        if (patternMatchToggleButton.selected.value) {
          patternMatchToggleButton.style = "-fx-base: steelblue"
        } else {
          patternMatchToggleButton.style = "-fx-base: lightblue"
        }
    }

    val speedCBLabel = new Label("Speed:")
    speedCBLabel.layoutX = 40 + 60 + 60 + 140 + 120
    speedCBLabel.layoutY = 320 + 4
    // おそらく240の約数じゃないと、コレクションメソッドのprocessの値がバグる、また、雰囲気60以上は危険
    val speedCB = new ComboBox[Int](Seq(1, 2, 3, 4, 5, 10, 30, 60))
    speedCB.layoutX = 40 + 60 + 60 + 140 + 120 + 50
    speedCB.layoutY = 320
    speedCB.value = 1
    speedCB.setOnAction {
      e: ActionEvent =>
        MyLogger.log(s"VIEW:changeSpeed:${speedCB.value.value}")
        speed = speedCB.value.value
    }

    new Stage(stage) {
      title = "ViFPiS"
      scene = new Scene() {
        content += canvas
        content += historyPane
        content += lineArea
        content += codeArea

        content += autoToggleButton
        content += stepButton
        content += collectionMethodToggleButton
        content += patternMatchToggleButton
        content += speedCBLabel
        content += speedCB
      }
    }.show

    mainLoop.keyFrames = KeyFrame(16 ms, "ViFPiS", (e: ActionEvent) => runFrame(gc, hgc))
    mainLoop.play
  }

  private def isAuto = autoTB.selected.value
  private def isStepReady = stepBtn.disable.value
  private def isExecReady = isAuto || isStepReady
  private def stepReady() {
    stepBtn.disable = false
  }


  var frameCount = 0
  def runFrame(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    execute(gc, hgc)
    printAllHistory()(hgc)
    if (CallStackItem.state == Fixed) return
    //    if(callStack.top.width > gc.canvas.width.toInt){
    //      gc.setFont(Font("Consolas", gc.getFont.getSize-2))
    //    }
    draw(gc, hgc)
    printSourceCode()
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // --- view start ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  var count = 0
  var countMax = 0
  var lineTmp = 0
  var highlight = 0
  val CMdistance = 50

  private def draw(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    implicit val g = gc
    clearCanvas()
    val now = callStack.top

    if ((CallStackItem.state == CollectionMethodStart
      || CallStackItem.state == CollectionMethodStep
      || CallStackItem.state == CollectionMethodEnd)
      && !cmTB.selected.value) {
      printCallStack()
      printLastTwoHistory()
      pulseLine(now.text, now.x, now.y, now.width, now.height, now.padding, count, bc = Color.YellowGreen)
      return
    }

    if (CallStackItem.subStep && !ptnTB.selected.value) {
      printCallStack()
      printLastTwoHistory()
      pulseLine(CallStackItem.mainText, now.offsetX, now.offsetY, now.width, now.height, now.padding, count, bc = Color.YellowGreen)
      return
    }


    if (CallStackItem.state != MoveDown &&
      CallStackItem.state != CollectionMethodStep &&
      CallStackItem.state != CollectionMethodEnd) {

      if (CallStackItem.state == CollectionMethodStart && count < (countMax / 2)) {
        gc.setGlobalAlpha(1 - ((count + 1) * 2.0) / countMax)
        printLastTwoHistory()
        gc.setGlobalAlpha(1)
      } else if (CallStackItem.state != CollectionMethodStart) {
        printLastTwoHistory()
      }

    }

    CallStackItem.state match {
      case Emphasis =>
        // サイズが大きくなるなら拡大する
        val diffW = if (textSize(now.nextText)._1 > now.width) {
          val nextWidth = textSize(now.nextText)._1
          val diff = nextWidth - now.width
          (diff / 60.0 * count).toInt
        } else {
          0
        }
        callStack.foreach { item =>
          item.updateSize(item.width + diffW, item.height)
        }
        printCallStack()

        pulseLine(now.text, now.x, now.y, now.width, now.height, now.padding, count)


      case Shadow =>
        val shadowDepth = Math.max(0xF0 - count, 0xA0)
        val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
        printCallStack()
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding, eff1 = dsE)

      case MoveDown =>
        val shadowDepth = Math.min(0xA0 + count / 1.5, 0xF0).toInt
        val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
        printCallStack()
        textInBox(now.nextText, now.x, now.y, now.width, now.height, now.padding)
        val gray1 = (0xF0 - (count / 120.0) * 0x2F).toInt
        textInBox(now.text, now.x, sinMove(now.y, count, 120, 120), now.width, now.height, now.padding, eff1 = dsE, boxColor = Color.rgb(gray1, gray1, gray1)) // 置き換え元

        showHistory match {
          case h1 :: h2 :: tail =>
            val gray2 = (0xA0 - (count / 120.0) * 0x20).toInt
            textInBox(h1.text, h1.x, sinMove(h1.y + 60 * 2, count, 120, 120), h1.w, h1.h, now.padding, boxColor = Color.rgb(gray2, gray2, gray2))
            gc.setGlobalAlpha(1.0 - count / 120.0)
            textInBox(h2.text, h2.x, sinMove(h2.y + 60 * 4, count, 120, 120), h2.w, h2.h, now.padding, boxColor = Color.rgb(0x80, 0x80, 0x80))
            gc.setGlobalAlpha(1)

          case h1 :: Nil =>
            val gray2 = (0xA0 - (count / 120.0) * 0x20).toInt
            textInBox(h1.text, h1.x, sinMove(h1.y + 60 * 2, count, 120, 120), h1.w, h1.h, now.padding, boxColor = Color.rgb(gray2, gray2, gray2))

          case _ =>
        }

      case Fixed =>

      case Expanding =>
        printCallStack()
        textInBox(callStack(1).text, callStack(1).offsetX, callStack(1).offsetY, callStack(1).width, callStack(1).height, callStack(1).padding)
        gc.setGlobalAlpha(count / 60.0)
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        gc.setGlobalAlpha(1)

      case Contracting =>
        printCallStack()
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        val c = (0xFF * (count / 60.0)).toInt
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding, bc = Color.rgb(c, c, c))
        gc.setGlobalAlpha(1)

      case SubExp =>
        printCallStack()
        gc.setGlobalAlpha(count / 60.0)
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        gc.setGlobalAlpha(1)

      case SubEnd =>
        printCallStack()
        val diffX = now.x - now.offsetX
        val diffY = now.y - now.offsetY
        textInBox(CallStackItem.mainText, now.offsetX, now.offsetY, now.width, now.height, now.padding)
        textInBox(now.text, (now.x - (diffX * (count / 60.0))).toInt, (now.y - (diffY * (count / 60.0))).toInt, now.width, now.height, now.padding)

      // コレクションメソッド部分を正規表現で抜き出し強調　そしてメソッドを適用していく場を作る
      case CollectionMethodStart =>
        printCallStack()
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        val wh = textSize(CMItem.exp)
        if (count < (countMax / 2)) {
          gc.setGlobalAlpha(count / (countMax / 2.0))
          textInBox(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setGlobalAlpha(1)
        } else {
          val count2 = count - countMax / 2
          textInBox(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setGlobalAlpha(count2 / (countMax / 2.0))
          printListAndLambda(now)
          gc.setGlobalAlpha(1)
        }

      case CollectionMethodStep =>
        printCallStack()
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        val wh = textSize(CMItem.exp)
        textInBox(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, boxColor = Color.CornflowerBlue)
        printList(CMItem.process, now.x, now.y + CMdistance * 3, maxN = CMItem.iterateCount)
        val trg = CMItem.list(CMItem.iterateCount)
        val twh = textSize(trg)
        val sumX = CMItem.list.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
        if (count < 60) {
          // Listの要素を1つ選択し、強調
          printListAndLambda(now)
          gc.setGlobalAlpha(count / 30.0)
          textInBox(trg, now.x + sumX, now.y + CMdistance, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setGlobalAlpha(1)
        } else if (count < 100) {
          // 選択した要素をラムダの引数へ動かす
          printListAndLambda(now)
          val count2 = count - 60
          textInBox(trg, sinMove(now.x + sumX, count2, 40, -sumX), sinMove(now.y + CMdistance, count2, 40, CMdistance), twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)

        } else if (count < 120) {
          // 動かしたらちょっと待つ
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
        } else if (count < 150) {
          // =>　を赤く点滅させる
          printListAndLambda(now, arrow = false)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setFill(Color.rgb(255, 0, 0))
          gc.fillText("=>", now.x + textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg))._1, now.y + CMdistance * 2)
        } else if (count < 210) {
          // 要素を実行した結果を生成
          val count5 = count - 150
          printListAndLambda(now, result = false)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + " %2s".format("=>"))._1
          if (count5 < 30) {
            val bwh = 10 * count5 / 30
            gc.setEffect(new BoxBlur(bwh, bwh, 3))
            gc.fillText(CMItem.lambda.exp, now.x + resultX, now.y + CMdistance * 2)
            gc.setEffect(null)
          } else {
            CMItem.lambda.exp match {
              case "filter" =>
                // TODO filterの表現をどうするか
                if (count5 == 30) {
                  CMItem.process += CMItem.actualResult(CMItem.iterateCount)
                }
              case _ =>
                val bwh = 9 - 10 * (count5 % 30) / 30
                gc.setEffect(new BoxBlur(bwh, bwh, 3))
                gc.fillText(CMItem.stepResult, now.x + resultX, now.y + CMdistance * 2)
                gc.setEffect(null)
                if (count5 == 30) {
                  CMItem.process += CMItem.stepResult
                }
            }
          }
        } else if (count < 230) {
          // 結果が出たらちょっと待つ
          printListAndLambda(now, result = false)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + " %2s".format("=>"))._1
          gc.fillText(CMItem.stepResult, now.x + resultX, now.y + CMdistance * 2)
        } else if (count < 260) {
          // 結果に枠と影をつける
          val count6 = count - 230
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val shadowDepth = Math.max(0xF0 - count6, 0xA0)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + " %2s".format("=>"))._1
          val stepResultWH = textSize(CMItem.stepResult)
          val bc = 255 - count6 * 255 / 30
          textInBox(CMItem.stepResult, now.x + resultX, now.y + CMdistance * 2, stepResultWH._1, stepResultWH._2, now.padding, eff1 = dsE, bc = Color.rgb(bc, bc, bc))
        } else if (count < 300) {
          // 結果を新しいListの場所へ動かす
          val count7 = count - 260
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + CMdistance * 2, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + " %2s".format("=>"))._1
          val sumX = CMItem.process.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
          val stepResultWH = textSize(CMItem.stepResult)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(0xA0, 0xA0, 0xA0))
          textInBox(CMItem.stepResult, sinMove(now.x + resultX, count7, 40, -resultX + sumX), sinMove(now.y + CMdistance * 2, count7, 40, CMdistance), stepResultWH._1, stepResultWH._2, eff1 = dsE)
        } else {
          // 結果を置いて、他の部分を戻して少し待つ
          printListAndLambda(now)
          val stepResultWH = textSize(CMItem.stepResult)
          val sumX = CMItem.process.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
          textInBox(CMItem.stepResult, now.x + sumX, now.y + CMdistance * 3, stepResultWH._1, stepResultWH._2)
        }

      case CollectionMethodEnd =>
        printCallStack()
        if (count < 90) {
          textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
          val wh = textSize(CMItem.exp)
          textInBox(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, boxColor = Color.CornflowerBlue)
          printListAndLambda(now)
          // メソッド適用後後処理のあるものをここで行う
          CMItem.name match {
            case "flatMap" =>
              if (count < 30) {
                val bwh = 10 * count / 30
                printList(CMItem.process, now.x, now.y + CMdistance * 3, eff = new BoxBlur(bwh, bwh, 3))
                gc.setEffect(null)
              } else if (count < 60) {
                val bwh = 9 - 10 * (count % 30) / 30
                printList(CMItem.actualResult, now.x, now.y + CMdistance * 3, eff = new BoxBlur(bwh, bwh, 3))
                gc.setEffect(null)
              } else {
                printList(CMItem.actualResult, now.x, now.y + CMdistance * 3)
              }
            case _ =>
              printList(CMItem.process, now.x, now.y + CMdistance * 3)
              count = 90
          }
        } else if (count < 150) {
          val count2 = count - 90
          printList(CMItem.actualResult, now.x, now.y + CMdistance * 3)
          val diffW = if (textSize(now.nextText)._1 > now.width) {
            val nextWidth = textSize(now.nextText)._1
            val diff = nextWidth - now.width
            (diff / 60.0 * count2).toInt
          } else {
            0
          }
          callStack.foreach { item => item.updateSize(item.width + diffW, item.height) }
          pulseLine(now.text, now.x, now.y, now.width, now.height, now.padding, count)
          val wh = textSize(CMItem.exp)
          pulseLine(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, count, bc = Color.CornflowerBlue)
        } else {
          val count3 = count - 150
          val shadowDepth = Math.max(0xF0 - count3, 0xA0)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
          textInBox(now.text, now.x, now.y, now.width, now.height, now.padding, eff1 = dsE)

          gc.setGlobalAlpha(1 - count3 / 60.0)
          printList(CMItem.actualResult, now.x, now.y + CMdistance * 3)
          gc.setGlobalAlpha(count3 / 60.0)
          printLastTwoHistory()
          gc.setGlobalAlpha(1)
        }

      case EndAnimation =>
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)

    }

    if (CallStackItem.subStep && CallStackItem.state != SubEnd) {
      textInBox(CallStackItem.mainText, now.offsetX, now.offsetY, now.width, now.height, now.padding)
    }

  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // view end --- /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // --- control start ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def execute(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    frameCount += 1
    count += speed
    implicit val g = gc

    //    println(callStack.mkString(" ") + "\n+" +
    //      (callStack.top.text, callStack.top.nextText) + "\n" +
    //      (execQueue.head, CallStackItem.state) + "\n")

    val now = callStack.top
    CallStackItem.state match {
      case Fixed =>
        if (!isExecReady) {
          count -= speed
          return
        }
        val command = getExecCommand()
        now.update()
        command.split(Sep) match {
          case Array(MethodEnter, msg, line) =>
            if (callStack.size == 1) {
              // ↑aete
              now.update()
              callStack.push(CallStackItem(callStack.size))
              val next = getExecCommand().split(Sep)
              callStack.top.updateNext(next.head)
              //              callStack.top.update()
              highlight = next.last.toInt
              //              val next = getExecCommand().split(Sep)
              //              callStack.top.updateNext(next.head)
              //              highlight = next.last.toInt
              //              CallStackItem.state = Emphasis
              count = 0
            } else {
              callStack.push(CallStackItem(callStack.size, offsetX = calcOffsetX(now, msg)))
              callStack.top.updateNext(extractMethodText(now, msg))
              callStack.top.update()
              highlight = line.toInt
              CallStackItem.state = Expanding
            }

          case Array(MethodExit, line) =>
            now.updateNext(MethodExit)
            highlight = line.toInt
            CallStackItem.state = if (CallStackItem.subStep) SubEnd else Contracting

          case Array(CollectionMethodEnter, list, funcName, lambdaArg, lambdaExp, funcVar, line, result, maxStepResult) =>
            highlight = line.toInt
            CMItem.listString = list
            CMItem.name = funcName
            CMItem.lambda = Lambda(lambdaArg, lambdaExp)
            CMItem.variable = funcVar
            CMItem.iterateCount = 0
            CMItem.result = result
            CMItem.maxStepResultLen = maxStepResult.toInt
            CMItem.process.clear()
            // 1 ???? x map { y => y * 2 } ????
            // 2 ???? x map(y => y * 2) ????
            // 3 ???? x.map(y => y * 2) ????
            // 4 ???? List(...) map { y => y * 2 } ????
            // 5 ???? List(...) map(y => y * 2) ????
            // 6 ???? List(...).map(y => y * 2) ???????
            val CMRegex1 = (""".*?(\w+ """ + CMItem.name + """ *\{.+?\}).*""").r
            val CMRegex2 = (""".*?(\w+ """ + CMItem.name + """\(.+?\)).*""").r
            val CMRegex3 = (""".*?(\w+\.""" + CMItem.name + """\(.+?\)).*""").r
            val CMRegex4 = (""".*?(List.+? """ + CMItem.name + """ *\{.+?\}).*""").r
            val CMRegex5 = (""".*?(List.+? """ + CMItem.name + """\(.+?\)).*""").r
            val CMRegex6 = (""".*?(List.+?\.""" + CMItem.name + """\(.+?\)).*""").r
            CMItem.exp = now.text match {
              case CMRegex1(e) => e
              case CMRegex2(e) => e
              case CMRegex3(e) => e
              case CMRegex4(e) => e
              case CMRegex5(e) => e
              case CMRegex6(e) => e
            }
            CallStackItem.state = CollectionMethodStart

          case Array(EndOfData) =>
            View.addHistory(now, 0, display = false)
            CallStackItem.state = EndAnimation

          case Array(msg, line) => // 単純に置き換え
            //    callStack.top.update()
            now.updateNext(msg)
            highlight = if (line.toInt == KeepLine) highlight else line.toInt
            if (now.nextText.startsWith("case ")) {
              if (!CallStackItem.subStep) {
                CallStackItem.mainText = now.text
                View.addHistory(now, line.toInt, display = false)
                now.update()
                val next = getExecCommand().split(Sep)
                now.updateNext(next.head)
                lineTmp = next.last.toInt
                CallStackItem.state = SubExp
              } else {
                CallStackItem.state = Emphasis
              }
              CallStackItem.subStep = true
            } else if (CallStackItem.subStep && !now.text.startsWith("case ")) {
              CallStackItem.state = SubEnd
            } else {
              CallStackItem.state = Emphasis
            }
            count = 0


          case x =>
            println("match error")
            println(x.mkString(","))
            throw new Exception

        }

      case Emphasis =>
        countMax = 60
        if (count > countMax) {
          count = 0
          updateSize(now.nextText, now.depth)
          CallStackItem.state = Shadow
        }
        if (CallStackItem.subStep && !ptnTB.selected.value) {
          countMax = -1
          CallStackItem.state = Fixed
          addHistory(now, highlight)
        }


      case Shadow =>
        countMax = 60
        if (count > countMax) {
          count = 0
          CallStackItem.state = MoveDown
        }

      case MoveDown =>
        countMax = 120
        if (count > countMax) {
          count = 0
          View.addHistory(now, highlight)
          CallStackItem.state = Fixed
          stepReady()
        }

      case Expanding =>
        countMax = 60
        if (count > countMax) {
          count = 0
          val next = getExecCommand().split(Sep)
          callStack.top.updateNext(next.head)
          highlight = next.last.toInt
          CallStackItem.state = Emphasis
        }

      case Contracting =>
        countMax = 60
        if (count > countMax) {
          count = 0
          callStack.pop()
          val caller = callStack.top
          val (s, e) = caller.methodCallIndex
          val h = caller.text.take(s)
          val t = caller.text.drop(e)
          val returnValueText = h + s"%-${now.maxIndex}s".format(now.text) + t
          caller.updateNext(returnValueText)
          caller.update()
          val next = getExecCommand().split(Sep)
          caller.updateNext(next.head)

          if (caller.nextText == EndOfData) {
            CallStackItem.state = EndAnimation
          } else {
            CallStackItem.state = Emphasis
            highlight = next.last.toInt
          }
        }

      case SubExp =>
        countMax = 60
        if (!ptnTB.selected.value) {
          countMax = -1
        }
        if (count > countMax) {
          count = 0
          highlight = if (lineTmp == KeepLine) highlight else lineTmp
          CallStackItem.state = Emphasis
        }

      case SubEnd =>
        countMax = 60
        if (!ptnTB.selected.value) {
          countMax = 120
        }
        if (count > countMax) {
          count = 0
          CallStackItem.subStep = false
          CallStackItem.state = if (now.nextText == MethodExit) Contracting else Emphasis
        }

      case CollectionMethodStart =>
        countMax = 90
        if (!cmTB.selected.value) {
          countMax = 60
        }
        if (count > countMax) {
          count = 0
          getExecCommand() match {
            case CollectionMethodExit =>
              CallStackItem.state = CollectionMethodEnd
              now.updateNext(now.text.replace(CMItem.exp, CMItem.result))
            case x =>
              CMItem.stepResult = x.split(Sep).head
              CallStackItem.state = CollectionMethodStep
          }
          stepReady()
        }

      case CollectionMethodStep =>
        if (!isExecReady && cmTB.selected.value) {
          count -= speed
          return
        }
        countMax = 330
        if (!cmTB.selected.value) {
          countMax = -1
        }
        if (count > countMax) {
          count = 0
          CMItem.iterateCount += 1
          getExecCommand() match {
            case CollectionMethodExit =>
              now.updateNext(now.text.replace(CMItem.exp, CMItem.result))
              CallStackItem.state = CollectionMethodEnd
            case x =>
              CMItem.stepResult = x.split(Sep).head
              CallStackItem.state = CollectionMethodStep
          }
          stepReady()
        }

      case CollectionMethodEnd =>
        countMax = 210
        if (!cmTB.selected.value) {
          countMax = -1
        }
        if (count > countMax) {
          count = 0
          CallStackItem.state = MoveDown
        }

      case EndAnimation =>
        countMax = -1

      case x =>
        println("match error")
        println(x)
        throw new Exception
    }

    updateSize(callStack.top.text, callStack.top.depth)

  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // control end --- //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def updateSize(text: String, depth: Int)(implicit gc: GraphicsContext): Unit = {
    callStack.foreach { item =>
      val (w, h) = textSize(text)
      val diffX = (depth - item.depth) * CallStackItem.DefaultX
      item.updateSize(w + diffX, h)
    }
  }

  private def calcOffsetX(item: CallStackItem, trgText: String)(implicit gc: GraphicsContext): Int = {
    val text = item.text.take(item.text.indexOf(trgText)) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰むかも？
    item.offsetX + textSize(text)._1
  }

  private def extractMethodText(item: CallStackItem, callMethod: String)(implicit gc: GraphicsContext): String = {
    val startIndex = item.text.indexOf(callMethod)
    val methodStartText = item.text.drop(startIndex)
    // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰むかも？
    val endIndex = searchMethodEnd(methodStartText)
    item.setMethodCallIndex(startIndex, startIndex + endIndex)
    methodStartText.take(endIndex)
  }


  // textの左上がxy、paddingに応じて四角形が大きくなる
  private def textInBox(text: String, x: Int, y: Int, w: Int, h: Int, padding: Int = 8, boxColor: Color = Color.White,
                        eff1: Effect = null, eff2: Effect = null, eff3: Effect = null, lw: Int = 2, bc: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    rectWithBorder(x - padding / 4, y - h + des - padding / 4 + lw / 2, w + padding / 2, h + padding / 2, boxColor, eff1, eff2, lw, bc)
    gc.setEffect(eff3)
    gc.fillText(text, x, y)
    gc.setEffect(null)
  }

  private def rectWithBorder(x: Int, y: Int, w: Int, h: Int, fillColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null, lw: Int = 2, borderColor: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    gc.setEffect(eff1)
    gc.setFill(fillColor)
    gc.fillRect(x, y, w, h)
    gc.setEffect(eff2)
    gc.setStroke(borderColor)

    gc.lineWidth = lw
    gc.setFill(Color.Black)
    gc.strokeRect(x, y, w, h)
  }

  private def fontMetrics(implicit gc: GraphicsContext): com.sun.javafx.tk.FontMetrics = Toolkit.getToolkit.getFontLoader.getFontMetrics(gc.font)

  private def textSize(text: String)(implicit gc: GraphicsContext): (Int, Int) = {
    val metrics = fontMetrics
    (metrics.computeStringWidth(text) toInt, (metrics.getLineHeight + metrics.getMaxDescent).toInt)
  }

  private def textSizeWithPadding(text: String, padding: Int)(implicit gc: GraphicsContext): (Int, Int) = textSize(text).map(_ + padding)

  private def clearCanvas()(implicit gc: GraphicsContext) {
    gc.setFill(Color.White)
    gc.fillRect(0, 0, gc.canvas.width.toInt, gc.canvas.height.toInt)
  }

  private def sinMove(start: Int, count: Int, limit: Int, distance: Int): Int = {
    start + ((Math.sin(-Math.PI / 2 + Math.PI / limit * count) + 1) / 2 * distance).toInt
  }


  private def pulseLine(text: String, x: Int, y: Int, w: Int = 0, h: Int = 0, padding: Int = 8, frame: Int, fillColor: Color = Color.White, bc: Color = Color.Red, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    val lww = if (frame % 30 < 15) (frame % 30) / 4 + 1 else 4 - (frame % 15) / 4
    rectWithBorder(x - padding / 4, y - h + des - padding / 4 + lww / 2, w + padding / 2, h + padding / 2, lw = lww, borderColor = bc, fillColor = fillColor, eff1 = eff1, eff2 = eff2)
    gc.fillText(text, x, y)
  }

  // 直近2つを近くに表示
  private def printLastTwoHistory()(implicit gc: GraphicsContext): Unit = {
    showHistory match {
      case h1 :: h2 :: tail =>
        textInBox(h1.text, h1.x, h1.y + 60 * 2, h1.w, h1.h, padding = h1.padding, boxColor = Color.rgb(0xC1, 0xC1, 0xC1))
        textInBox(h2.text, h2.x, h2.y + 60 * 4, h2.w, h2.h, padding = h2.padding, boxColor = Color.rgb(0x80, 0x80, 0x80))

      case h1 :: Nil =>
        textInBox(h1.text, h1.x, h1.y + 60 * 2, h1.w, h1.h, padding = h1.padding, boxColor = Color.rgb(0xC1, 0xC1, 0xC1))

      case h1 :: tail =>
        textInBox(h1.text, h1.x, h1.y + 60 * 2, h1.w, h1.h, padding = h1.padding, boxColor = Color.rgb(0xC1, 0xC1, 0xC1))
        textInBox(tail.head.text, tail.head.x, 60 * 4, tail.head.w, tail.head.h, tail.head.padding, boxColor = Color.rgb(0x80, 0x80, 0x80))

      case _ => // do nothing
    }
  }

  private def printAllHistory()(implicit gc: GraphicsContext): Unit = {
    clearCanvas()
    gc.setFill(Color.Black)
    val canvas = gc.canvas
    val hist = if (ptnTB.selected.value) View.history.reverse.zipWithIndex else View.history.reverse.zipWithIndex.filter(x => !x._1.text.contains("case"))
    val height = 32
    var maxWidth = canvas.width.toInt
    canvas.height = (hist.size * height + height / 2 + height + 10) max canvas.height.toInt
    for ((h, index) <- hist.zipWithIndex) {
      val x = (h._1.x - CallStackItem.DefaultX) / 10 + canvas.getLayoutX.toInt + 5
      val y = (index + 1) * height + canvas.getLayoutY.toInt + 10
      val newHy = hmy + canvas.getLayoutY.toInt
      val withinRange = newHy - height / 3 < y && y < newHy + height / 1.5
      // よく分からんけど、3と1.5がちょうどいい
      val boxColor = if (withinRange) Color.SlateGray else Color.LightGray
      val borderColor = if (withinRange) Color.DimGray else Color.Black
      maxWidth = maxWidth max (x + h._1.w - canvas.getLayoutX.toInt)
      textInBox(h._1.text, x, y, h._1.w, height - 8, boxColor = boxColor, bc = borderColor)
      // 履歴がクリックされていたら、その場所を復元する
      // クリックイベントにしなかった理由は、ここで定義したheightによって位置が変わることと、withinRangeを利用するため
      if (withinRange && restoreHistoryF) {
        restoreHistory(h._2, h._1.qIndex)
      }
    }
    canvas.width = maxWidth
    restoreHistoryF = false
  }


  // 今までの呼び出し状況の描画、ただし最初に無駄なcallStackItemが1つあるので除去
  private def printCallStack()(implicit gc: GraphicsContext): Unit = {
    callStack.tail.reverse.tail.foreach(item => textInBox(item.text, item.offsetX, item.offsetY, item.width, item.height, item.padding))
  }

  private def restoreHistory(index: Int, qIndex: Int): Unit = {
    historyMaxIndex = historyMaxIndex max history.head.qIndex
    callStack.clear()
    callStack.pushAll(copyCallStack(history.reverse(index).state).reverse)
    highlight = history.reverse(index).line
    count = 0
    execQueue.clear()
    execQueue ++= historyQueue.drop(qIndex)

    val now = callStack.top
    MyLogger.log(s"VIEW:RestoreHistory:${now.text}")
    val nextIsEnd = (now.nextText == EndOfData) || now.nextText == ""
    val nextIsSubexp = now.nextText.startsWith("case ")
    CallStackItem.subStep = nextIsSubexp || now.text.startsWith("case ")
    CallStackItem.state =
      if (nextIsSubexp || now.text.startsWith("case ")) SubExp
      else if (nextIsEnd) EndAnimation
      else Emphasis
    if (nextIsSubexp) {
      CallStackItem.mainText = now.text
      now.update()
      now.updateNext(getExecCommand().split(Sep).head)
      lineTmp = highlight
    }
    if (now.text.startsWith("case ")) {
      CallStackItem.mainText = historyQueue.take(qIndex).reverse.map(_.split(Sep).head).find(_.contains("match")).get
    }
    stepReady()

  }

  private def printListAndLambda(now: CallStackItem, padding: Int = 8, distance: Int = CMdistance, arrow: Boolean = true, result: Boolean = true)(implicit gc: GraphicsContext): Unit = {
    printList(CMItem.list, now.x, now.y + distance, padding = padding)
    val arrowStr = if (arrow) "=>" else ""
    val resultStr = if (result) CMItem.lambda.exp else ""
    val resultWidth = CMItem.maxStepResultLen max CMItem.lambda.exp.length
    val lambda = s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s ".format(arrowStr) + s"%-${resultWidth}s".format(resultStr)
    val lwh = textSize(lambda)
    textInBox(lambda, now.x, now.y + distance * 2, lwh._1 + 10, lwh._2, padding)
  }

  private def printList(list: Seq[String], x: Int, y: Int, maxN: Int = -1, padding: Int = 8, eff: Effect = null)(implicit gc: GraphicsContext): Unit = {
    var sumX = 0
    val n = if (maxN == -1) list.length else maxN
    for (elem <- list.take(n)) {
      val wh = textSize(elem)
      textInBox(elem, x + sumX, y, wh._1, wh._2, padding, eff3 = eff)
      sumX += wh._1 + padding / 2
    }
  }

  private def printSourceCode(): Unit = {
    var counter = 0
    val metrics = Toolkit.getToolkit.getFontLoader.getFontMetrics(ctArea.font.value)
    val lHeight = metrics.getAscent + metrics.getDescent
    for ((line, index) <- ctArea.text.value.lines.zipWithIndex) {
      if (index == (highlight - 1)) {
        if (ctArea.scrollTop.toInt < ((index - 16) * lHeight).toInt || ((index + 16) * lHeight).toInt < ctArea.scrollTop.toInt) {
          ctArea.scrollTop = (index - 3) * lHeight
          ltArea.scrollTop = ctArea.scrollTop.value
        }
        ctArea.selectRange(counter, counter + line.length)
      }
      counter += line.length + 1
    }
  }


}