import javafx.application.Application

import com.sun.javafx.tk.Toolkit

import scala.collection.mutable
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.ScrollPane
import scalafx.scene.effect.{DropShadow, Effect}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage


/**
  * Created by slab on 2016/09/14.
  */
object ConnectTest {
  type Command = String

  private val execQueue = new mutable.Queue[Command]
  private val historyQueue = new mutable.Queue[Command]
  val callStack = new mutable.Stack[CallStackItem]
  private val history = new mutable.Stack[History]
  private val historyIndexMap = new mutable.HashMap[Int, Int]()
  var historyMaxIndex = 0

  val MethodEnter = "MEnter"
  val MethodExit = "MExit"

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }

  // --- API start
  def push(next: String) {
    execQueue += next
  }

  def addCallStack(method: String = "") {
    execQueue += s"$MethodEnter<@>$method"
  }

  def popCallStack() {
    execQueue += MethodExit
  }

  def end(): Unit = {
    execQueue.foreach(historyQueue += _)
  }
  // API end ---

  def addHistory(item: CallStackItem,display:Boolean=true): Unit = {
    if (nowIndex > historyMaxIndex) {
      history.push(History(item, nowIndex, copyCallStack(callStack),display))
      historyIndexMap.put(nowIndex, history.size)
    }
  }

  def copyCallStack(cs: Seq[CallStackItem]): List[CallStackItem] = {
    def loop(stack: List[CallStackItem]): List[CallStackItem] = stack match {
      case h :: t => CallStackItem.dupe(h) :: loop(t)
      case Nil    => Nil
    }
    loop(cs.toList)
  }

  def nowIndex: Int = historyQueue.size - execQueue.size

  var prevHistSize = 0
  def showHistory: List[History] = {
    val dequeuePlanN =
      if (CallStackItem.state == Expanding ||
        CallStackItem.state == Contracting ||
        CallStackItem.state == SubEnd
      ) {
        1
      } else {
        0
      }
    val ni = nowIndex + dequeuePlanN
    val ret = (if (ni > historyMaxIndex) ConnectTest.history
    else {
      val n: Int = historyIndexMap.getOrElse(ni, historyIndexMap.filter(kv => kv._1 < ni).maxBy(kv => kv._1)._2)
      ConnectTest.history.drop(ConnectTest.history.size - n + 1)
    }).toList
    ret.filter(_.display)
  }

  addCallStack("dummy")
  push("sliceRecursive(start, end, list)")
  // ListMapTes




  // ListMapTes
  push("(start, end, ls) match {")
  push("(1, 3, List(\"a\", \"b\", \"c\", \"d\", \"e\")) match {")
    push("case (_, _, Nil) => Nil")
    push("case (_, e, _) if e <= 0")
    push("case (s, e, h :: tail) if s <= 0")
    push("case (s, e, h :: tail)")
    push("case (1, 3, \"a\" :: List(\"b\", \"c\", \"d\", \"e\"))")
    push("sliceRecursive(s - 1, e - 1, tail)")
    push("sliceRecursive(0, 2, List(\"b\", \"c\", \"d\", \"e\"))")
    //addCallStack()
    push("(start, end, ls) match {")
    push("(0, 2, List(\"b\", \"c\", \"d\", \"e\")) match {")
      push("case (_, _, Nil) => Nil")
  push("case (_, e, _) if e <= 0")
  push("case (s, e, h :: tail) if s <= 0")
  push("case (0, 2, \"b\" :: List(\"c\", \"d\", \"e\")) if 0 <= 0")
  push("h :: sliceRecursive(0, e - 1, tail)")
  push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))")
  addCallStack("sliceRecursive")
  push("(start, end, ls) match {")
  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))") //
  addCallStack("sliceRecursive") //
  push("(start, end, ls) match {")
  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  push("case (_, _, Nil) => Nil")
  //  push("case (_, e, _) if e <= 0")
  //  push("case (s, e, h :: tail) if s <= 0")
  //  push("case (0, 1, \"c\" :: List(\"d\", \"e\")) if 0 <= 0")
  //  push("h :: sliceRecursive(0, e - 1, tail)")
  //  push("\"c\" :: sliceRecursive(0, 0, List(\"d\", \"e\"))")
  //  push("(start, end, ls) match {")
  //  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  //  push("case (_, _, Nil) => Nil")
  push("case (_, e, _) if e <= 0")
  push("case (_, 0, _) if 0 <= 0")
  push("Nil")
  popCallStack()
  push("List(\"c\")")
  popCallStack()
  push("List(\"b\", \"c\")")
  callStack.push(CallStackItem(callStack.size))
  push("case (_, _, Nil) => Nil") //
  push("case (_, _, Nil) => Nil") //
  push("case (_, _, Nil) => Nil") //
  end()

  def main(args: Array[String]) = Application.launch(classOf[ConnectTest])

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
case object ListMethodStart extends AnimationState
case object ListMethodEnd extends AnimationState

class ConnectTest extends Application with myUtil {

  import ConnectTest._

  // use mouse event
  var hmx, hmy: Int = 0
  var restoreHistoryF = false


  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(1200, 600)
    val gc: GraphicsContext = canvas.getGraphicsContext2D

    val historyPane = new ScrollPane()
    val hCanvas = new Canvas(300, 400)
    val hgc = hCanvas.graphicsContext2D
    historyPane.content = hCanvas
    historyPane.layoutX = 900
    historyPane.layoutY = 200
    historyPane.maxHeight = 400
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
    new Stage(stage) {
      title = "connectTest"
      scene = new Scene() {
        content += canvas
        content += historyPane
      }
    }.show

    mainLoop.keyFrames = KeyFrame(16 ms, "ViFPiS", (e: ActionEvent) => runFrame(gc, hgc))
    mainLoop.play
  }

  //  n倍速
  val speed = 2
  var frameCount = 0
  def runFrame(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    gc.setFont(Font("Consolas", 24))
    execute(gc, hgc)
    if (CallStackItem.state == Fixed) return
    draw(gc, hgc)
    printAllHistory()(hgc)
  }

  var count = 0
  def draw(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    implicit val g = gc
    clearCanvas()
    val now = callStack.top

    if (CallStackItem.state != MoveDown) {
      printLastTwoHistory()
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
    }

    if (CallStackItem.subStep && CallStackItem.state != SubEnd) {
      textInBox(CallStackItem.mainText, now.offsetX, now.offsetY, now.width, now.height, now.padding)
    }


  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def execute(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    frameCount += 1
    count += speed

    implicit val g = gc

    //    println(callStack.mkString(" ") + "\n+" +
    //      (callStack.top.text, callStack.top.nextText) + "\n" +
    //      (execQueue.head, CallStackItem.state) + "\n")

    val now = callStack.top
    CallStackItem.state match {
      case Fixed =>
        val command = execQueue.dequeue()
        now.update()
        command.split("<@>") match {
          case Array(MethodEnter, msg) =>
            if (callStack.size == 1) {
              // aete
              now.update()
              callStack.push(CallStackItem(callStack.size))
              callStack.top.updateNext(execQueue.dequeue())
              callStack.top.update()
              callStack.top.updateNext(execQueue.dequeue())
              CallStackItem.state = Emphasis
              count = 0
            } else {
              callStack.push(CallStackItem(callStack.size, offsetX = calcOffsetX(now, msg)))
              callStack.top.updateNext(extractMethodText(now, msg))
              callStack.top.update()
              CallStackItem.state = Expanding
            }

          case Array(MethodExit) =>
            now.updateNext(MethodExit)
            CallStackItem.state = if (CallStackItem.subStep) SubEnd else Contracting

          case Array(msg) => // 単純に置き換え
            //    callStack.top.update()
            now.updateNext(command)
            if (now.nextText.startsWith("case ")) {
              if (!CallStackItem.subStep) {
                CallStackItem.mainText = now.text
                ConnectTest.addHistory(now,display = false)
                now.update()
                now.updateNext(execQueue.dequeue())
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
        }

      case Emphasis =>
        if (count > 60) {
          count = 0
          updateSize(now.nextText, now.depth)
          CallStackItem.state = Shadow
        }

      case Shadow =>
        if (count > 60) {
          count = 0
          CallStackItem.state = MoveDown
        }

      case MoveDown =>
        if (count > 120) {
          count = 0
          ConnectTest.addHistory(now)
          CallStackItem.state = Fixed
        }

      case Expanding =>
        if (count > 60) {
          count = 0
          callStack.top.updateNext(execQueue.dequeue())
          CallStackItem.state = Emphasis
        }

      case Contracting =>
        if (count > 60) {
          count = 0
          callStack.pop()
          val caller = callStack.top
          val (s, e) = caller.methodCallIndex
          val h = caller.text.take(s)
          val t = caller.text.drop(e)
          val returnValueText = h + s"%-${now.maxIndex}s".format(now.text) + t
          caller.updateNext(returnValueText)
          caller.update()
          caller.updateNext(execQueue.dequeue())

          CallStackItem.state = Emphasis
        }

      case SubExp =>
        if (count > 60) {
          count = 0
          CallStackItem.state = Emphasis
        }

      case SubEnd =>
        if (count > 60) {
          count = 0
          CallStackItem.subStep = false
          CallStackItem.state = if (now.nextText == MethodExit) Contracting else Emphasis
        }
    }

    updateSize(callStack.top.text, callStack.top.depth)

  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def searchMethodEnd(text: String, braceCount: Int = 0, index: Int = 1): Int = text.toList match {
    case '(' :: t                    => searchMethodEnd(t.mkString(""), braceCount + 1, index + 1)
    case ')' :: t if braceCount == 1 => index
    case ')' :: t                    => searchMethodEnd(t.mkString(""), braceCount - 1, index + 1)
    case h :: t                      => searchMethodEnd(t.mkString(""), braceCount, index + 1)
    case Nil                         =>
      sys.error(s"引数 $text が不正なテキストです")
      0 // ここには来ないはず
  }

  def updateSize(text: String, depth: Int)(implicit gc: GraphicsContext): Unit = {
    callStack.foreach { item =>
      val (w, h) = textSize(text)
      val diffX = (depth - item.depth) * CallStackItem.DefaultX
      item.updateSize(w + diffX, h)
    }
  }

  def calcOffsetX(item: CallStackItem, callMethod: String)(implicit gc: GraphicsContext): Int = {
    val text = item.text.split(callMethod) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰むかも？
    item.offsetX + textSize(text(0))._1
  }

  def extractMethodText(item: CallStackItem, callMethod: String)(implicit gc: GraphicsContext): String = {
    val startIndex = item.text.indexOf(callMethod)
    val methodStartText = item.text.drop(startIndex) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰むかも？
    val endIndex = searchMethodEnd(methodStartText)
    item.setMethodCallIndex(startIndex, startIndex + endIndex)
    methodStartText.take(endIndex)
  }

  // textの左上がxy、paddingに応じて四角形が大きくなる
  def textInBox(text: String, x: Int, y: Int, w: Int, h: Int, padding: Int = 8, boxColor: Color = Color.White,
    eff1: Effect = null, eff2: Effect = null, lw: Int = 2, bc: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    rectWithBorder(x - padding / 4, y - h + des - padding / 4 + lw / 2, w + padding / 2, h + padding / 2, boxColor, eff1, eff2, lw, bc)
    gc.fillText(text, x, y)
  }

  def rectWithBorder(x: Int, y: Int, w: Int, h: Int, fillColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null, lw: Int = 2, borderColor: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    gc.setEffect(eff1)
    gc.setFill(fillColor)
    gc.fillRect(x, y, w, h)
    gc.setEffect(eff2)
    gc.setStroke(borderColor)

    gc.lineWidth = lw
    gc.setFill(Color.Black)
    gc.strokeRect(x, y, w, h)
  }

  def fontMetrics(implicit gc: GraphicsContext): com.sun.javafx.tk.FontMetrics = Toolkit.getToolkit.getFontLoader.getFontMetrics(gc.font)

  def textSize(text: String)(implicit gc: GraphicsContext): (Int, Int) = {
    val metrics = fontMetrics
    (metrics.computeStringWidth(text) toInt, (metrics.getLineHeight + metrics.getMaxDescent).toInt)
  }

  def textSizeWithPadding(text: String, padding: Int)(implicit gc: GraphicsContext): (Int, Int) = textSize(text).map(_ + padding)

  def clearCanvas()(implicit gc: GraphicsContext) {
    gc.setFill(Color.White)
    gc.fillRect(0, 0, 1200, 600)
  }

  def sinMove(startY: Int, count: Int, limit: Int, distance: Int): Int = {
    startY + ((Math.sin(-Math.PI / 2 + Math.PI / limit * count) + 1) / 2 * distance).toInt
  }


  def pulseLine(text: String, x: Int, y: Int, w: Int = 0, h: Int = 0, padding: Int = 8, frame: Int, fillColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    val lww = if (frame % 30 < 15) (frame % 30) / 4 + 1 else 4 - (frame % 15) / 4
    rectWithBorder(x - padding / 4, y - h + des - padding / 4 + lww / 2, w + padding / 2, h + padding / 2, lw = lww, borderColor = Color.Red, fillColor = fillColor)
    gc.fillText(text, x, y)
  }

  // 直近2つを近くに表示
  def printLastTwoHistory()(implicit gc: GraphicsContext): Unit = {
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

  def printAllHistory()(implicit gc: GraphicsContext): Unit = {
    val canvas = gc.canvas
    val hist = ConnectTest.history
    val height = 32
    canvas.height = (hist.size * height + height / 2) max 400
    for (h <- hist.reverse.zipWithIndex) {
      val x = (h._1.x - CallStackItem.DefaultX) / 10 + canvas.getLayoutX.toInt+5
      val y = (h._2 + 1) * height + canvas.getLayoutY.toInt
      val newHy = hmy + canvas.getLayoutY.toInt
      val withinRange = newHy - height / 3 < y && y < newHy + height / 1.5 // よく分からんけど、3と1.5がちょうどいい
      val boxColor = if (withinRange) Color.SlateGray else Color.LightGray
      val borderColor = if (withinRange) Color.DimGray else Color.Black
      textInBox(h._1.text, x, y, h._1.w, height - 8, boxColor = boxColor, bc = borderColor)
      // 履歴がクリックされていたら、その場所を復元する
      // クリックイベントにしなかった理由は、ここで定義したheightによって位置が変わることと、withinRangeを利用するため
      if (withinRange && restoreHistoryF) {
        restoreHistory(h._2, h._1.qIndex)
      }
    }
    restoreHistoryF = false
  }


  // 今までの呼び出し状況の描画、ただし最初に無駄なcallStackItemが1つあるので除去
  def printCallStack()(implicit gc: GraphicsContext): Unit = {
    callStack.tail.reverse.tail.foreach(item => textInBox(item.text, item.offsetX, item.offsetY, item.width, item.height, item.padding))
  }

  def restoreHistory(index: Int, qIndex: Int): Unit = {
    historyMaxIndex = historyMaxIndex max history.head.qIndex
    callStack.clear()
    callStack.pushAll(copyCallStack(history.reverse(index).state).reverse)

    count = 0
    execQueue.clear()
    execQueue ++= historyQueue.drop(qIndex)

    val now = callStack.top
    val nextIsSubexp = now.nextText.startsWith("case ")
    CallStackItem.subStep = nextIsSubexp || now.text.startsWith("case ")
    CallStackItem.state = if (CallStackItem.subStep) SubExp else Emphasis
    if (nextIsSubexp) {
      now.update()
      now.updateNext(execQueue.dequeue())
    }

  }

}