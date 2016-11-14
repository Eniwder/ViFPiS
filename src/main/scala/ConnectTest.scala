import javafx.application.Application

import com.sun.javafx.tk.Toolkit

import scala.collection.mutable
import scala.language.postfixOps
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.effect.{DropShadow, Effect}
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage


/**
  * Created by slab on 2016/09/14.
  */
object ConnectTest {
  type Command = String
  type History = (String, Int, Int)
  private val execQueue = new scala.collection.mutable.Queue[Command]
  val callStack = new scala.collection.mutable.Stack[CallStackItem]
  private val history = new scala.collection.mutable.Stack[History]
  val MethodEnter = "MEnter"
  val MethodExit = "MExit"

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }

  def push(next: String) {
    execQueue += next
  }

  def addCallStack(method: String = "") {
    execQueue += s"$MethodEnter<@>$method"
  }

  def popCallStack() {
    execQueue += MethodExit
  }

  def addHistory(item: History): Unit = {
    history.push(item)
  }


  addCallStack("dummy")
  push("sliceRecursive(start, end, list)")
  push("(start, end, ls) match {")
  push("(1, 3, List(\"a\", \"b\", \"c\", \"d\", \"e\")) match {")
//  push("case (_, _, Nil) => Nil")
//  push("case (_, e, _) if e <= 0")
//  push("case (s, e, h :: tail) if s <= 0")
//  push("case (s, e, h :: tail)")
//  push("case (1, 3, \"a\" :: List(\"b\", \"c\", \"d\", \"e\"))")
//  push("sliceRecursive(s - 1, e - 1, tail)")
//  push("sliceRecursive(0, 2, List(\"b\", \"c\", \"d\", \"e\"))")
//  //addCallStack()
//  push("(start, end, ls) match {")
//  push("(0, 2, List(\"b\", \"c\", \"d\", \"e\")) match {")
//  push("case (_, _, Nil) => Nil")
//  push("case (_, e, _) if e <= 0")
//  push("case (s, e, h :: tail) if s <= 0")
//  push("case (0, 2, \"b\" :: List(\"c\", \"d\", \"e\")) if 0 <= 0")
//  push("h :: sliceRecursive(0, e - 1, tail)")
  push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))")
  addCallStack("sliceRecursive")
  push("(start, end, ls) match {")
  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  push("\"b\" :: sliceRecursive(0, 1, List(\"c\", \"d\", \"e\"))") //
  addCallStack("sliceRecursive") //
  push("case (_, _, Nil) => Nil")
  push("case (_, e, _) if e <= 0")
  push("case (s, e, h :: tail) if s <= 0")
  push("case (0, 1, \"c\" :: List(\"d\", \"e\")) if 0 <= 0")
  push("h :: sliceRecursive(0, e - 1, tail)")
  push("\"c\" :: sliceRecursive(0, 0, List(\"d\", \"e\"))")
  push("(start, end, ls) match {")
  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
  push("case (_, _, Nil) => Nil")
  push("case (_, e, _) if e <= 0")
  push("case (_, 0, _) if 0 <= 0")
  push("Nil")
  popCallStack()
  push("List(\"c\")")
  popCallStack()
  push("List(\"b\", \"c\")")
  callStack.push(CallStackItem(callStack.size))

  def main(args: Array[String]) = Application.launch(classOf[ConnectTest])

}

sealed trait AnimationState
case object MoveDown extends AnimationState
case object Emphasis extends AnimationState
case object Shadow extends AnimationState
case object Fixed extends AnimationState
case object Expanding extends AnimationState
case object Contracting extends AnimationState

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

class ConnectTest extends Application with myUtil {

  import ConnectTest._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(1200, 600)
    val gc: GraphicsContext = canvas.getGraphicsContext2D

    new Stage(stage) {
      title = "connectTest"
      scene = new Scene() {
        content = canvas
      }
    }.show

    runFrame(gc)

    mainLoop.keyFrames = KeyFrame(16 ms, "ViFPiS", (e: ActionEvent) => runFrame(canvas.graphicsContext2D))
    mainLoop.play
  }

  //  n倍速
  val speed = 1
  var frameCount = 0
  def runFrame(gc: GraphicsContext): Unit = {
    gc.setFont(Font("Consolas", 24))
    execute(gc)
    draw(gc)
  }

  var count = 0
  def draw(gc: GraphicsContext): Unit = {
    implicit val g = gc
    clearCanvas()
    val now = callStack.top

    if (CallStackItem.state != MoveDown) {
      printHistory()
    }


    CallStackItem.state match {
      case Emphasis =>
        val width =
          if (textSize(now.nextText)._1 > now.width) {
            val nextWidth = textSize(now.nextText)._1
            val diff = nextWidth - now.width
            now.width + (diff / 60.0 * count).toInt
          } else {
            now.width
          }
        printCallStack()
        pulseLine(now.text, now.offsetX, now.offsetY, width, now.height, now.padding, count)


      case Shadow =>
        val shadowDepth = Math.max(0xF0 - count, 0xA0)
        val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
        printCallStack()
        textInBox(now.text, now.offsetX, now.offsetY, now.width, now.height, now.padding, eff1 = dsE)

      case MoveDown =>
        val shadowDepth = Math.min(0xA0 + count / 1.5, 0xF0).toInt
        val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
        printCallStack()
        textInBox(now.nextText, now.offsetX, now.offsetY, now.width, now.height, now.padding)
        val gray1 = (0xF0 - (count / 60.0) * 0x2F).toInt
        textInBox(now.text, now.offsetX, sinMove(now.offsetY, count, 120, 120), now.width, now.height, now.padding, eff1 = dsE, boxColor = Color.rgb(gray1, gray1, gray1)) // 置き換え元

        ConnectTest.history match {
          case (h1 +: h2 +: tail) =>
            val gray2 = (0xA0 - (count / 60.0) * 0x20).toInt
            textInBox(h1._1, 100, sinMove(now.offsetY + 60 * 2, count, 120, 120), h1._2, h1._3, now.padding, boxColor = Color.rgb(gray2, gray2, gray2))
            gc.setGlobalAlpha(1.0 - count / 60.0)
            textInBox(h2._1, 100, sinMove(now.offsetY + 60 * 4, count, 120, 120), h2._2, h2._3, now.padding, boxColor = Color.rgb(0x80, 0x80, 0x80))
            gc.setGlobalAlpha(1)

          case (h1 +: mutable.Stack()) =>
            val gray2 = (0xA0 - (count / 60.0) * 0x20).toInt
            textInBox(h1._1, 100, sinMove(now.offsetY + 60 * 2, count, 120, 120), h1._2, h1._3, now.padding, boxColor = Color.rgb(gray2, gray2, gray2))

          case _ =>
        }

      case Fixed =>

      case Expanding =>
        printCallStack()
        textInBox(callStack(1).text, callStack(1).offsetX, callStack(1).offsetY, callStack(1).width, callStack(1).height, callStack(1).padding)
        gc.setGlobalAlpha(count / 60.0)
        textInBox(now.text, now.offsetX, now.offsetY, now.width, now.height, now.padding)
        gc.setGlobalAlpha(1)

      case Contracting =>
    }

  }


  def execute(gc: GraphicsContext): Unit = {
    frameCount += 1
    count += speed

    implicit val g = gc

    println(callStack.mkString(" "))
    println(callStack.top.text, callStack.top.nextText)
    println(execQueue.head, CallStackItem.state)
    println()

    val now = callStack.top
    CallStackItem.state match {
      case Fixed =>
        val command = execQueue.dequeue()
        now.update()
        command.split("<@>") match {
          case Array(MethodEnter, msg) =>
            if (callStack.size == 1) {  // aete
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

          case Array(MethodExit, msg) =>
            callStack.pop()
            CallStackItem.state = Contracting

          case Array(msg) => // 単純に置き換え
            //    callStack.top.update()
            callStack.top.updateNext(command)
            CallStackItem.state = Emphasis
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
          ConnectTest.addHistory(now.text, now.width, now.height)
          CallStackItem.state = Fixed
        }

      case Expanding =>
        if (count > 60) {
          count = 0
          callStack.top.updateNext(execQueue.dequeue())
          CallStackItem.state = Emphasis
        }

      case Contracting =>
      case _ =>
    }

    updateSize(callStack.top.text, callStack.top.depth)

  }

  def searchMethodEnd(text: String, braceCount: Int = 0, index: Int = 1): Int = text.toList match {
    case '(' :: t => searchMethodEnd(t.mkString(""), braceCount + 1, index + 1)
    case ')' :: t if braceCount == 1 => index
    case ')' :: t => searchMethodEnd(t.mkString(""), braceCount - 1, index + 1)
    case h :: t => searchMethodEnd(t.mkString(""), braceCount, index + 1)
    case Nil =>
      println(s"引数 $text が不正なテキストです")
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
    val text = item.text.split(callMethod) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰む
    item.offsetX + textSize(text(0))._1
  }

  def extractMethodText(item: CallStackItem, callMethod: String)(implicit gc: GraphicsContext): String = {
    val startIndex = item.text.indexOf(callMethod)
    val methodStartText = item.text.drop(startIndex) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰む
    val endIndex = searchMethodEnd(methodStartText)
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

  // TODO 履歴部分
  def printHistory()(implicit gc: GraphicsContext): Unit = {
    // 直近2つは近くに表示、それ以外は履歴部分へ
    ConnectTest.history match {
      case (h1 +: h2 +: tail) =>
        textInBox(h1._1, 100, 220, h1._2, h1._3, boxColor = Color.rgb(0xA0, 0xA0, 0xA0))
        textInBox(h2._1, 100, 340, h2._2, h2._3, boxColor = Color.rgb(0x80, 0x80, 0x80))

      case (h1 +: mutable.Stack()) =>
        textInBox(h1._1, 100, 220, h1._2, h1._3, boxColor = Color.rgb(0xA0, 0xA0, 0xA0))

      case (h1 +: tail) =>
        textInBox(h1._1, 100, 220, h1._2, h1._3, boxColor = Color.rgb(0xA0, 0xA0, 0xA0))
        textInBox(tail.top._1, 100, 340, tail.top._2, tail.top._3, boxColor = Color.rgb(0x80, 0x80, 0x80))


      case _ =>
    }
  }

  // 今までの呼び出し状況の描画、ただし最初に無駄なcallStackItemが1つあるので除去
  def printCallStack()(implicit gc: GraphicsContext): Unit = {
    callStack.tail.reverse.tail.foreach(item => textInBox(item.text, item.offsetX, item.offsetY, item.width, item.height, item.padding))
  }

}
