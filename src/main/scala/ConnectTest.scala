import javafx.application.Application

import com.sun.javafx.tk.Toolkit

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
  private val execQueue = new scala.collection.mutable.Queue[Command]
  private val callStack = new scala.collection.mutable.Stack[CallStackItem]
  val MethodEnter = "MEnter"
  val MethodExit = "MExit"


  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }

  def push(next: String) {
    execQueue += next
  }

  def addCallStack() {
    execQueue += MethodEnter
  }

  def popCallStack() {
    execQueue += MethodExit
  }


  addCallStack()
  push("sliceRecursive(start, end, list)")
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
  addCallStack()
  push("(start, end, ls) match {")
  push("(0, 1, List(\"c\", \"d\", \"e\")) match {")
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
case object Fixed extends AnimationState
case object Expanding extends AnimationState
case object Contracting extends AnimationState

case class CallStackItem(depth: Int) {
  var width = 0
  var height = 0

  private var now = ""
  private var next = ""


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
}

class ConnectTest extends Application with myUtil {

  import ConnectTest._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(800, 600)
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

  var frameCount = 0
  var sy = 75
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

    CallStackItem.state match {
      case Emphasis =>
        if (count <= 60) {  // 赤い枠
          pulseLine(now.text, 100, 100, now.width, now.height, count)
        } else if (count <= 120) {  // 影をつける
          val shadowDepth = Math.max(0xF0 - (count % 60), 0xA0)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
          textInBox(now.text, 100, 100, now.width, now.height, eff1 = dsE)
        } else {
          count = 0
          CallStackItem.state = MoveDown
        }
      case MoveDown =>
        textInBox(now.nextText, 100, 100, now.width, now.height)
        textInBox(now.text, 100, 100 + count, now.width, now.height)
      case Fixed =>

      case Expanding =>
      case Contracting =>
    }

    count += 1

  }

  def execute(gc: GraphicsContext): Unit = {
    frameCount += 1
    implicit val g = gc
    val Padding = 8

    println(callStack.mkString(" "))
    println(callStack.top.text)
    println(execQueue.head, CallStackItem.state)
    println()

    val now = callStack.top
    if (CallStackItem.state == Fixed) {
      val command = execQueue.dequeue()
      now.update()
      command match {
        case MethodEnter =>
          now.update()
          callStack.push(CallStackItem(callStack.size))

          if (callStack.size == 2) {  // aete
            callStack.top.updateNext(execQueue.dequeue())
            callStack.top.update()
            callStack.top.updateNext(execQueue.dequeue())
            CallStackItem.state = Emphasis
            count = 0
          } else {
            CallStackItem.state = Expanding
          }

        case MethodExit =>
          callStack.pop()
          CallStackItem.state = Contracting

        case _ => // 単純に置き換え
          callStack.top.update()
          callStack.top.updateNext(command)
          CallStackItem.state = Emphasis
          count = 0
      }
    }

    now.updateSize(textSizeWithPadding(now.text, Padding))
  }

  // 四角の左上がx
  def textInBox(text: String, x: Int, y: Int, w: Int, h: Int, padding: Int = 8, boxColor: Color = Color.White,
                eff1: Effect = null, eff2: Effect = null, lw: Int = 2, bc: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    rectWithBorder(x, -h + 2 + des + y, w, h, boxColor, eff1, eff2, lw, bc)
    gc.fillText(text, x + padding / 2, y - padding / 2)
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
    gc.fillRect(0, 0, 800, 600)
  }

  def pulseLine(text: String, x: Int, y: Int, w: Int = 0, h: Int = 0, frame: Int, padding: Int = 8, fillColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    val lww = if (frame % 30 < 15) (frame % 30) / 4 + 1 else 4 - (frame % 15) / 4

    rectWithBorder(x, -h + 2 + des + y, w, h, lw = lww, borderColor = Color.Red, fillColor = fillColor)
    gc.fillText(text, x + padding / 2, y - padding / 2)
  }

}
