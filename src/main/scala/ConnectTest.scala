import javafx.application.Application

import com.sun.javafx.tk.Toolkit

import scala.language.postfixOps
import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.effect.Effect
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage


/**
  * Created by slab on 2016/09/14.
  */
object ConnectTest {

  private val callStack = new scala.collection.mutable.Stack[CallStackItem]

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }

  def push(next: String): Unit = {
    callStack.top.push(next)
  }

  def addCallStack(): Unit = {
    callStack.push(CallStackItem(callStack.size))
  }


  def main(args: Array[String]) = Application.launch(classOf[ConnectTest])
}

case class CallStackItem(depth: Int) {
  var width = 0
  var height = 0

  var now = ""
  private val nexts = scala.collection.mutable.Queue.empty[String]

  def push(replace: String): Unit = {
    nexts += replace
  }

  def update(): String = {
    now = nexts.dequeue
    now
  }

  def updateSize(w: Int, h: Int): Unit = {
    width = width max w
    height = height max h
  }

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
    execute()
    draw(gc)
  }


  def draw(gc: GraphicsContext): Unit = {
    implicit val g = gc
    gc.setFont(Font("Consolas", 24))


  }

  def execute(): Unit = {
    frameCount += 1
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
