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
object Demo8 {
  private val queue = scala.collection.mutable.Queue.empty[String]

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }


  def push(msg: String): Unit = {
    queue += msg
  }

  def main(args: Array[String]) = Application.launch(classOf[Demo8])

}


class Demo8 extends Application with myUtil {

  import Demo8._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(800, 600)
    val gc: GraphicsContext = canvas.getGraphicsContext2D

    new Stage(stage) {
      title = "v8"
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
    if (frameCount < 60 * 1) {
      clearCanvas()
      textInBox("'b :: sliceRecursive(0, 1, List('c, 'd, 'e))", 100, 100, 50, ww = 640)
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75)

    } else if (frameCount < 60 * 2) {
      clearCanvas()
      textInBox("'b :: sliceRecursive(0, 1, List('c, 'd, 'e))", 100, 100, 50, ww = 640)
      redLine("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75, (frameCount - 60))
    } else if (frameCount < 60 * 2+10) {
    } else if (frameCount < 60 * 4) {
      clearCanvas()
      val shadowDepth = Math.max(0xF0 - (frameCount - 60 * 2), 0xA0)
      val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
      textInBox("'b :: sliceRecursive(0, 1, List('c, 'd, 'e))", 100, 100, 50, ww = 640)
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75, eff1 = dsE,bc=Color.Red)

    } else if (frameCount < 60 * 6) {
      clearCanvas()
      val shadowDepth = Math.max(0xF0 - (frameCount - 60 * 2), 0xA0)
      val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
      textInBox("'b :: (0, 1, List('c, 'd, 'e)) match", 100, 100, 50, ww = 640)
      textInBox("(0, 1, List('c, 'd, 'e)) match", 202, 75)
      // sin(PI/2/120*Count)*300 , (sin( -PI/2 + PI/120*Count )+1)/2*300
      val xxx2 = 75 + ((Math.sin(-Math.PI / 2 + Math.PI / 120 * (frameCount - 60 * 4)) + 1) / 2 * 240).toInt
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, xxx2, eff1 = dsE,bc=Color.Red)

    } else {
      clearCanvas()
      textInBox("'b :: (0, 1, List('c, 'd, 'e)) match", 100, 100, 50, ww = 640)
      textInBox("(0, 1, List('c, 'd, 'e)) match", 202, 75)

      val colorDepth = Math.max(0xD0, 0xFF - (frameCount - 60 * 6))
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75 + 240, boxColor = Color.rgb(colorDepth, colorDepth, colorDepth))

    }

    if (frameCount > 60 * 8) frameCount = 0

  }

  def execute(): Unit = {
    frameCount += 1
  }

  def textInBox(text: String, x: Int, y: Int, padding: Int = 8, boxColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null, ww: Int = 0, hh: Int = 0, bc: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    val (w, h) = textSizeWithPadding(text, padding)
    val www = if (ww == 0) w else ww
    val hhh = if (hh == 0) h else hh
    val des = fontMetrics.getDescent.toInt
    rectWithBlackBorder(x, -h + 2 + des + y, www, hhh, boxColor, eff1, eff2, bc)
    gc.fillText(text, x + padding / 2, y - padding / 2)
  }

  def rectWithBlackBorder(x: Int, y: Int, w: Int, h: Int, c: Color = Color.White, eff1: Effect = null, eff2: Effect = null, borderColor: Color = Color.Black)(implicit gc: GraphicsContext): Unit = {
    gc.setEffect(eff1)
    gc.setFill(c)
    gc.fillRect(x, y, w, h)
    gc.setEffect(eff2)
    gc.setStroke(borderColor)

    gc.lineWidth = if(borderColor == Color.Black) 2 else 3
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

  def redLine(text: String, x: Int, y: Int, frame: Int, padding: Int = 8, boxColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null, ww: Int = 0, hh: Int = 0)(implicit gc: GraphicsContext): Unit = {
    val (w, h) = textSizeWithPadding(text, padding)
    val www = if (ww == 0) w else ww
    val hhh = if (hh == 0) h else hh
    val des = fontMetrics.getDescent.toInt
    val vLen = (hhh / 59.0) * frame
    val hLen = (www / 59.0) * frame
    val yy = -h + 2 + des + y
    rectWithBlackBorder(x, yy, www, hhh, boxColor, eff1, eff2)
    gc.fillText(text, x + padding / 2, y - padding / 2)
    gc.setStroke(Color.Red)
    gc.lineWidth = 3
    gc.strokeLine(x, yy + hhh, x, yy + hhh - vLen) // 1 -> 7
    gc.strokeLine(x, yy, x + hLen, yy) // 7 -> 9
    gc.strokeLine(x + www, yy, x + www, yy + vLen) // 9 -> 3
    gc.strokeLine(x + www, yy + hhh, x + www - hLen, yy + hhh) // 3 -> 1
    gc.setStroke(Color.Black)
    gc.lineWidth = 1
  }

}
