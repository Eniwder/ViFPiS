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
object Demo3 {
  private val queue = scala.collection.mutable.Queue.empty[String]

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }


  def push(msg: String): Unit = {
    queue += msg
  }

  def main(args: Array[String]) = Application.launch(classOf[Demo3])

}


class Demo3 extends Application with myUtil {

  import Demo3._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(800, 600)
    val gc: GraphicsContext = canvas.getGraphicsContext2D

    new Stage(stage) {
      title = "v3"
      scene = new Scene() {
        content = canvas
      }
    }.show

    runFrame(gc)

    mainLoop.keyFrames = KeyFrame(16 ms, "ViFPiS", (e: ActionEvent) => runFrame(canvas.graphicsContext2D))
    mainLoop.play
  }

  var frameCount = 0
  def runFrame(gc: GraphicsContext): Unit = {
    execute()
    draw(gc)
  }

  def draw(gc: GraphicsContext): Unit = {
    implicit val g = gc

    gc.setFont(Font("Consolas", 24))

    if (frameCount < 60 * 2) {
      clearCanvas()
      textInBox("'b :: sliceRecursive(0, 1, List('c, 'd, 'e))", 100, 100, 50)
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75)
    } else if (frameCount < 60 * 4) {
      clearCanvas()
      val shadowDepth = Math.max(0xF0 - (frameCount - 60 * 2), 0xA0)
      val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
      textInBox("'b :: sliceRecursive(0, 1, List('c, 'd, 'e))", 100, 100, 50)
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75, eff1 = dsE)

    } else if (frameCount < 60 * 6) {
      clearCanvas()
      val shadowDepth = Math.max(0xF0 - (frameCount - 60 * 2), 0xA0)
      val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
      textInBox("'b :: (0, 1, List('c, 'd, 'e)) match", 100, 100, 50)
      textInBox("(0, 1, List('c, 'd, 'e)) match", 202, 75)

      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75+ (frameCount - 60 * 4), eff1 = dsE)
    } else {
      clearCanvas()
      textInBox("'b :: (0, 1, List('c, 'd, 'e)) match", 100, 100, 50)
      textInBox("(0, 1, List('c, 'd, 'e)) match", 202, 75)

      val colorDepth = Math.max(0xD0, 0xFF - (frameCount - 60 * 6))
      textInBox("sliceRecursive(0, 1, List('c, 'd, 'e))", 202, 75 + 120, boxColor = Color.rgb(colorDepth, colorDepth, colorDepth))
    }

    if (frameCount > 60 * 8) frameCount = 0

  }

  def execute(): Unit = {
    frameCount += 1

  }

  def textInBox(text: String, x: Int, y: Int, padding: Int = 8, boxColor: Color = Color.White, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    val (w, h) = textSizeWithPadding(text, padding)
    val des = fontMetrics.getDescent.toInt
    rectWithBlackBorder(x, -h + 2 + des + y, w, h, boxColor, eff1, eff2)
    gc.fillText(text, x + padding / 2, y - padding / 2)
  }

  def rectWithBlackBorder(x: Int, y: Int, w: Int, h: Int, c: Color = Color.White, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    gc.setEffect(eff1)
    gc.setFill(c)
    gc.fillRect(x, y, w, h)
    gc.setEffect(eff2)
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

}
