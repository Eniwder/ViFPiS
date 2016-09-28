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
object Window {
  private val queue = scala.collection.mutable.Queue.empty[String]

  val mainLoop = new Timeline {
    cycleCount = Timeline.Indefinite
  }


  def push(msg: String): Unit = {
    queue += msg
  }

  def main(args: Array[String]) = Application.launch(classOf[Window])

}


class Window extends Application with myUtil {

  import Window._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(800, 600)
    val gc: GraphicsContext = canvas.getGraphicsContext2D

    new Stage(stage) {
      title = "irof Advent Calendar 2012 - Save the Earth! irof-san"
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
    gc.setFill(Color.White)
    gc.fillRect(0, 0, 800, 600)

    gc.setFont(Font("Consolas", 24))

    val shadowDepth = Math.max(0xF0 - frameCount, 0xA0)
    val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
    val (w, h) = textSizeWithPadding("sliceRecursive", 2)
    val des = fontMetrics.getDescent.toInt
    whiteRectWithBlackBorder(100, -h + 2 + des + 100 + frameCount, w, h, eff1 = dsE)


    gc.fillText("sliceRecursive", 100, 100 + frameCount)
  }

  def execute(): Unit = {
    frameCount += 1

  }

  def whiteRectWithBlackBorder(x: Int, y: Int, w: Int, h: Int, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    gc.setEffect(eff1)
    gc.setFill(Color.White)
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

}
