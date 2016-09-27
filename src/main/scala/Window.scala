import javafx.application.Application

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


class Window extends Application {

  import Window._

  def start(stage: javafx.stage.Stage): Unit = {
    val canvas = new Canvas(800, 600)
    val gc = canvas.getGraphicsContext2D

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
    gc.setFill(Color.White)
    gc.fillRect(0, 0, 800, 600)

    gc.setFont(Font("Consolas", 24))

    val shadowDepth = Math.max(0xF0 - frameCount, 0xA0)
    val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
    whiteRectWithBlackBorder(gc, 100, 100 + frameCount, 300, 100, dsE)


    gc.fillText("sliceRecursive", 200, 150 + frameCount)
  }

  def execute(): Unit = {
    frameCount += 1

  }

  def whiteRectWithBlackBorder(gc: GraphicsContext, x: Int, y: Int, w: Int, h: Int, eff1: Effect = null, eff2: Effect = null): Unit = {
    gc.setEffect(eff1)
    gc.setFill(Color.White)
    gc.fillRect(x, y, w, h)
    gc.setEffect(eff2)
    gc.setFill(Color.Black)
    gc.strokeRect(x, y, w, h)
  }

  def eitherOneIfLeft[T](v: T, defaultValue: T)(p: T => Boolean): T = if (p(v)) v else defaultValue

}
