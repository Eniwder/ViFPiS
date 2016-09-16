import javafx.application.Application
import javafx.scene.paint.Color._

import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.{CycleMethod, LinearGradient, Stop}
import scalafx.stage.Stage
import javafx.scene.shape.ArcType

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
    val canvas = new Canvas(600, 600)
    val gc = canvas.getGraphicsContext2D

    new Stage(stage) {
      title = "irof Advent Calendar 2012 - Save the Earth! irof-san"
      scene = new Scene() {
        content = canvas
      }
    }.show

    runFrame(gc)

    mainLoop.keyFrames = KeyFrame(30 ms, "[irof]", (e: ActionEvent) => runFrame(canvas.graphicsContext2D))
    mainLoop.play
  }

  var frameCount = 0
  def runFrame(gc: GraphicsContext): Unit = {
    execute()
    draw(gc)
  }

  def draw(gc: GraphicsContext): Unit = {
    frameCount+=1
    val lg = LinearGradient(0, 0, 200, 200, true, CycleMethod.Reflect, Stop(0.0, RED), Stop(1.0, BLUE))
    gc.setStroke(lg)
    gc.setLineWidth(30)
    gc.stroke
    gc.setFill(GREEN)
    gc.setStroke(BLUE)
    gc.setLineWidth(5)
    gc.strokeLine(frameCount, 10, 10, 40)
    gc.fillOval(10, 60, 30, 30)
    gc.strokeOval(60, 60, 30, 30)
    gc.fillRoundRect(110, 60, 30, 30, 10, 10)
    gc.strokeRoundRect(160, 60, 30, 30, 10, 10)
    gc.fillArc(10, 110, 30, 30, 45, 240, ArcType.OPEN)
    gc.fillArc(60, 110, 30, 30, 45, 240, ArcType.CHORD)
    gc.fillArc(110, 110, 30, 30, 45, 240, ArcType.ROUND)
    gc.strokeArc(10, 160, 30, 30, 45, 240, ArcType.OPEN)
    gc.strokeArc(60, 160, 30, 30, 45, 240, ArcType.CHORD)
    gc.strokeArc(110, 160, 30, 30, 45, 240, ArcType.ROUND)
    gc.fillPolygon(Array(10, 40, 10, 40), Array(210, 210, 240, 240), 4)
    gc.strokePolygon(Array(60, 90, 60, 90), Array(210, 210, 240, 240), 4)
    gc.strokePolyline(Array(110, 140, 110, 140), Array(210, 210, 240, 240), 4)

  }

  def execute(): Unit = {
    println("here")
  }

}
