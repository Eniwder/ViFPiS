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
import scalafx.scene.effect.{BoxBlur, DropShadow, Effect}
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.stage.Stage


/**
  * Created by slab on 2016/09/14.
  */
object ConnectTest {

  //  n倍速
  val speed = 1

  type Command = String

  private val execQueue = new mutable.Queue[Command]
  private val historyQueue = new mutable.Queue[Command]
  val callStack = new mutable.Stack[CallStackItem]
  private val history = new mutable.Stack[History]
  private val historyIndexMap = new mutable.HashMap[Int, Int]()
  private var historyMaxIndex = 0

  private val CMItem = CollectionMethodItem()

  private val CMEnterIndexs = new mutable.Stack[Int]

  val MethodEnter = "MEnter"
  val MethodExit = "MExit"
  val CollectionMethodEnter = "CMEnter"
  val CollectionMethodExit = "CMExit"

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

  def enterCollectionMethod(list: String, funcName: String, lambda: CollectionLambda, funcVar: String = " "): Unit = {
    execQueue += s"$CollectionMethodEnter<@>$list<@>$funcName<@>${lambda.arg}<@>${lambda.exp}<@>$funcVar"
    CMEnterIndexs.push(execQueue.size)
  }

  def exitCollectionMethod(result: String): Unit = {
    execQueue += s"$CollectionMethodExit"
    // 演算結果が先に欲しいので、直近の演算開始のところにぶち込む、現状履歴管理が楽
    val n = CMEnterIndexs.pop()
    val tmp = execQueue.take(n - 1)
    val tmp2 = execQueue.drop(n - 1)
    val addResultText = tmp2.dequeue() + "<@>" + result
    execQueue.clear()
    execQueue ++= tmp += addResultText ++= tmp2
  }

  // API end ---

  def addHistory(item: CallStackItem, display: Boolean = true): Unit = {
    if (nowIndex > historyMaxIndex) {
      history.push(History(item, nowIndex, copyCallStack(callStack), display))
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
  push("x :: list map { x => x * 2 } ::: List(a,b,c,d,e)")
  enterCollectionMethod( """List("a","b","c","d")""", "map", CollectionLambda("x", "x*2"))
  push("\"aa\"")
  push("\"bb\"")
  push("\"cc\"")
  push("\"dd\"")
  exitCollectionMethod( """List("aa","bb","cc","dd")""")
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
case object CollectionMethodStart extends AnimationState
case object CollectionMethodEnd extends AnimationState
case object CollectionMethodStep extends AnimationState

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

  var frameCount = 0
  def runFrame(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    gc.setFont(Font("Consolas", 24))
    execute(gc, hgc)
    if (CallStackItem.state == Fixed) return
    draw(gc, hgc)
    printAllHistory()(hgc)
  }

  var count = 0
  var countMax = 0
  def draw(gc: GraphicsContext, hgc: GraphicsContext): Unit = {
    implicit val g = gc
    clearCanvas()
    val now = callStack.top

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
        printCallStack()

      case CollectionMethodStep =>
        printProcess(now)
        textInBox(now.text, now.x, now.y, now.width, now.height, now.padding)
        val wh = textSize(CMItem.exp)
        textInBox(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, boxColor = Color.CornflowerBlue)
        val trg = CMItem.list(CMItem.iterateCount)
        val twh = textSize(trg)
        val sumX = CMItem.list.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
        if (count < 60) {
          // Listの要素を1つ選択し、強調
          printListAndLambda(now)
          gc.setGlobalAlpha(count / 30.0)
          textInBox(trg, now.x + sumX, now.y + 100, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setGlobalAlpha(1)
        } else if (count < 100) {
          // 選択した要素をラムダの引数へ動かす
          printListAndLambda(now)
          val count2 = count - 60
          textInBox(trg, sinMove(now.x + sumX, count2, 40, -sumX), sinMove(now.y + 100, count2, 40, 100), twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)

        } else if (count < 120) {
          // 動かしたらちょっと待つ
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
        } else if (count < 150) {
          // =>　を赤く点滅させる
          printListAndLambda(now, arrow = false)
          val count4 = count - 120
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          gc.setFill(Color.rgb(255, 0, 0))
          gc.fillText("=>", now.x + textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg))._1, now.y + 200)
        } else if (count < 210) {
          // 要素を実行した結果を生成
          val count5 = count - 150
          printListAndLambda(now, result = false)
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s".format("=>"))._1
          if (count5 < 30) {
            val bwh = 10 * count5 / 30
            gc.setEffect(new BoxBlur(bwh, bwh, 3))
            gc.fillText(CMItem.lambda.exp, now.x + resultX, now.y + 200)
            gc.setEffect(null)
          } else {
            val bwh = 9 - 10 * (count5 % 30) / 30
            gc.setEffect(new BoxBlur(bwh, bwh, 3))
            gc.fillText(CMItem.stepResult, now.x + resultX, now.y + 200)
            gc.setEffect(null)
          }
        } else if (count < 230) {
          // 結果が出たらちょっと待つ
          printListAndLambda(now, result = false)
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s".format("=>"))._1
          gc.fillText(CMItem.stepResult, now.x + resultX, now.y + 200)
        } else if (count < 260) {
          // 結果に枠と影をつける
          val count6 = count - 230
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val shadowDepth = Math.max(0xF0 - count6, 0xA0)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s".format("=>"))._1
          val stepResultWH = textSize(CMItem.stepResult)
          val bc = 255 - count6 * 255 / 30
          textInBox(CMItem.stepResult, now.x + resultX, now.y + 200, stepResultWH._1, stepResultWH._2, now.padding, eff1 = dsE, bc = Color.rgb(bc, bc, bc))
        } else if (count < 300) {
          // 結果を新しいListの場所へ動かす
          val count7 = count - 260
          printListAndLambda(now)
          textInBox(trg, now.x, now.y + 200, twh._1, twh._2, now.padding, boxColor = Color.CornflowerBlue)
          val resultX = textSize(s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s".format("=>"))._1
          val sumX = CMItem.actualResult.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
          val stepResultWH = textSize(CMItem.stepResult)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(0xA0, 0xA0, 0xA0))
          textInBox(CMItem.stepResult, sinMove(now.x + resultX, count7, 40, -resultX + sumX), sinMove(now.y + 200, count7, 40, 100), stepResultWH._1, stepResultWH._2, eff1 = dsE)
        } else {
          // 結果を置いて、他の部分を戻して少し待つ
          printListAndLambda(now)
          val stepResultWH = textSize(CMItem.stepResult)
          val sumX = CMItem.actualResult.take(CMItem.iterateCount).map(textSize(_)._1 + 8 / 2).sum
          textInBox(CMItem.stepResult, now.x + sumX, now.y + 300, stepResultWH._1, stepResultWH._2)
        }

        printCallStack()

      case CollectionMethodEnd =>
        if (count < 60) {
          printProcess(now)
          val diffW = if (textSize(now.nextText)._1 > now.width) {
            val nextWidth = textSize(now.nextText)._1
            val diff = nextWidth - now.width
            (diff / 60.0 * count).toInt
          } else {
            0
          }
          callStack.foreach { item => item.updateSize(item.width + diffW, item.height) }
          pulseLine(now.text, now.x, now.y, now.width, now.height, now.padding, count)
          val wh = textSize(CMItem.exp)
          pulseLine(CMItem.exp, calcOffsetX(now, CMItem.exp), now.y, wh._1, wh._2, now.padding, count, bc = Color.CornflowerBlue)
        }else{
          val count2 = count -60
          val shadowDepth = Math.max(0xF0 - count2, 0xA0)
          val dsE = new DropShadow(10, 10, 10, Color.rgb(shadowDepth, shadowDepth, shadowDepth))
          textInBox(now.text, now.x, now.y, now.width, now.height, now.padding,eff1=dsE)

          gc.setGlobalAlpha(1 - count2/60.0)
          printProcess(now)
          gc.setGlobalAlpha(count2/60.0)
          printLastTwoHistory()
          gc.setGlobalAlpha(1)
        }

        printCallStack()


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

          case Array(CollectionMethodEnter, list, funcName, lambdaArg, lambdaExp, funcVar, result) =>
            CMItem.listString = list
            CMItem.name = funcName
            CMItem.lambda = CollectionLambda(lambdaArg, lambdaExp)
            CMItem.variable = funcVar
            CMItem.iterateCount = 0
            CMItem.result = result
            // 1 ???? x map { y => y * 2 } ????
            // 2 ???? x.map(y => y * 2) ????
            val CMRegex1 = (""".*?(\w+ """ + CMItem.name + """ *\{.+?\}).*""").r
            val CMRegex2 = (""".*?(\w+\.""" + CMItem.name + """\(.+?\)).*""").r
            CMItem.exp = now.text match {
              case CMRegex1(e) => e
              case CMRegex2(e) => e
            }

            CallStackItem.state = CollectionMethodStart

          case Array(msg) => // 単純に置き換え
            //    callStack.top.update()
            now.updateNext(command)
            if (now.nextText.startsWith("case ")) {
              if (!CallStackItem.subStep) {
                CallStackItem.mainText = now.text
                ConnectTest.addHistory(now, display = false)
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
          ConnectTest.addHistory(now)
          CallStackItem.state = Fixed
        }

      case Expanding =>
        countMax = 60
        if (count > countMax) {
          count = 0
          callStack.top.updateNext(execQueue.dequeue())
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
          caller.updateNext(execQueue.dequeue())

          CallStackItem.state = Emphasis
        }

      case SubExp =>
        countMax = 60
        if (count > countMax) {
          count = 0
          CallStackItem.state = Emphasis
        }

      case SubEnd =>
        countMax = 60
        if (count > countMax) {
          count = 0
          CallStackItem.subStep = false
          CallStackItem.state = if (now.nextText == MethodExit) Contracting else Emphasis
        }

      case CollectionMethodStart =>
        countMax = 90
        if (count > countMax) {
          count = 0
          execQueue.dequeue() match {
            case CollectionMethodExit =>
              CallStackItem.state = CollectionMethodEnd
              now.updateNext(now.text.replace(CMItem.exp, CMItem.result))
            case x                    =>
              CMItem.stepResult = x
              CallStackItem.state = CollectionMethodStep
          }
        }

      case CollectionMethodStep =>
        countMax = 330
        if (count > countMax) {
          count = 0
          CMItem.iterateCount += 1
          execQueue.dequeue() match {
            case CollectionMethodExit =>
              now.updateNext(now.text.replace(CMItem.exp, CMItem.result))
              CallStackItem.state = CollectionMethodEnd
            case x                    =>
              CMItem.stepResult = x
              CallStackItem.state = CollectionMethodStep
          }
        }

      case CollectionMethodEnd =>
        countMax = 120
        if (count > countMax) {
          count = 0
          CallStackItem.state = MoveDown
        }

      case x =>
        println("match error")
        println(x)
        throw new Exception
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

  def calcOffsetX(item: CallStackItem, trgText: String)(implicit gc: GraphicsContext): Int = {
    val text = item.text.take(item.text.indexOf(trgText)) // TODO この方法だと同じ式で同じメソッドを複数呼んだ場合に詰むかも？
    item.offsetX + textSize(text)._1
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

  def sinMove(start: Int, count: Int, limit: Int, distance: Int): Int = {
    start + ((Math.sin(-Math.PI / 2 + Math.PI / limit * count) + 1) / 2 * distance).toInt
  }


  def pulseLine(text: String, x: Int, y: Int, w: Int = 0, h: Int = 0, padding: Int = 8, frame: Int, fillColor: Color = Color.White, bc: Color = Color.Red, eff1: Effect = null, eff2: Effect = null)(implicit gc: GraphicsContext): Unit = {
    val des = fontMetrics.getDescent.toInt
    val lww = if (frame % 30 < 15) (frame % 30) / 4 + 1 else 4 - (frame % 15) / 4
    rectWithBorder(x - padding / 4, y - h + des - padding / 4 + lww / 2, w + padding / 2, h + padding / 2, lw = lww, borderColor = bc, fillColor = fillColor, eff1 = eff1, eff2 = eff2)
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
      val x = (h._1.x - CallStackItem.DefaultX) / 10 + canvas.getLayoutX.toInt + 5
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

  def printListAndLambda(now: CallStackItem, padding: Int = 8, distance: Int = 100, arrow: Boolean = true, result: Boolean = true)(implicit gc: GraphicsContext): Unit = {
    var sumX = 0
    for (elem <- CMItem.list) {
      val wh = textSize(elem)
      textInBox(elem, now.x + sumX, now.y + distance, wh._1, wh._2, padding)
      sumX += wh._1 + padding / 2
    }
    val arrowStr = if (arrow) "=>" else ""
    val resultStr = if (result) CMItem.lambda.exp else ""
    val lambda = s"%-${CMItem.maxElemLen + 1}s".format(CMItem.lambda.arg) + "%2s".format(arrowStr) + s"%-${CMItem.maxResultLen}s".format(resultStr)
    val lwh = textSize(lambda)
    textInBox(lambda, now.x, now.y + distance * 2, lwh._1 + 10, lwh._2, padding)
  }

  def printProcess(now: CallStackItem, padding: Int = 8, distance: Int = 100)(implicit gc: GraphicsContext): Unit = {
    var sumX = 0
    for (elem <- CMItem.actualResult.take(CMItem.iterateCount)) {
      val wh = textSize(elem)
      textInBox(elem, now.x + sumX, now.y + distance * 3, wh._1, wh._2, padding)
      sumX += wh._1 + padding / 2
    }

  }


}