import com.sun.jdi.ObjectReference
import com.sun.jdi.event.{ExceptionEvent, MethodEntryEvent, MethodExitEvent, StepEvent}
import org.scaladebugger.api.debuggers.LaunchingDebugger
import org.scaladebugger.api.lowlevel.events.EventType._
import org.scaladebugger.api.utils.JDITools
import org.scaladebugger.api.virtualmachines.ScalaVirtualMachine

import scala.io.Source

object LowlevelTest extends App {

  val x = "tte({x => x*n})"

  // Get the executing class name (remove $ from object class name)
  val klass = DebugTarget.getClass
  val className = klass.getName.replaceAllLiterally("$", "")

  // Add our main class to the classpath used to launch the class
  val classpath = JDITools.jvmClassPath
  val jvmOptions = Seq("-classpath", s"F:/Temp/ScalaDebuggerTest/target/scala-2.11/classes;$classpath")
  val launchingDebugger = LaunchingDebugger(
    className = className,
    jvmOptions = jvmOptions,
    suspend = true // Wait to start the main class until after connected
  )

  val file = Source.fromFile("F:/Temp/ScalaDebuggerTest/src/main/scala/DebugTarget.scala").getLines().toList

  val listClass = "scala.collection.immutable.List"
  val functionN = "scala.Function"
  val coverListMethods = Set("map", "flatMap", "filter")

  var listExpwNow = false

  def resisterMethodEntryEvent(s: ScalaVirtualMachine, filter: String) {
    val entryReq = s.underlyingVirtualMachine.eventRequestManager().createMethodEntryRequest()
    entryReq.addClassFilter(filter)
    entryReq.enable()
  }

  def resisterMethodExitEvent(s: ScalaVirtualMachine, filter: String) {
    val exitReq = s.underlyingVirtualMachine.eventRequestManager().createMethodExitRequest()
    exitReq.addClassFilter(filter)
    exitReq.enable()
  }

  launchingDebugger.start { s =>
    println("Launched and connected to JVM: " + s.uniqueId)

    val cfileName = JDITools.scalaClassStringToFileString(className)
    val clineNumber = 7
    s.lowlevel.breakpointManager.createBreakpointRequest(cfileName, clineNumber)

    s.onUnsafeBreakpoint(cfileName, clineNumber).foreach(be => {
      s.lowlevel.breakpointManager.removeBreakpointRequest(cfileName, clineNumber)

      resisterMethodEntryEvent(s, "DebugTarget*")
      resisterMethodEntryEvent(s, s"$listClass*")
      //   resisterMethodEntryEvent(s,s"$functionN*")

      resisterMethodExitEvent(s, "DebugTarget*")
      resisterMethodExitEvent(s, s"$listClass*")
      //  resisterMethodExitEvent(s,s"$functionN*")


      val path = be.location().sourcePath()
      val line = be.location().lineNumber()
      print(s"Break:\t$path:$line \tSource:${file(line - 1)}")
      println("\t" + be.location().method())
      s.stepIntoLine(be.thread())

    })

    s.onUnsafeEvent(ExceptionEventType).foreach { ee =>
      val exceptionEvent = ee.asInstanceOf[ExceptionEvent]
      println("!!" + exceptionEvent.catchLocation())
    }

    /////////////////////////////////////////////////////
    /////////////////////////////////////////////////////
    s.onUnsafeEvent(MethodEntryEventType).foreach { mee =>
      val methodEntry = mee.asInstanceOf[MethodEntryEvent]
      val fullMethod = methodEntry.method.toString
      val methodName = methodEntry.method.name
      val caller = fullMethod.split('(').head
      if (caller.contains(listClass)) {
        if (coverListMethods.contains(methodName)) {
          print("MEntry:")
          println(s"\t${methodEntry.method()}")

          searchAnonfunVariableName(methodEntry) match {
            case Some(zzz) => println(zzz)
            case None => println("no use variable")
          }
          println("\t" + makeList(methodEntry.thread().frame(0).thisObject()))
          listExpwNow = true
          // ここでListを文字列で渡して、描画側ではコンマ区切りで要素を取り出して、ラムダ式ごとに要素に適用してって、exitの結果をあてはめてって、最後にmap系のexitの結果を答えにして戻す
        }
      } else if (!fullMethod.contains("anonfun")) {
        print("MEntry:")
        println(s"\t${methodEntry.method()}")
      } else if (fullMethod.contains("anonfun") && fullMethod.contains("apply")) {
        print(s"MEntry:Anonfun:${methodEntry.location().lineNumber()}\t")
        if (listExpwNow) {
          val lambdaExpSource = file.drop(methodEntry.location().lineNumber() - 1).mkString("") // 複数行のラムダに対応
          //val lambdaExpSource = file(methodEntry.location().lineNumber() - 1)
          val lambdaRegex1 = """.*?\w+? \w+? \{(.*?)=>(.*?)\}.*""".r
          val lambdaRegex2 = """.*? \w\.\((.*?)=>(.*?)\).*""".r
          val lambdaRegex3 = """.*?\{(.*?)=>(.*?)\}.*""".r
          lambdaExpSource match {
            case lambdaRegex1(arg, exp) => println(s"1$arg => $exp")
            case lambdaRegex2(arg, exp) => println(s"2$arg => $exp")
            case lambdaRegex3(arg, exp) => println(s"3$arg => $exp")
            case _ => println("err", lambdaExpSource)
          }
        } else {
          println(searchAnonfunVariableName(methodEntry))
          println(s"\t${methodEntry.method()}")
        }


        // println(s"\t$methodEntry")
        //  println(s"\t${methodEntry.method()}")

        //println(methodEntry.method().variablesByName("x"))
        //        println(s"\t${methodEntry.thread().frames().size()}")
        // println(methodEntry.method(), methodEntry.location())
      }

    }
    /////////////////////////////////////////////////////
    /////////////////////////////////////////////////////
    s.onUnsafeEvent(MethodExitEventType).foreach { mee =>
      val methodExit = mee.asInstanceOf[MethodExitEvent]
      var value: Any = ""
      val fullMethod = methodExit.method.toString
      val methodName = methodExit.method.name
      val caller = fullMethod.split('(').head

      value = methodExit.returnValue() match {
        case or: ObjectReference =>
          if (isList(or)) {
            makeList(or)
          } else {
            //   println(or.getValues(or.referenceType().allFields()))
            or
          }

        case v => v
      }

      if (caller.contains(listClass)) {
        if (coverListMethods.contains(methodName)) {
          print("MExit:")
          println(s"\t${methodExit.method()}")
          println(s"\tReturn:\t$value")
          listExpwNow = false
        }
      } else if (!fullMethod.contains("anonfun")) {
        print("MExit:")
        println(s"\t${methodExit.method()}")
        println(s"\tReturn:\t$value")
      }else if (fullMethod.contains("anonfun") && fullMethod.contains("apply")) {
        print("MExit:Lambda")
        println(s"\tReturn:\t$value")
        if(listExpwNow){

        }
      }

      //          println("\t" + methodExit.thread().frame(0).visibleVariables())
      if (methodExit.method().toString.contains("main(")) {
        // end ////////////
        launchingDebugger.stop()
      }
    }

    /////////////////////////////////////////////////////
    /////////////////////////////////////////////////////
    s.onUnsafeEvent(StepEventType).foreach { pp =>
      val stepEvent = pp.asInstanceOf[StepEvent]
      val fileName = stepEvent.location().sourcePath()
      val lineNumber = stepEvent.location().lineNumber()
      val method = stepEvent.location().method().toString
      if (method.contains("main(")) {
        printStep(fileName, lineNumber, stepEvent)
        s.stepIntoLine(stepEvent.thread())
      } else if (method.contains("sliceRecursive")) {
        printStep(fileName, lineNumber, stepEvent)
        s.stepIntoLine(stepEvent.thread())
      } else if (method.contains("List.")) {
//        println("//////////" + stepEvent.location().method().toString)
//        println("\t" + makeList(stepEvent.thread().frame(1).getValue(stepEvent.thread().frame(1).visibleVariableByName("list")).asInstanceOf[ObjectReference]))
//        println("\t" + stepEvent.thread().frame(1).visibleVariables())
//        println("\t" + stepEvent.thread().frame(0).visibleVariables())

        s.stepOutLine(stepEvent.thread())
//      } else if(method.contains("main")){
//        printStep(fileName, lineNumber, stepEvent)
//        s.stepIntoLine(stepEvent.thread())
      } else {
        print(s"Step: $fileName:$lineNumber \t Source:${file(lineNumber - 1)}")
        println("\t" + method + " , " + stepEvent)
        s.stepOutLine(stepEvent.thread())
      }

    }
  }

  var be = ""
  def printStep(fileName: String, lineNum: Int, stepEvent: StepEvent): Unit = {
    // if (be == s"Step:\t$fileName:$lineNum \tSource:${file(lineNum - 1)}") return
    println(s"Step:\t$fileName:$lineNum \tSource:${file(lineNum - 1)}")
    be = s"Step:\t$fileName:$lineNum \tSource:${file(lineNum - 1)}"

    //  println("\t" + stepEvent.location().method())
  }

  def isList(or: ObjectReference): Boolean =
    or.referenceType().fieldByName("head") != null

  def makeList(or: ObjectReference): List[_] = {
    val fields = or.getValues(or.referenceType().allFields())
    val head = or.referenceType().fieldByName("head")
    val tail = or.referenceType().fieldByName("tl")
    (fields.get(head), fields.get(tail)) match {
      case (h, t: ObjectReference) => h :: makeList(t)
      case _ => Nil
    }
  }

  def searchAnonfunVariableName(methodEntry:MethodEntryEvent):Option[String] = {
    import collection.JavaConversions._
    val functionVarNames = methodEntry.thread().frame(1).visibleVariables().filter(_.typeName().contains(".Function")).map(_.name())
    val applyLine = file(methodEntry.thread().frame(1).location().lineNumber() - 1)
    functionVarNames.find(fv => applyLine.contains(s" $fv") || applyLine.contains(s".$fv"))
  }

}
