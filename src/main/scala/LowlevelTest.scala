import com.sun.jdi.event.{ExceptionEvent, MethodEntryEvent, MethodExitEvent, StepEvent}
import com.sun.jdi.{ObjectReference, Type}
import org.scaladebugger.api.debuggers.LaunchingDebugger
import org.scaladebugger.api.lowlevel.events.EventType._
import org.scaladebugger.api.utils.JDITools

import scala.io.Source

/**
  * Created by slab on 2016/08/22.
  */
object LowlevelTest extends App {
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

  launchingDebugger.start { s =>
    println("Launched and connected to JVM: " + s.uniqueId)

    val cfileName = JDITools.scalaClassStringToFileString(className)
    val clineNumber = 7
    s.lowlevel.breakpointManager.createBreakpointRequest(cfileName, clineNumber)

    s.onUnsafeBreakpoint(cfileName, clineNumber).foreach(be => {
      s.lowlevel.breakpointManager.removeBreakpointRequest(cfileName, clineNumber)

      val entryReq1 = s.underlyingVirtualMachine.eventRequestManager().createMethodEntryRequest()
      entryReq1.addClassFilter("DebugTarget*")
      entryReq1.enable()

      val entryReq2 = s.underlyingVirtualMachine.eventRequestManager().createMethodEntryRequest()
      entryReq2.addClassFilter("scala.collection.immutable.List*")
      entryReq2.enable()

      val exitReq1 = s.underlyingVirtualMachine.eventRequestManager().createMethodExitRequest()
      exitReq1.addClassFilter("DebugTarget*")
      exitReq1.enable()

      val exitReq2 = s.underlyingVirtualMachine.eventRequestManager().createMethodExitRequest()
      exitReq2.addClassFilter("scala.collection.immutable.List*")
      exitReq2.enable()

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
      print("MEntry:")
      println(s"\t${methodEntry.method()}")
      //println(methodEntry.method().variablesByName("x"))
//        println(s"\t${methodEntry.thread().frames().size()}")
      // println(methodEntry.method(), methodEntry.location())
    }



    /////////////////////////////////////////////////////
    /////////////////////////////////////////////////////
    s.onUnsafeEvent(MethodExitEventType).foreach { mee =>
      val methodExit = mee.asInstanceOf[MethodExitEvent]
      var value: Any = ""

      methodExit.returnValue() match {
        case or: ObjectReference =>
          if (isListType(methodExit.method().returnType())) {
            value = makeList(or)
          }
        case v => value = v
      }

      print("MExit:")
      println(s"\t${methodExit.method()}")
      println(s"\tReturn:\t$value")
//    println("\t" + methodExit.thread().frame(0).visibleVariables())
      if (methodExit.method().toString.contains("main(")) {
        launchingDebugger.stop()
      }
    }

    /////////////////////////////////////////////////////
    /////////////////////////////////////////////////////
    s.onUnsafeEvent(StepEventType).foreach { pp =>
      val stepEvent = pp.asInstanceOf[StepEvent]
      val fileName = stepEvent.location().sourcePath()
      val lineNumber = stepEvent.location().lineNumber()
      if (stepEvent.location().method().toString.contains("main(")) {
        printStep(fileName, lineNumber, stepEvent)
        s.stepIntoLine(stepEvent.thread())
      } else if (stepEvent.location().method().toString.contains("sliceRecursive")) {
        printStep(fileName, lineNumber, stepEvent)
        s.stepIntoLine(stepEvent.thread())
      } else if (stepEvent.location().method().toString.contains("List.")) {
        println("//////////" + stepEvent.location().method().toString)

        println("\t" + makeList(stepEvent.thread().frame(1).getValue(stepEvent.thread().frame(1).visibleVariableByName("list")).asInstanceOf[ObjectReference]))
        println("\t" + stepEvent.thread().frame(1).visibleVariables())
        println("\t" + stepEvent.thread().frame(0).visibleVariables())
        println("\t" + makeList(stepEvent.thread().frame(0).thisObject()))
        // println("\t" + stepEvent.thread().frame(0).getValue(stepEvent.thread().frame(0).visibleVariableByName("bf")))
        s.stepOutLine(stepEvent.thread())
      } else {
        //print(s"Step: $fileName:$lineNumber \t Source:${file(lineNumber - 1)}")
        //println("\t" + stepEvent.location().method() + " , " + stepEvent)
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

  def isListType(tpe: Type): Boolean = tpe.name.contains("scala.collection.immutable.List")


  def makeList(or: ObjectReference): List[_] = {
    val fields = or.getValues(or.referenceType().allFields())
    val head = or.referenceType().fieldByName("head")
    val tail = or.referenceType().fieldByName("tl")
    (fields.get(head), fields.get(tail)) match {
      case (h, t: ObjectReference) => h :: makeList(t)
      case _ => Nil
    }
  }
}
