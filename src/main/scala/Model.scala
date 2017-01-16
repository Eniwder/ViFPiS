import java.util

import com.sun.jdi._
import com.sun.jdi.event._
import org.scaladebugger.api.debuggers.LaunchingDebugger
import org.scaladebugger.api.lowlevel.events.EventType._
import org.scaladebugger.api.utils.JDITools
import org.scaladebugger.api.virtualmachines.ScalaVirtualMachine

import scala.collection.mutable
import scala.io.Source

object Model extends App {

  val BPNum = 13
  // 4
  val klass = Class.forName("Ex4")
  // Sample
  val Path = "F:/Temp/ScalaDebuggerTest"

  // Get the executing class name (remove $ from object class name)
  val className = klass.getName.replaceAllLiterally("$", "")

  // Add our main class to the classpath used to launch the class
  val classpath = JDITools.jvmClassPath
  val jvmOptions = Seq("-classpath", s"$Path/target/scala-2.11/classes;$classpath")
  val launchingDebugger = LaunchingDebugger(
    className = className,
    jvmOptions = jvmOptions,
    suspend = true // Wait to start the main class until after connected
  )
  val file = Source.fromFile(s"$Path/src/main/scala/$className.scala").getLines().toList

  val listClass = "scala.collection.immutable.List"
  val functionN = "scala.Function"
  val coverListMethods = Set("map", "flatMap", "filter")

  private var collectionExpNow = false
  private var collectionMethodInit = false
  private var CMList = ""
  private var CMName = ""
  private var CMFName = ""
  private var CMLine = 0

  private var AnonLambda = new scala.collection.mutable.Stack[String]
  private var AnonBe = new scala.collection.mutable.Stack[String]
  private val argsMap = mutable.Map[com.sun.jdi.Method, Map[String, String]]()
  private var anonSearchTime = false
  private var anonBe = ""
  private var anonFrameDepth = 1


  private var methodExited = false
  // ダメな奴だけど暫定
  val NoMatch = 0
  val InMatch = 1
  val MatchOut = 2
  val MatchEnd = 3
  private val matchEnter = Array.fill(1000)(NoMatch) // とりあえず適当に1000 0:default 1:In Match 2:other step

  // とりあえず適当に1000
  private val be = Array.fill(1000)("")
  private val beLine = Array.fill(1000)("")
  private val beArgs = Array.fill(1000)("")
  private val ifMap = Array.fill(1000)(scala.collection.mutable.Map.empty[Int, Boolean])

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

    View.loadSourceCode(file)

    val cfileName = JDITools.scalaClassStringToFileString(className)
    val clineNumber = BPNum
    s.lowlevel.breakpointManager.createBreakpointRequest(cfileName, clineNumber)

    s.onUnsafeBreakpoint(cfileName, clineNumber).foreach { be =>
      val breakpointEvent = be
      s.lowlevel.breakpointManager.removeBreakpointRequest(cfileName, clineNumber)

      resisterMethodEntryEvent(s, s"$className*")
      resisterMethodEntryEvent(s, s"$listClass*")

      resisterMethodExitEvent(s, s"$className*")
      resisterMethodExitEvent(s, s"$listClass*")

      val path = be.location().sourcePath()
      val lineNum = be.location().lineNumber()
      val line = file(lineNum - 1)
      val execLine = extractExecLine(line).trim
      print(s"Break:\t$path:$lineNum \tSource:$line")
      println("\t" + be.location().method())
      printStep(cfileName, lineNum, execLine, breakpointEvent.thread().frameCount(), breakpointEvent)
      s.stepIntoLine(be.thread())

    }

    s.onUnsafeEvent(ExceptionEventType).foreach { ee =>
      val exceptionEvent = ee.asInstanceOf[ExceptionEvent]
      println("!!" + exceptionEvent.catchLocation())
    }

    s.onUnsafeEvent(MethodEntryEventType).foreach { mee =>

      val methodEntry = mee.asInstanceOf[MethodEntryEvent]
      val fullMethod = methodEntry.method.toString
      val methodNameWithDollar = methodEntry.method.name
      val methodName = if (methodNameWithDollar.endsWith("$1")) methodNameWithDollar.dropRight(2).split('$').last
      else methodNameWithDollar.split('$').last
      val caller = fullMethod.split('(').head
      if (collectionExpNow && !collectionMethodInit) {
        // noop
      } else if (caller.contains(listClass)) {
        if (coverListMethods.contains(methodNameWithDollar)) {
          print("MEntry:")
          println(s"\t${methodEntry.method()}")

          CMFName = searchAnonfunVariableName(methodEntry) match {
            case Some(zzz) => println(zzz)
              zzz
            case None => println("no use variable")
              ""
          }
          collectionExpNow = true
          collectionMethodInit = true
          // ここでListを文字列で渡して、描画側ではコンマ区切りで要素を取り出して、ラムダ式ごとに要素に適用してって、exitの結果をあてはめてって、最後にmap系のexitの結果を答えにして戻す
          println("Enter Collection Method")
          println("\t" + getValue(methodEntry.thread().frame(0).thisObject(), "List()"))
          CMList = getValue(methodEntry.thread().frame(0).thisObject(), "List()").toString
          CMName = methodNameWithDollar
          CMLine = methodEntry.thread.frame(1).location().lineNumber()
        }
      } else if (!fullMethod.contains("anonfun")) {
        print("MEntry:")
        println(s"\t${methodEntry.method()}")

        val be = beLine(methodEntry.thread().frameCount - 1)
        if (hasArgMethod(be, methodName)) {
          val argMap = getArgValueMap(methodEntry.method().arguments(), be, methodName)
          argsMap.put(methodEntry.method(), argMap)
        }

        //    println("\t" + methodName)
        View.addCallStack(methodName, methodEntry.method.location.lineNumber)
      } else if (fullMethod.contains("anonfun") && !fullMethod.contains("<init>")) {
        //print(s"MEntry:Anonfun:${methodEntry.location().lineNumber()}\t")
        // 複数行のラムダに対応
        val lambdaExpSource = file.drop(methodEntry.location().lineNumber() - 1).mkString("")
        val lambdaExpLine = file(methodEntry.location().lineNumber() - 1)
        // 一行限定
        val lambda = getLambda(lambdaExpLine)
        if (collectionExpNow) {
          if (collectionMethodInit && fullMethod.contains("apply")) {
            collectionMethodInit = false
            // 暫定pushで対応、関数の変数を中身に置き換える
            if (CMFName != "") {
              val line = beLine(methodEntry.thread().frameCount - 2)
              val lambdaString = s"{ ${lambda.arg} => ${lambda.exp} }"
              val rpAnonVar = line.replace(s" $CMFName", s" $lambdaString").replace(s"($CMFName)", s"($lambdaString)")
              View.push(rpAnonVar, CMLine)
              println("Step:\t" + rpAnonVar)
            }
            View.enterCollectionMethod(CMList, CMName, lambda, CMFName, CMLine)
          }
        } else {
          // MethodExitに対応する高階関数の置き換え履歴をここで追加
          // AnonBe.push(rpRpAnonVar)
          // AnonLambda.push(rpLambdaString)

          searchAnonfunVariableName(methodEntry) match {
            case Some(afvn) =>
              val _line = file(methodEntry.thread().frame(1).location().lineNumber() - 1)
              val line = replaceVariable2Value(_line, methodEntry.thread().frame(1))
              val lambdaString = s"{ ${lambda.arg} => ${lambda.exp} }"
              println(s"MEntry:$afvn:${methodEntry.location().lineNumber()}")
              val rpLambdaString = replaceVariable2Value(lambdaString, methodEntry.thread().frame(0))
              val rpRpAnonVar = line.replaceFirst(afvn + """\(.+?\)""", rpLambdaString).trim
              // View.push(rpRpAnonVar, lineN)
              // println("StepRT:\t" + rpRpAnonVar)
              AnonBe.push(rpRpAnonVar)
              AnonLambda.push(rpLambdaString)
            case None =>
            // 何もしなくてよさそう？
          }
        }
      } else {
        //  noop
      }


      // println(s"\t$methodEntry")
      //  println(s"\t${methodEntry.method()}")

      //println(methodEntry.method().variablesByName("x"))
      //        println(s"\t${methodEntry.thread().frames().size()}")
      // println(methodEntry.method(), methodEntry.location())

    }

    s.onUnsafeEvent(MethodExitEventType).foreach { mee =>
      val methodExit = mee.asInstanceOf[MethodExitEvent]
      var value: Any = ""
      val fullMethod = methodExit.method.toString
      val line = methodExit.method.location.lineNumber
      //      val methodName = methodExit.method.name
      //      val caller = fullMethod.split('(').head

      if (methodExit.method().toString.contains("main(")) {
        // end ////////////
        println("Debugger Stop")
        launchingDebugger.stop()
        View.end()
        View.main(Array())
      }

      value = getValue(methodExit.returnValue(), methodExit.returnValue.toString)
      value = if(value == Nil) "Nil" else value
      if (fullMethod.contains("<init>")) {
        // noop
      } else if (collectionExpNow && fullMethod.contains("apply") && !fullMethod.contains("List") && methodExit.method().returnTypeName().contains("java.lang.Object")) {
        println(s"\tStepResult:\t$value")
        View.pushCMResult(value.toString)
        //        println(s"\tStepResult:\t${methodExit.method()}")
      } else if (collectionExpNow && coverListMethods.contains(methodExit.method().name())) {
        println(s"Exit CollectionMethod:")
        println(s"\tReturn:\t$value")
        View.exitCollectionMethod(value.toString)
        collectionExpNow = false
      } else if (collectionExpNow) {
        // noop
      } else if (fullMethod.contains(listClass)) {
        val methodNameWithoutArg = fullMethod.split('(') toList match {
          case a :: b :: t => a
          case a :: nil => a
          case _ => ""
        }
        if (!methodNameWithoutArg.contains(listClass)) {
          print("MExit:")
          println(s"\t${methodExit.method()}")
          println(s"\tReturn:\t$value")
          View.popCallStack(value.toString, line)
        }
      } else if (!fullMethod.contains("anonfun")) {
        methodExited = true
        print("MExit:")
        println(s"\t${methodExit.method()}")
        println(s"\tReturn:\t$value")

        View.popCallStack(value.toString, line)

      } else if (fullMethod.contains("anonfun") && fullMethod.contains("apply") && !fullMethod.contains("<init>")) {
        searchAnonfunVariableName(methodExit) match {
          case Some(avn) =>
            print("MExit:Anon")
            println("\t" + avn)
            println(s"\tReturn:\t$value")
            //View.popCallStack(value.toString, methodExit.thread().frame(1).location().lineNumber)
            val be = AnonBe.pop()
            val la = AnonLambda.pop()
            val rpText = be.replaceAllLiterally(la, value.toString)
            View.push(rpText, methodExit.thread().frame(1).location().lineNumber)
          case None =>
        }
      }

    }

    s.onUnsafeEvent(StepEventType).foreach { pp =>
      val stepEvent = pp.asInstanceOf[StepEvent]
      val fileName = stepEvent.location().sourcePath()
      val lineNumber = stepEvent.location().lineNumber()
      val frameName = stepEvent.location().method().toString
      val frameCount = stepEvent.thread().frameCount()
      if (frameName.contains("main(")) {
        printStep(fileName, lineNumber, extractExecLine(file(lineNumber - 1)), frameCount, stepEvent)
        s.stepIntoLine(stepEvent.thread())
      } else if (frameName.contains(className) && !frameName.contains("<init>")) {
        // 暫定的に、メソッド宣言行や変数宣言行の場合は右辺のみを見る
        val execLine = extractExecLine(file(lineNumber - 1)).trim
        val line = if (execLine.contains("case")) execLine.replace("=>", "") else execLine
        val bl = beLine(frameCount)
        // TODO 高階関数がクラス中の関数宣言場所を参照するのを防ぎたいが、これでいいのか不明
        if (!stepEvent.location().toString.contains("main$")
          && !stepEvent.location().toString.contains("anonfun$")
          //  && !methodExited
          && !(matchEnter(frameCount) >= InMatch && execLine.contains("match"))
          && !(matchEnter(frameCount) == NoMatch && execLine.contains("case")) // 末尾再帰が最適化されるとこうなる？
        ) {
          printStep(fileName, lineNumber, line, frameCount, stepEvent)
          methodExited = false
          anonSearchTime = false
          anonBe = ""
          anonFrameDepth = 1
        } else if(stepEvent.location().toString.contains("anonfun$")){
          (anonFrameDepth until stepEvent.thread().frameCount()).find((n: Int) => searchAnonfunVariableName(stepEvent, n) nonEmpty) match {
            case Some(fc) =>
              anonFrameDepth = fc
              val afvn = searchAnonfunVariableName(stepEvent, fc).get
              val _line =
                if (anonSearchTime) {
                  anonBe
                } else {
                  anonSearchTime = true
                  file(stepEvent.thread().frame(1).location().lineNumber() - 1)
                }
              val line = replaceVariable2Value(_line, stepEvent.thread().frame(1))
              val lambda = getLambda(execLine)
              val defoLS = s"{ ${lambda.arg} => ${lambda.exp} }"
              val lambdaString = argsMap.get(stepEvent.thread().frame(fc).location().method()) match {
                case Some(argMap) => argMap.getOrElse(afvn, defoLS)
                case None => defoLS
              }
              println(s"StepAnon:$afvn:${stepEvent.location().lineNumber()}")
              println("\t" + line)
              println("\t" + lambdaString)
              val rpAnonVar = line.replaceFirst(afvn + """\(.+?\)""", lambdaString).trim
              println("Step:\t" + rpAnonVar)
              val lineN = stepEvent.thread().frame(1).location().lineNumber()
              View.push(rpAnonVar, lineN)
              val rpLambdaString = replaceVariable2Value(lambdaString, stepEvent.thread().frame(0))
              val rpRpAnonVar = line.replaceFirst(afvn + """\(.+?\)""", rpLambdaString).trim
              View.push(rpRpAnonVar, lineN)
              println("StepRT:\t" + rpRpAnonVar)
              anonBe = rpRpAnonVar
              // MethodExitと対応が取れないので、ここでは履歴に追加しない
              // AnonBe.push(rpRpAnonVar)
              // AnonLambda.push(rpLambdaString)
            case None =>
              // noop
              println("@@@@@@@@@@@", stepEvent.location, line)
          }
        }
        if (!stepEvent.location().toString.contains("main$")) {
          matchEnter(frameCount) =
            if (matchEnter(frameCount) == MatchOut && execLine.contains("match")) NoMatch
            else if (matchEnter(frameCount) == NoMatch && execLine.contains("match")) InMatch
            else if (matchEnter(frameCount) == InMatch && (execLine.contains("match") || execLine.contains("case"))) InMatch
            else if (matchEnter(frameCount) == MatchEnd) MatchEnd
            else if (matchEnter(frameCount) == NoMatch) NoMatch
            else MatchOut
        }
        // TODO match caseのネストに非対応
        if (matchEnter(frameCount) == MatchOut) {
          val beRpText = replaceVariable2Value(bl, stepEvent.thread().frame(0))
          if (bl != beRpText) {
            println("StepPOB:\t" + beRpText)
            View.pushOneBefore(beRpText, View.KeepLine)
          }
          val rpText = replaceVariable2Value(line, stepEvent.thread().frame(0))
          if (line != rpText) {
            println("StepRT:\t" + rpText)
            View.push(rpText, lineNumber)
          }
          matchEnter(frameCount) = MatchEnd
        }
        import collection.JavaConversions._
        val nowArgs = s"${stepEvent.thread().frameCount()}@@@${stepEvent.location().method()}@@@${stepEvent.location().method().arguments().map(stepEvent.thread().frame(0).getValue).mkString("")}"
        // メソッドのインライン化が行われた？
        val bearg = beArgs(frameCount).split("@@@")
        if (bearg.length >= 3) {
          val nargs = nowArgs.split("@@@")
          if (bearg(0) == nargs(0) && bearg(1) == nargs(1) && bearg(2) != nargs(2)) {
            matchEnter(stepEvent.thread().frameCount()) = 0
          }
        }
        beArgs(frameCount) = nowArgs

        s.stepIntoLine(stepEvent.thread())
      }
      else if (frameName.contains("List.")) {
        //        println("//////////" + stepEvent.location().method().toString)
        //
        //        println("\t" + makeList(stepEvent.thread().frame(1).getValue(stepEvent.thread().frame(1).visibleVariableByName("list")).asInstanceOf[ObjectReference]))
        //        println("\t" + stepEvent.thread().frame(1).visibleVariables())
        //        println("\t" + stepEvent.thread().frame(0).visibleVariables())
        //        println("\t" + makeList(stepEvent.thread().frame(0).thisObject()))
        // println("\t" + stepEvent.thread().frame(0).getValue(stepEvent.thread().frame(0).visibleVariableByName("bf")))
        s.stepOutLine(stepEvent.thread())
      } else {
        //print(s"Step: $fileName:$lineNumber \t Source:${file(lineNumber - 1)}")
        //println("\t" + stepEvent.location().method() + " , " + stepEvent)
        s.stepOutLine(stepEvent.thread())
      }

    }
  }

  def printStep(fileName: String, lineNum: Int, line: String, frameCount: Int, stepEvent: com.sun.jdi.event.LocatableEvent): Unit = {
    if (be(frameCount) == s"Step:\t$fileName:$lineNum \tSource:$line") return
    if (ifMap(frameCount).getOrElse(lineNum, false)) {
      ifMap(frameCount).put(lineNum, false)
      return
    }
    println(s"Step:\t$fileName:$lineNum \tSource:\t$line")
    be(frameCount) = s"Step:\t$fileName:$lineNum \tSource:$line"
    View.push(line, lineNum)
    beLine(frameCount) = line
    if (line.contains("if (")) {
      println(line)
      ifMap(frameCount).put(lineNum, true)
    }
    val rpText = replaceVariable2Value(line, stepEvent.thread().frame(0))
    if (!line.contains("case") && line != rpText && matchEnter(frameCount) != InMatch) {
      println("StepRT:\t" + rpText)
      View.push(rpText, lineNum)
      beLine(frameCount) = rpText
    }
    //  println("\t" + stepEvent.location().method())
  }

  def isList(or: ObjectReference): Boolean =
    or.referenceType().fieldByName("head") != null ||
      or.referenceType().name == "scala.collection.immutable.Nil$"

  def makeList(or: ObjectReference): List[_] = {
    val fields = or.getValues(or.referenceType().allFields())
    val head = or.referenceType().fieldByName("head")
    val tail = or.referenceType().fieldByName("tl")
    //   println(fields)
    (fields.get(head), fields.get(tail)) match {
      case (h, t: ObjectReference) => h :: makeList(t)
      case _ => Nil
    }
  }

  def searchAnonfunVariableName(methodEntry: com.sun.jdi.event.LocatableEvent, n: Int = 1): Option[String] = {
    import collection.JavaConversions._
    val functionVarNames = methodEntry.thread().frame(n).visibleVariables().filter(_.typeName().contains(".Function")).map(_.name())
    val applyLine = file(methodEntry.thread().frame(1).location().lineNumber() - 1)
    val removeCommentLine = applyLine.replaceAll( """\/\*.*\*\/""", "")
    functionVarNames.find(fv => removeCommentLine.contains(s" $fv") || removeCommentLine.contains(s"($fv)"))
  }

  def extractExecLine(line: String): String = {
    val execLine = if (line.contains("def")) {
      // def x():X = {
      // def x() =
      if (line.replaceAll("==|<=|>=|=>", "") contains ("=")) {
        line.split("[^<>=]=[^<>=]", 2).tail.mkString("")
      } else {
        // def x(){
        line.split("\\{", 2).tail.mkString("")
      }
    } else if (line.contains("val") || line.contains("var")) {
      // declaration = xxx
      line.split("=", 2).tail.mkString("")
    } else {
      line
    }
    execLine.trim
  }

  def replaceVariable2Value(text: String, sf: StackFrame): String = {
    import collection.JavaConversions._
    //println(sf.visibleVariables().map(v => (getValue(sf.getValue(v),v.name()),v.name())).mkString("..."))
    sf.visibleVariables().foldLeft(text) { (z, v) =>
      z.replaceAll(s"([^0-9a-zA-Z]|^)${v.name}([^0-9a-zA-Z]|$$)", "$1" + getValue(sf.getValue(v), v.name).toString + "$2")
    }
  }

  def makeListDeep(list: List[Any]): List[_] = {
    list.collect {
      case sl: ObjectReference => getValue(sl, sl.toString)
      case x => x
    }
  }

  def getLambda(line: String): Lambda = {
    val lambdaRegex1 =
      """.*? \((\w+?).*?\).*?=>(.+?)""".r
    // xxx = (x:Int) => x*2
    val lambdaRegex2 =
      """.*?\w+? *?= *?\{(.*?)=>(.*?)\}.*?""".r
    // xxx = { (x:Int) => x * 2}
    val lambdaRegex3 =
      """.*?\{(.*?)=>(.*?)\}.*?""".r
    // { x => x*2 }
    line match {
      case lambdaRegex1(arg, exp) => Lambda(arg.trim, exp.trim)
      case _ => line match {
        case lambdaRegex2(arg, exp) => Lambda(arg.trim, exp.trim)
        case lambdaRegex3(arg, exp) => Lambda(arg.trim, exp.trim)
        case _ => Lambda("lambda serach regex match err", line)
      }
    }
  }

  def hasArgMethod(line: String, mname: String): Boolean = line.contains(s"$mname(") && !line.contains(s"$mname()")

  // ViewのextractMethodTextと似てる
  def getArgValueMap(variables: util.List[LocalVariable], be: String, mname: String) = {
    val startIndex = be.indexOf(mname)
    val methodStartText = be.drop(startIndex + mname.length)
    val endIndex = View.searchMethodEnd(methodStartText)
    val methodCall = methodStartText.slice(1, endIndex - 1)
    var braceCount = 0
    var buffer = ""
    var resultBuff = mutable.Buffer.empty[String]
    methodCall.foreach { c =>
      if (c == ',' && braceCount == 0) {
        resultBuff += buffer.trim
        buffer = ""
      } else {
        buffer += c
      }
      if (c == '(') {
        braceCount += 1
      } else if (c == ')') {
        braceCount -= 1
      }
    }
    resultBuff += buffer.trim
    import collection.JavaConversions._
    variables.map(_.name()).zip(resultBuff).toMap
  }


  def getValue(value: com.sun.jdi.Value, name: String): Any = {
    val ret = value match {
      case v: BooleanValue => v.value()
      case v: ByteValue => v.value()
      case v: CharValue => "'" + v.value() + "'"
      case v: DoubleValue => v.value()
      case v: FloatValue => v.value()
      case v: IntegerValue => v.value()
      case v: LongValue => v.value()
      case v: ShortValue => v.value()
      case v: ArrayReference => v // TODO
      case v: StringReference => "\"" + v.value() + "\""
      case v: ThreadReference => v // 未対応
      case v: ThreadGroupReference => v // 未対応
      case v: ClassObjectReference => v // 未対応()
      case v: ClassLoaderReference => v // 未対応()
      case v: ObjectReference if isList(v) => makeListDeep(makeList(v)) // 現状オブジェクトはListのみ対応とする
      case v: ObjectReference if v.referenceType().fieldByName("value") != null =>
        getValue(v.getValue(v.referenceType().fieldByName("value")), name)
      case v: ObjectReference => name         // 未対応のオブジェクトの場合は申し訳ないが名前のままにする
      case v: VoidValue => "void"
      case null => null
      case v => v // 多分こない
    }
    // また、パターンマッチなどで使わない変数が最適化された場合、nullとなるのでそれも防ぐ
    // 基本的にScalaの場合nullはありえないとする
    if (ret == null) name else ret
  }


}
