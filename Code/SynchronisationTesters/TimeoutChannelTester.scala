package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog, StatelessTester}
import synchronisationObject.{
  TimeoutChannelT, TimeoutChannel, FaultyTimeoutChannel}

import ox.gavin.profiling.{Profiler,SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object TimeoutChannelTester extends Tester{

  /** The maximum value sent. */
  var MaxVal = 20

  /** The timeout time with the progress check. */
  var timeout = -1

  var verbose = false

  var countProfile = false

  /** Representation of an operation in the log. */
  trait Op
  case class Send(x: Int) extends Op
  case object Receive extends Op

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object.   */
  def matching: PartialFunction[List[Op], List[Any]] = {
    case List(Send(x)) => List(false)
    case List(Receive) => List(None)
    case List(Send(x), Receive) => List(true, Some(x))
  }

  /** A worker. */
  def worker(chan: TimeoutChannelT[Int])(me: Int, log: HistoryLog[Op]) = {
    val random = new Random()
    for(i <- 0 until iters){
      // Note: the delay below means we get a reasonable mix of successul and
      // unsuccessful invocations.
      Thread.sleep(random.nextInt(1))
      if(random.nextInt(2) == 0){
        val x = random.nextInt(MaxVal)
        def doSend() = chan.sendWithin(x, 1+random.nextInt(1))
        if(countProfile)
          log(me, 
            { val op = doSend(); Profiler.count(s"send $op"); op },
            Send(x))
        else log(me, doSend(), Send(x))
      }
      else{ // receive
        def doRec() = chan.receiveWithin(1+random.nextInt(1))
        if(countProfile)
          log(me, 
            { val op = doRec(); Profiler.count(s"receive ${op.nonEmpty}"); op }, 
            Receive)
        else log(me, doRec(), Receive)
      }
    }
  }

  var faulty = false

  /** Run a single test. */
  def doTest = {
    val chan: TimeoutChannelT[Int] = 
      if(faulty) new FaultyTimeoutChannel[Int] else new TimeoutChannel[Int]
    val tester = new StatelessTester[Op](
      worker(chan), p, List(1,2), matching, verbose = verbose)
    // Following might or might not involve timeouts: default is timeout = -1
    tester(timeout)
    // if(!tester(timeout)) sys.exit()
  }

  def main(args: Array[String]): Unit = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case "--countProfile" => countProfile = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    if(profiling){
      def filter(frame: StackTraceElement) : Boolean =
        SamplingProfiler.defaultFilter(frame) &&
          !frame.getClassName.contains("jdk.internal")
      def printer(data: ArrayBuffer[SamplingProfiler.StackTrace]) = {
        SamplingProfiler.print(filter = filter, length = 20)(data)+"\n"+
        SamplingProfiler.printTree(
          filter = filter, expand = ProfilerSummaryTree.expandToThreshold(0.1)
        )(data)
      }
      val profiler = new SamplingProfiler(interval = interval, print = printer)
      profiler{ runTests(reps, timing) }
    }
    else runTests(reps, timing) 

    if(countProfile) Profiler.report
  }
}
