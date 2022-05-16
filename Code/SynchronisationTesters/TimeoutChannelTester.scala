package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatelessTester}
import synchronisationObject.{
  TimeoutChannelT,TimeoutChannel,FaultyTimeoutChannel}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object TimeoutChannelTester{

  /** Number of worker threads to run.  */
  var p = 4

  /** The number of iterations by each thread. */
  var iters = 10

  /** The maximum value sent. */
  var MaxVal = 20

  var verbose = false

  /** Representation of an operation in the log. */
  trait Op
  case class Send(x: Int) extends Op
  case object Receive extends Op

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. Any n invocations can
    * synchronise, and all should receive the unit value.  */
  def matching: PartialFunction[List[Op], List[Any]] = {
    case List(Send(x)) => List(false)
    case List(Receive) => List(None)
    case List(Send(x), Receive) => List(true, Some(x))
  }

  /** A worker, which calls barrier.sync once. */
  def worker(chan: TimeoutChannelT[Int])(me: Int, log: HistoryLog[Op]) = {
    // Note: the parameters defining the delays seem to give a reasonable mix
    // of successful and unsuccessful invocations.
    for(i <- 0 until iters){
      Thread.sleep(Random.nextInt(2))
      if(Random.nextInt(2) == 0){
        val x = Random.nextInt(MaxVal)
        log(me, chan.sendWithin(x, 1+Random.nextInt(2)), Send(x))
      }
      else log(me, chan.receiveWithin(1+Random.nextInt(2)), Receive)
    }
  }

  var faulty = false

  /** Run a single test. */
  def doTest = {
    val chan: TimeoutChannelT[Int] = 
      if(faulty) new FaultyTimeoutChannel[Int] else new TimeoutChannel[Int]
    val tester = 
      new StatelessTester[Op](worker(chan) _, p, List(1,2), matching, verbose)
    //println(tester())
    if(!tester()) sys.exit()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      // case "--version2" => version2 = true; i += 1
      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1
      // case "--faulty3" => faulty2 = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    def run() = {
      for(i <- 0 until reps){ doTest; if(i%10 == 0) print(".") }
      println()
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
      profiler{ run() }
    }
    else run()
    () 
  }
}
