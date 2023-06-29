package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog, StatelessTester}
import synchronisationObject.{
  TimeoutExchangerT, TimeoutExchanger, FaultyTimeoutExchanger, 
  FaultyTimeoutExchanger2}
import ox.gavin.profiling.{Profiler,SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object TimeoutExchangerTester extends Tester{

  /** The maximum value sent. */
  var MaxVal = 100

  /** The timeout time with the progress check. */
  var timeout = -1

  /** Are we running the counting profiler (to profile number of successful or
    * unsuccessful invocations)? */
  var countProfile = false

  /** Representation of an operation. */
  case class Exchange(x: Int)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object.   */
  def matching: PartialFunction[List[Exchange], List[Option[Int]]] = {
    case List(Exchange(x), Exchange(y)) => List(Some(y), Some(x))
    case List(Exchange(x)) => List(None)
  }

  /** A worker. */
  def worker(exchanger: TimeoutExchangerT[Int])
    (me: Int, log: HistoryLog[Exchange]) 
  = {
    val random = new Random(me)
    for(i <- 0 until iters){
      Thread.sleep(random.nextInt(3))
      val x = random.nextInt(MaxVal)
      if(countProfile)
        log(me, 
          { val res = exchanger.exchange(x); 
            Profiler.count(res.nonEmpty.toString); res },
          Exchange(x) )
      else 
        log(me, exchanger.exchange(x), Exchange(x))
    }
  }

  var faulty = false; var faulty2 = false 

  /** Run a single test. */
  def doTest = {
    val exchanger: TimeoutExchangerT[Int] =
      if(faulty) new FaultyTimeoutExchanger[Int](1) 
      else if(faulty2) new FaultyTimeoutExchanger2[Int](1) 
      else new TimeoutExchanger[Int](1)
    val tester = 
      new StatelessTester[Exchange](worker(exchanger) _, p, List(1,2), matching)
    // Following might or might not involve timeouts: default is timeout = -1
    tester(timeout)
    // if(!tester(timeout)) sys.exit()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      // case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1

      case "--progressCheck" => // false positives with 100
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
