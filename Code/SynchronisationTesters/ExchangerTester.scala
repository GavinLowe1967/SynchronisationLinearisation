package synchronisationTester

import scala.util.Random

import synchronisationTesting.{HistoryLog,HomogeneousBinaryStatelessTester}
import synchronisationObject.{ExchangerT,Exchanger,FaultyExchanger}
import ox.gavin.profiling.{Profiler,SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object ExchangerTester extends Tester{
  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** The timeout time with the progress check. */
  var timeout = -1

  // Representation of operations within the log.
  case class Exchange(x: Int)

  /** Specification object. */
  object Spec{
    def sync(x: Int, y: Int) = (y, x)
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[(Exchange,Exchange), (Any,Any)] = {
    case (Exchange(x), Exchange(y)) => Spec.sync(x, y) 
  }

  /** A worker.  Each worker performs a single invocation, to avoid
    * deadlocks.  */
  def worker(exchanger: ExchangerT[Int])(me: Int, log: HistoryLog[Exchange]) = {
    val x = Random.nextInt(MaxVal)
    log(me, exchanger.exchange(x), Exchange(x))
  }

  /** A worker that performs randomly chosen operations.  A system built from
    * these is likely to deadlock.  */
  def worker1(exchanger: ExchangerT[Int])(me: Int, log: HistoryLog[Exchange]) = {
    for(_ <- 0 until iters){
      val x = Random.nextInt(MaxVal)
      log(me, exchanger.exchange(x), Exchange(x))
    }
  }

  /** Should we use the faulty channel implementation? */
  private var faulty = false

  /** Do a single test. */
  def doTest = {
    val exchanger: ExchangerT[Int] = 
      if(faulty) new FaultyExchanger[Int] else new Exchanger[Int]
    if(progressCheck){
      val tester = new HomogeneousBinaryStatelessTester[Exchange](
        worker1(exchanger), p, matching)
      tester(timeout)
    }
    else{
      val tester = new HomogeneousBinaryStatelessTester[Exchange](
        worker(exchanger), p, matching)
      tester()
    }
  }

  def main(args: Array[String]): Unit = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    var timing = false; var expectTrueTest = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case "--expectTrue" => expectTrueTest = true; i += 1 
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0 || progressCheck)

    if(expectTrueTest) expectTrue(reps)
    else  if(profiling){
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
  }
}
 

// For the faulty version, use scala ExchangerTester --faulty -p 12 --MaxVal 2
