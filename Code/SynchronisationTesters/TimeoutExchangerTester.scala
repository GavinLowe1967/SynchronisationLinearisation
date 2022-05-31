package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog, StatelessTester}
import synchronisationObject.{
  TimeoutExchangerT, TimeoutExchanger, FaultyTimeoutExchanger}
import ox.gavin.profiling.Profiler

object TimeoutExchangerTester{
  /** Number of worker threads to run.  */
  var p = 4

  /** The number of iterations by each thread. */
  var iters = 10

  /** The maximum value sent. */
  var MaxVal = 20

  /** Do we check the progress condition? */
  var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1

  /** Representation of an operation. */
  case class Exchange(x: Int)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object.   */
  def matching: PartialFunction[List[Exchange], List[Option[Int]]] = {
    case List(Exchange(x), Exchange(y)) => 
      /*Profiler.count("sync"); */ List(Some(y), Some(x))
    case List(Exchange(x)) => /* Profiler.count("non-sync");*/ List(None)
  }

  /** A worker. */
  def worker(exchanger: TimeoutExchangerT[Int])
    (me: Int, log: HistoryLog[Exchange]) 
  = {
    for(i <- 0 until iters){
      Thread.sleep(Random.nextInt(20))
      val x = Random.nextInt(MaxVal)
      log(me, exchanger.exchange(x), Exchange(x))
    }
  }

  var faulty = false

  /** Run a single test. */
  def doTest = {
    val exchanger: TimeoutExchangerT[Int] =
      if(faulty) new FaultyTimeoutExchanger[Int](3) 
      else new TimeoutExchanger[Int](3)
    val tester = 
      new StatelessTester[Exchange](worker(exchanger) _, p, List(1,2), matching)
    // Following might or might not involve timeouts
    if(!tester(timeout)) sys.exit()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      // case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      // case "--version2" => version2 = true; i += 1
      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1
      // case "--faulty3" => faulty2 = true; i += 1

      case "--progressCheck" => // false positives with 100
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    for(i <- 0 until reps){
      doTest; if(i%10 == 0) print(".")
    }
    println(); Profiler.report
  }

}
