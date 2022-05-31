package synchronisationTester

import scala.util.Random

import synchronisationTesting.{HistoryLog,HomogeneousBinaryStatelessTester}
import synchronisationObject.{ExchangerT,Exchanger,FaultyExchanger}

object ExchangerTester{
  /** Number of worker threads to run. */
  var p = 16

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** Do we check the progress condition? */
  var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1

  /** Number of iterations when testing for progress. */
  var iters = 20

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
      if(!tester(timeout)) sys.exit()
    }
    else{
      val tester = new HomogeneousBinaryStatelessTester[Exchange](
        worker(exchanger), p, matching)
      if(!tester()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0 || progressCheck)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ 
      doTest; if(i%100 == 0 || progressCheck && i%10 == 0) print(".") 
    }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}


// For the faulty version, use scala ExchangerTester --faulty -p 12 --MaxVal 2
