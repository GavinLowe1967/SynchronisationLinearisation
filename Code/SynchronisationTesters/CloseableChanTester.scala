package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog, StatefulTester}
import synchronisationObject.{
  ClosedException, CloseableChan, CloseableSyncChan, FaultyCloseableSyncChan, 
  WrappedCloseableChan, FaultyWrappedCloseableChan}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object CloseableChanTester extends Tester{
  /** The maximum value sent. */
  var MaxVal = 20

  /** The timeout time with the progress check. */
  var timeout = -1

  var verbose = false

  /** Representation of an operation in the log. */
  trait Op
  case class Send(x: Int) extends Op
  case object Receive extends Op
  case object Close extends Op

  /* We represent the results of sends as booleans, with success mapping to
   * true, and ClosedException to false.  We represent the results of receives
   * using Option values, similarly. */

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object.  Here the specification object
    * is simply a Boolean, indicating whether the channel is closed.  */
  def matching(closed: Boolean): 
      PartialFunction[List[Op], (Boolean, List[Any])] = {
    case List(Send(x), Receive) if !closed => (closed, List(true, Some(x)))
    case List(Send(x)) if closed => (closed, List(false))
    case List(Receive) if closed => (closed, List(None))
    case List(Close) => (true, List(()))
  }

  /** The probability of an operation by a receiver being a close. */
  private var closeProb = 0.05

  /** Try to send x on chan, catching a ClosedException.  Return true if
    * successful. */ 
  @inline private def trySend(chan: CloseableChan[Int], x: Int): Boolean = 
    try{ chan!x; true } catch { case _: ClosedException => false }

  /** Try to receive on chan, catching a ClosedException.  Optionally return the
    * value received. */ 
  @inline private def tryReceive(chan: CloseableChan[Int]): Option[Int] = 
    try{ Some(chan?()) } catch { case _: ClosedException => None }

  /** A worker.  Workers close the channel with probability closeProb;
    * otherwise, workers with an even identity send; workers with an odd
    * identity receive.  */
  def worker(chan: CloseableChan[Int])(me: Int, log: HistoryLog[Op]) = {
    val random = new Random()
    for(i <- 0 until iters){
      if(random.nextFloat() < closeProb) log(me, chan.close, Close)
      else if(me%2 == 0){
        val x = random.nextInt(MaxVal); log(me, trySend(chan, x), Send(x))
      }
      else log(me, tryReceive(chan), Receive)
    }
  }    

  def worker1(chan: CloseableChan[Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      if(Random.nextFloat() < closeProb) log(me, chan.close, Close)
      else if(Random.nextFloat() < 0.5){
        val x = Random.nextInt(MaxVal); log(me, trySend(chan, x), Send(x))
      }
      else log(me, tryReceive(chan), Receive)
    }
  }  

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]) = 
    ops.length == 1 || 
      (ops match{ case(List(Send(_), Receive)) => true; case _ => false })

  /* Flags for identifying the channel type. */
  var faulty = false
  var wrapped = false
  var faultyWrapped = false
 
  /** Do we use ASAP? */
  var doASAP = false

  /** Run a single test. */
  def doTest = {
    val chan: CloseableChan[Int] = 
      if(faulty) new FaultyCloseableSyncChan[Int] 
      else if(wrapped) new WrappedCloseableChan[Int]
      else if(faultyWrapped) new FaultyWrappedCloseableChan[Int]
      else new CloseableSyncChan[Int]
    if(progressCheck){ 
      val tester =
        new StatefulTester[Op,Boolean](
          worker1(chan), p, List(1,2), matching, suffixMatching, false,
          doASAP, verbose)
      tester(timeout)
    }
    else{
      val tester =
        new StatefulTester[Op,Boolean](
          worker(chan), p, List(1,2), matching, // suffixMatching, 
          spec0 = false, doASAP = doASAP, verbose = verbose)
      tester()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false; // var countReps = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--closeProb" => closeProb = args(i+1).toDouble; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--wrapped" => wrapped = true; i += 1
      case "--faultyWrapped" => faultyWrapped = true; i += 1

      case "--doASAP" => doASAP = true; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--timing" => timing = true; i += 1
      // case "--countReps" => countReps = true; i += 1

      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    assert(p%2 == 0)

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
      profiler{ runTests(reps, timing /*, countReps*/) }; ()
    }
    else runTests(reps, timing /*, countReps*/ )
  }
}
