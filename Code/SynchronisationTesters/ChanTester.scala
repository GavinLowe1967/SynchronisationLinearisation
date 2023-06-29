package synchronisationTester

// import io.threadcso._ 
import scala.util.Random

import synchronisationTesting.{HistoryLog,ThreadUtil,BinaryStatelessTester}

import synchronisationObject.{
  Chan, FaultyChan, DeadlockingChan, FaultyChan2, WrappedSCLChan, SyncChan}

/** A testing file. */
object ChanTester extends Tester{
  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** The timeout time with the progress check. */
  var timeout = -1

  // Representation of operations within the log
  trait Op
  case class Send(x: Int) extends Op
  case object Receive extends Op

  /** The specification class. */
  object Spec{
    def sync(x: Int, u: Unit) = ((), x)
  } 

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[(Op,Op), (Any,Any)] = {
    case (Send(x), Receive) => Spec.sync(x, ()) 
  }

  /** Do a send on c, and store in log, associated with me. */
  @inline def doSend(c: Chan[Int], me: Int, log: HistoryLog[Op]) = { 
    val x = Random.nextInt(MaxVal); log(me, c!x, Send(x)) 
  }

  /** A worker.  An even number of these workers should not produce a
    * deadlock. */
  def worker(c: Chan[Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters)
      if(me%2 == 0) doSend(c, me, log)
      else log(me, c?(), Receive)
  }

  /** A worker that performs randomly chosen operations.  A system built from
    * these is likely to deadlock. */
  def worker1(c: Chan[Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters)
      if(Random.nextInt(2) == 0) doSend(c, me, log)
      else log(me, c?(), Receive)
  } 

  /* Flags for which channel implementation to use.  Default is SyncChan*/
  private var faulty = false   // FaultyChan
  private var faulty2 = false // FaultyChan2
  private var deadlock = false // DeadlockingChan
  private var wrapped = false // WrappedSCLChan

  /** Do a single test.  Return true if it passes. */
  def doTest: Boolean = {
    val c: Chan[Int] = 
      if(faulty) new FaultyChan[Int] 
      else if(deadlock) new DeadlockingChan[Int] 
      else if(faulty2) new FaultyChan2[Int]
      else if(wrapped) new WrappedSCLChan[Int]
      else new SyncChan[Int]
    if(progressCheck){
      val bst = new BinaryStatelessTester[Op](worker1(c), p, matching)
      bst(timeout)
    }
    else{
      val bst = new BinaryStatelessTester[Op](worker(c), p, matching)
      bst()
    }
  }
 
  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var timing = false; var expectTrueTest = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1
      case "--wrapped" => wrapped = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case "--expectTrue" => expectTrueTest = true; i += 1 
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
 
    assert(p%2 == 0 || progressCheck)

    if(expectTrueTest) expectTrue(reps)
    else runTests(reps, timing)
  }
}
