package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepLinSpec}
import synchronisationObject.{  
  ChanCounterT, ChanCounter, ChanCounter2, ChanCounter3, FaultyChanCounter, 
  DeadlockingChanCounter}
import TwoStepLinSpec.{log2,SyncSpecObject}

/** A testing file. */
object ChanCounterTwoStepTester{
  /** Number of worker threads to run. */
  var numThreads = 4

  /** Number of iterations per worker thread. */
  var iters = 20

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** The type we're testing. */
  type C = ChanCounterT[Int]

  /* We will take `send` to correspond to op_1, and `receive` to correspond to
   * op_2.  That corresponds to taking A_1 = B_1 = B_2 = Int, and A_2 Unit. */

  /** The synchronisation specification object.  It keeps track of the sequence
    * counter. */
  case class SyncSpec(counter: Int = 0) extends SyncSpecObject[Int,Unit,Int,Int]{
    def sync(x: Int, u: Unit): ((Int, Int), SyncSpec) = 
      ((counter+1, x), new SyncSpec(counter+1))
  }
 
  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepLinSpec[Int,Unit,Int,Int]

  /** A worker.  An even number of these workers should not produce a
    * deadlock. */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    for(i <- 0 until iters)
      if(me%2 == 0){
        val x = Random.nextInt(MaxVal) // ; var y = ()
        // log(c => {y = c!x}, s"$me: send($x)", _.op1(me,x)) // Perform the send
        // log(c => y, s"$me: sendX($x)", _.op1X(me)) // Register the second step
        // Note: the above can be simplified, because y = () throughout.  But
        // it follows the standard pattern. 
        log2(log, me, (c:C) => c.send(x), s"send($x)", x)
      }
      else log(_.receive(), s"receive", _.op2(()))
  }

  /* Now with the roles of `send` and `receive` swapped. */

  case class SyncSpecR(counter: Int = 0) 
      extends SyncSpecObject[Unit,Int,Int,Int]{
    def sync(u: Unit, x: Int): ((Int, Int), SyncSpecR) = 
      ((x, counter+1), new SyncSpecR(counter+1))
  }

  type SpecR = TwoStepLinSpec[Unit,Int,Int,Int]

  def workerR(me: Int, log: LinearizabilityLog[SpecR,C]) = {
    for(i <- 0 until iters)
      if(me%2 == 0){
        val x = Random.nextInt(MaxVal); log(_.send(x), s"send($x)", _.op2(x))
      }
      else{
        // var y: Int = -1
        // log(c => {y = c?()}, s"$me: receive", _.op1(me,()))
        // log(c => y, s"$me: receiveX", _.op1X(me))
        log2(log, me, (c:C) => c.receive(), "receive", ())
      }
  }

  /* Flags for which channel implementation to use. */
  private var faulty = false // FaultyChanCounter
  private var deadlock = false // DeadlockingChanCounter
  private var version2 = false // ChanCounter2
  private var version3 = false // ChanCounter3
  /* The default is ChanCounter */

  /** Are we doing the test with SpecR and workerR. */
  private var reversed = false

  /** Do a single test. */
  def doTest = {
    val c: C = 
      if(faulty) new FaultyChanCounter[Int]
      else if(deadlock) new DeadlockingChanCounter[Int]
      else if(version2) new ChanCounter2[Int]
      else if(version3) new ChanCounter3[Int]
      else new ChanCounter[Int]
    if(!reversed){
      val specObj = new SyncSpec
      val spec = TwoStepLinSpec[Int,Unit,Int,Int](numThreads, specObj)
      val tester = LinearizabilityTester[Spec,C](spec, c, numThreads, worker _)
      if(tester() <= 0) sys.exit()
    }
    else{
      // def syncR(u: Unit, x: Int) = (x, ())
      val specObj = new SyncSpecR
      val spec = TwoStepLinSpec[Unit,Int,Int,Int](numThreads, specObj)
      val tester = LinearizabilityTester[SpecR,C](spec, c, numThreads, workerR _)
      if(tester() <= 0) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-p" => numThreads = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--reversed" => reversed = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1
      case "--version2" => version2 = true; i += 1
      case "--version3" => version3 = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(numThreads%2 == 0)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}
