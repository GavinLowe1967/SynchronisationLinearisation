package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepLinSpec}
import synchronisationObject.{
  Chan, FaultyChan, DeadlockingChan, FaultyChan2, WrappedSCLChan, SyncChan}
import TwoStepLinSpec.log2

/** A testing file. */
object ChanTwoStepTester{
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
  type C = Chan[Int]

  /* We will take `!` to correspond to op_1, and `?` to correspond to op_2.
   * That corresponds to taking A_1 = B_2 = Int, and A_2 = B_1 = Unit. */ 

  type Spec = TwoStepLinSpec[Int,Unit,Unit,Int]

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
        log2(log, me, (c:C) => c!x, s"send($x)", x)
      }
      else log(_?(), s"receive", _.op2(()))
  }

  /* Now with the roles of `!` and `?` swapped. */

  type SpecR = TwoStepLinSpec[Unit,Int,Int,Unit]

  def workerR(me: Int, log: LinearizabilityLog[SpecR,C]) = {
    for(i <- 0 until iters)
      if(me%2 == 0){
        val x = Random.nextInt(MaxVal); log(_!x, s"send($x)", _.op2(x))
      }
      else{
        // var y: Int = -1
        // log(c => {y = c?()}, s"$me: receive", _.op1(me,()))
        // log(c => y, s"$me: receiveX", _.op1X(me))
        log2(log, me, (c:C) => c?(), "receive", ())
      }
  }

  /* Flags for which channel implementation to use.  Default is SyncChan*/
  private var faulty = false   // FaultyChan
  private var faulty2 = false // FaultyChan2
  private var deadlock = false // DeadlockingChan
  private var wrapped = false // WrappedSCLChan

  /** Are we doing the test with SpecR and workerR. */
  private var reversed = false

  /** Do a single test. */
  def doTest = {
    val c: Chan[Int] = 
      if(faulty) new FaultyChan[Int] 
      else if(deadlock) new DeadlockingChan[Int] 
      else if(faulty2) new FaultyChan2[Int] 
      else if(wrapped) new WrappedSCLChan[Int]
      else new SyncChan[Int]
    if(!reversed){
      val specObj = new TwoStepLinSpec.SpecObject[Int,Unit,Unit,Int]{ 
        def sync(x: Int, u: Unit) = ((), x) 
      }
      val spec = TwoStepLinSpec[Int,Unit,Unit,Int](numThreads, specObj)
      val tester = LinearizabilityTester[Spec,C](spec, c, numThreads, worker _)
      if(tester() <= 0) sys.exit()
    }
    else{
      def syncR(u: Unit, x: Int) = (x, ())
      val spec = TwoStepLinSpec[Unit,Int,Int,Unit](numThreads, syncR _)
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
      case "--faulty2" => faulty2 = true; i += 1
      case "--wrapped" => wrapped = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(numThreads%2 == 0)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}
