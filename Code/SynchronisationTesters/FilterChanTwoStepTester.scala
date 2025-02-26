package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepLinSpec}
import synchronisationObject.{FilterChanT, FilterChan, FaultyFilterChan, 
                              DeadlockingFilterChan, SemaphoreFilterChan}
import TwoStepLinSpec.log2

/** A testing file. */
object FilterChanTwoStepTester extends Tester{

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** The type we're testing. */
  type C = FilterChanT[Int]

  /* We will take send() to correspond to op_1, and receive() to correspond to op_2.
   * That corresponds to taking A_1 = B_2 = Int, and A_2 = B_1 = Unit. */ 

  type Spec = TwoStepLinSpec[Int,Int => Boolean,Unit,Int]

  /** A worker.  An even number of these workers should not produce a
    * deadlock. */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    val even = me%4 < 2
    val f = (x: Int) => x%2 == (if(even) 0 else 1)
    for(i <- 0 until iters)
      if(me%2 == 0){  // sender
        val x = 2*Random.nextInt(MaxVal/2)+(if(even) 0 else 1)
        log2(log, me, (c:C) => c.send(x), s"send($x)", x)
      }
      else log(_.receive(f), s"receive", _.op2(f))
  }

  /* Reverse roles of send and receive in 2-step testing. */

  type SpecR = TwoStepLinSpec[Int => Boolean,Int,Int,Unit]

  def workerR(me: Int, log: LinearizabilityLog[SpecR,C]) = {
    val even = me%4 < 2
    val f = (x: Int) => x%2 == (if(even) 0 else 1)
    for(i <- 0 until iters)
      if(me%2 == 0){ // sender
        val x = 2*Random.nextInt(MaxVal/2)+(if(even) 0 else 1)
        log(_.send(x), s"send($x)", _.op2(x))
      }
      else{
        // var y: Int = -1
        // log(c => {y = c.receive()}, s"$me: receive", _.op1(me,()))
        // log(c => y, s"$me: receiveX", _.op1X(me))
        log2(log, me, (c:C) => c.receive(f), "receive", f)
      }
  }

  /* Flags for which channel implementation to use.  Default is SemaphoreFilterChan*/
  private var faulty = false // FaultyFilterChan
  // private var semaphore = false // SemaphoreFilterChan
  private var deadlock = false // DeadlockingChan
  private var nonProgressing = false

  /** Are we doing the test with SpecR and workerR. */
  private var reversed = false

  /** Do a single test. */
  def doTest: Boolean = {
    val c: FilterChanT[Int] = 
      if(faulty) new FaultyFilterChan[Int] 
      else if(deadlock) new DeadlockingFilterChan[Int]
      else if(nonProgressing) new FilterChan[Int]
      else new SemaphoreFilterChan[Int]
    if(!reversed){
      /* The following definition of sync() will prevent the tester from
      linearising op2() unless x matches the f condition.*/
      def sync(x: Int, f: Int => Boolean) = {require(f(x));((), x)}
      val spec = TwoStepLinSpec[Int,Int => Boolean,Unit,Int](p, sync _)
      val tester = LinearizabilityTester[Spec,C](spec, c, p, worker _)
      tester() > 0
    }
    else{
      /* The following definition of sync() will prevent the tester from
      linearising op2() unless x matches the f condition. */
      def syncR(f: Int => Boolean, x: Int) = {require(f(x));(x,())}
      val spec = TwoStepLinSpec[Int => Boolean,Int,Int,Unit](p, syncR _)
      val tester = LinearizabilityTester[SpecR,C](spec, c, p, workerR _)
      tester() > 0 
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
      case "--reversed" => reversed = true; i += 1

      case "--faulty" => faulty = true; i += 1
      // case "--semaphore" => semaphore = true; i += 1
      case "--nonProgressing" => nonProgressing = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    assert(p%2 == 0 || progressCheck)

    if(expectTrueTest) expectTrue(reps)
    else runTests(reps, timing)
  }
}
