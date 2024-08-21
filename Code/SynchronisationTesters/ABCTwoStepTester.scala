package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepLinSpec3}
import synchronisationObject.{ABCT, ABC, FaultyABC, DeadlockABC}
import TwoStepLinSpec3.{log1,log2,SyncSpecObject}

/** A testing file. */
object ABCTwoStepTester  extends Tester {
  /* Number of worker threads to run.  Needs to be a multiple of 3 here. */
  p = 6
 
  /** The type we're testing. */
  type C = ABCT[Int,Int,Int]

  /** The synchronisation specification object.  
  This example is stateless.
  */
  // case class SyncSpec() extends SyncSpecObject[Int,Int,Int,(Int,Int),(Int,Int),(Int,Int)]{
  //   def sync(a: Int, b: Int, c: Int): (((Int, Int), (Int, Int), (Int, Int)), SyncSpec) = 
  //     (((b,c), (a,c), (a,b)), this)
  // }
 
  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepLinSpec3[Int,Int,Int,(Int,Int),(Int,Int),(Int,Int)]

  /** A worker.  An even number of these workers should not produce a
    * deadlock. */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    for(i <- 0 until iters) {
      me%3 match {
        case 0 => log1(log, me, (c:C) => c.syncA(me), s"syncA($me)", me)
        case 1 => log2(log, me, (c:C) => c.syncB(me), s"syncB($me)", me)
        case 2 => log(_.syncC(me), s"syncC($me)", _.op3(me))
      } // match{}
    }
  }

  /* ToDo: change mapping between syncA/B/C and op1/2/3 ?? */

  /* Flags for which channel implementation to use. */
  private var faulty = false // FaultyABC
  private var deadlock = false // DeadlockABC
  /* The default is ABC */

  /** Do a single test. */
  def doTest = {
    val c: C = 
      if(faulty) new FaultyABC
      else if(deadlock) new DeadlockABC
      else new ABC
    def sync(a: Int, b: Int, c: Int) = ((b,c), (a,c), (a,b))
    val spec = 
      TwoStepLinSpec3[Int,Int,Int,(Int,Int),(Int,Int),(Int,Int)](p, sync)
      // val specObj = new SyncSpec
      // val spec = TwoStepLinSpec3[Int,Int,Int,(Int,Int),(Int,Int),(Int,Int)](p, specObj)
    val tester = LinearizabilityTester[Spec,C](spec, c, p, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false; var countReps = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--countReps" => countReps = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%3 == 0)

    runTests(reps, timing)  
    }
}
