package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepHomoLinSpec}
import synchronisationObject.{ExchangerT, Exchanger, FaultyExchanger}
import TwoStepHomoLinSpec.{log2,SyncSpecObject}

/** A testing file. */
object ExchangerTwoStepTester extends Tester{

  /** The maximum value sent.   */
  var MaxVal = 100

  /** The type we're testing. */
  type C = ExchangerT[Int]
  
  // /** The synchronisation specification object.  
  //   * This example is stateless so the SyncSpec object is unchanged.  */
  // case class SyncSpec() extends SyncSpecObject[Int,Int]{
  //   def sync(x: Int, y: Int): ((Int, Int), SyncSpec) = 
  //     ((y, x), this)
  // }
 
  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepHomoLinSpec[Int,Int]

  /** A worker.  An even number of these workers should not produce a
    * deadlock as long as each worker only performs one exchange.
    */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    val x = Random.nextInt(MaxVal) 
    log2(log, me, (c:C) => c.exchange(x), s"exchange($x)", x)
  }

  
  /* Flags for which channel implementation to use. */
  private var faulty = false // FaultyExchanger
  /* The default is Exchanger */

  /** Are we doing the test with SpecR and workerR. */
  private var reversed = false

  /** Do a single test. */
  def doTest = {
    val c: C = 
      if(faulty) new FaultyExchanger
      else new Exchanger
      // val specObj = new SyncSpec
      // val spec = TwoStepHomoLinSpec[Int,Int](p, specObj)
    def sync(x: Int, y: Int) = (y,x)
    val spec = TwoStepHomoLinSpec[Int,Int](p, sync)
    val tester = LinearizabilityTester[Spec,C](spec, c, p, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
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
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0 || progressCheck)

    if(expectTrueTest) expectTrue(reps)
    else runTests(reps, timing)
  }
}
