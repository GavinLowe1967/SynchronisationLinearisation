package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepLinSpec}
import synchronisationObject.{  
  MenWomenT, MenWomen, FaultyMenWomen, DeadlockMenWomen}
import TwoStepLinSpec.{log2,SyncSpecObject}

/** A testing file. */
object MenWomenTwoStepTester extends Tester{

  /** The maximum value sent.   */
  var MaxVal = 100

  /** The type we're testing. */
  type C = MenWomenT

  /* We will take `manSync` to correspond to op_1, and `womanSync` to correspond
   * to op_2.  That corresponds to taking A_1 = A_2 = B_1 = B_2 = Int */

  /** The synchronisation specification object.  
  This example is stateless.
  */
  // case class SyncSpec() extends SyncSpecObject[Int,Int,Int,Int]{
  //   def sync(m: Int, w: Int): ((Int, Int), SyncSpec) = 
  //     ((w, m), this)
  // }
 
  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepLinSpec[Int,Int,Int,Int]

  /** A worker.  An even number of these workers should not produce a
    * deadlock. */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    for(i <- 0 until iters) {
      val x = Random.nextInt(MaxVal) // ; var y = ()
      if(me%2 == 0){
        // log(c => {y = c!x}, s"$me: manSync($x)", _.op1(me,x)) // Perform the manSync
        // log(c => y, s"$me: sendX($x)", _.op1X(me)) // Register the second step
        // Note: the above can be simplified, because y = () throughout.  But
        // it follows the standard pattern. 
        log2(log, me, (c:C) => c.manSync(x), s"manSync($x)", x)
      }
      else log(_.womanSync(x), s"womanSync($x)", _.op2(x))
      }
  }

  /* Now with the roles of `manSync` and `womanSync` swapped. */

  // case class SyncSpecR(counter: Int = 0) 
  //     extends SyncSpecObject[Int,Int,Int,Int]{
  //   def sync(w: Int, m: Int): ((Int, Int), SyncSpecR) = 
  //     ((m, w), this)
  // }

  type SpecR = TwoStepLinSpec[Int,Int,Int,Int]

  def workerR(me: Int, log: LinearizabilityLog[SpecR,C]) = {
    for(i <- 0 until iters) {
    val x = Random.nextInt(MaxVal)
      if(me%2 == 0){
        log(_.manSync(x), s"manSync($x)", _.op2(x))
      }
      else{
        // var y: Int = -1
        // log(c => {y = c?()}, s"$me: womanSync", _.op1(me,()))
        // log(c => y, s"$me: receiveX", _.op1X(me))
        log2(log, me, (c:C) => c.womanSync(x), "womanSync($x)", x)
      }
      }
  }

  /* Flags for which channel implementation to use. */
  private var faulty = false // FaultyMenWomen
  private var deadlock = false // DeadlockMenWomen
  /* The default is MenWomen */

  /** Are we doing the test with SpecR and workerR. */
  private var reversed = false

  /** Do a single test. */
  def doTest = {
    val c: C = 
      if(faulty) new FaultyMenWomen
      else if(deadlock) new DeadlockMenWomen
      else new MenWomen
    // The spec is independed of whether we use `reversed`
    def sync(x: Int, y: Int) = (y, x)
    val spec = TwoStepLinSpec[Int,Int,Int,Int](p, sync)
    if(!reversed){
      // val specObj = new SyncSpec
      // val spec = TwoStepLinSpec[Int,Int,Int,Int](p, specObj)
      // val spec = TwoStepLinSpec[Int,Int,Int,Int](p, sync)
      val tester = LinearizabilityTester[Spec,C](spec, c, p, worker _)
      tester() > 0
    }
    else{
      // def syncR(u: Int, x: Int) = (x, ())
      // val specObj = new SyncSpecR
      // val spec = TwoStepLinSpec[Int,Int,Int,Int](p, specObj)
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
      case "--reversed" => reversed = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--timing" => timing = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0)

    runTests(reps, timing)
  }
}
