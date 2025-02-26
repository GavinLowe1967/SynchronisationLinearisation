package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import scala.util.Random
import synchronisationTesting.{TwoStepHomoLinSpec}
import synchronisationObject.{
  OneFamilyT, OneFamily, OneFamilyFaulty, DeadlockOneFamily}
import TwoStepHomoLinSpec.{log2,SyncSpecObject}

/** A testing file. */
object OneFamilyTwoStepTester extends Tester{

  /** The type we're testing. */
  type C = OneFamilyT

  /** Type of the map storing already synchronised pairs. */
  type BitMap = Array[Array[Boolean]]
  
  /** The synchronisation specification object.  
  bitMap shows which threads have already synchronised. 
  */
  case class SyncSpec(val bitMap: BitMap) 
             extends SyncSpecObject[Int,Int]{
    def sync(a: Int, b: Int): ((Int, Int), SyncSpec) = { 
      // These two must not have sync'ed before
      require(!bitMap(a)(b) && !bitMap(b)(a)) 
      // Create updated bitmap.
      val newBitMap = bitMap.map(_.clone); 
      newBitMap(a)(b) = true; newBitMap(b)(a) = true
      ((b,a), new SyncSpec(newBitMap) )
    }

    /* Factory method for SyncSpec() initial state.  */
    def this(n: Int) = this(Array.fill[Boolean](n,n)(false)) 
  }

  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepHomoLinSpec[Int,Int]

  /** A worker.  There must be the same number of workers 
  as the size of the family.
    */
  def worker(me: Int, log: LinearizabilityLog[Spec,C]) = {
    for(_ <- 0 until p-1)  // each worker performs p-1 sync()'s.
      log2(log, me, (c:C) => c.sync(me), s"sync($me)", me)
    }
  
  /* Flags for which channel implementation to use. */
  private var faulty = false // FaultyOneFamily
  /* The default is OneFamily */

  /** Do a single test. 
  Note that the size of the family is the number of workers, p.
  */
  def doTest = {
    val c: C = 
      if(faulty) new OneFamilyFaulty(p)
      else new OneFamily(p)
      val specObj = new SyncSpec(p)
      val spec = TwoStepHomoLinSpec[Int,Int](p, specObj)
      val tester = LinearizabilityTester[Spec,C](spec, c, p, worker _)
      tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    var timing = false; var expectTrueTest = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => i += 2 // Ignore this! 
      case "--reps" => reps = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    if(expectTrueTest) expectTrue(reps)
    else runTests(reps, timing)
  }
}
