package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationTesting.TwoStepLinSpec
import TwoStepLinSpec.{log2,SyncSpecObject}
import synchronisationObject.{TwoFamiliesT,TwoFamilies,FaultyTwoFamilies}

object TwoFamiliesTwoStepTester extends Tester{
  /** Number of A threads threads to run. */
  var m = 4

  /** Number of B threads to run. */
  var n = 3

  /** The type of synchronisation objects tested. */
  type TF = TwoFamiliesT

  /** A bit map, used in the synchronisation specification object to record
    * which threads have synchronised. */
  type BitMap = Array[Array[Boolean]]

  /** The synchronisation specification object. */
  class SyncSpec(val bitMap: BitMap = Array.ofDim[Boolean](m,n))
      extends SyncSpecObject[Int,Int,Int,Int]{

    def sync(a: Int, b: Int) = {
      require(!bitMap(a)(b)) // these two haven't previously synchronised. 
      // Create updated bitmap.
      val newBitMap = bitMap.map(_.clone); newBitMap(a)(b) = true
      ((b,a), new SyncSpec(newBitMap))
    }

    // Equality is value equality of the bitmaps.
    override def equals(that: Any) = that match{
      case s: SyncSpec => 
        (0 until m).forall(a => bitMap(a).sameElements(s.bitMap(a)))
    }

    // hashcode based on the bitmaps. 
    override def hashCode = {
      var h = 0
      for(a <- 0 until m; b <- 0 until n){ h = h << 1; if(bitMap(a)(b)) h += 1 }
      h
    }
  } // end of SyncSpec

  /** The type of two-step linearizability specification objects. */
  type Spec = TwoStepLinSpec[Int,Int,Int,Int]

  /** A worker.  Threads [0..m) are A-threads, and threads [m..m+n) are
    * B-threads. */
  def worker(me: Int, log: LinearizabilityLog[Spec,TF]) = {
    if(me < m) // A thread
      for(_ <- 0 until n) 
        log2(log, me, (tf: TF) => tf.syncA(me), s"syncA($me)", me)
    else{ // B thread
      val b = me-m // the id to use
      for(_ <- 0 until m) 
        log((tf: TF) => tf.syncB(b), s"syncB($me)", _.op2(b))
    }
  }

  private var faulty = false

  /** Run a single test. */
  def doTest: Boolean = {
    val p = m+n // total number of threads
    val tf: TF = 
      if(faulty) new FaultyTwoFamilies(m, n) else new TwoFamilies(m, n)
    val specObj = new SyncSpec
    val spec = TwoStepLinSpec[Int,Int,Int,Int](p, specObj)
    val tester = LinearizabilityTester[Spec,TF](spec, tf, p, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var timing = false; 
    while(i < args.length) args(i) match{
      case "-m" => m = args(i+1).toInt; i += 2
      case "-n" => n = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      // case "--reversed" => reversed = true; i += 1
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }
}
