package synchronisationTester

import scala.util.Random

import synchronisationTesting.{HistoryLog}
import synchronisationObject.{TwoFamiliesT,TwoFamilies,FaultyTwoFamilies}

/** A testing file for TwoFamiliesT objects. */
object TwoFamiliesTester{
  /** Number of A threads threads to run. */
  var m = 4

  /** Number of B threads to run. */
  var n = 3

  // Representation of operations within the log
  trait Op
  case class ASync(a: Int) extends Op
  case class BSync(b: Int) extends Op

  type BitMap = Array[Array[Boolean]]

  /** The specification class. */
  class Spec(val bitMap: BitMap = Array.ofDim[Boolean](m,n)){
    def sync(a: Int, b: Int): (Spec, (Int, Int)) = { 
      require(!bitMap(a)(b)) // These two must not have sync'ed before
      // Create updated bitmap.
      val newBitMap = bitMap.map(_.clone); newBitMap(a)(b) = true
      (new Spec(newBitMap), (b,a)) 
    }

    // Equality is value equality of the bitmaps.
    override def equals(that: Any) = that match{
      case s: Spec => 
        (0 until m).forall(a => bitMap(a).sameElements(s.bitMap(a)))
    }

    // hashcode based on the bitmaps. 
    override def hashCode = {
      var h = 0
      for(a <- 0 until m; b <- 0 until n){ h = h << 1; if(bitMap(a)(b)) h += 1 }
      h
    }

    override def toString = "Spec("+
      bitMap.map(_.map(x => if(x) "T" else "F").mkString).mkString(";")+")"
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[(Op,Op), (Spec,(Any,Any))] = {
    case (ASync(a), BSync(b)) => spec.sync(a, b) 
  }

  /** A worker.  Threads [0..m) are A-threads, and threads [m..m+n) are
    * B-threads. */
  def worker(tf: TwoFamiliesT)(me: Int, log: HistoryLog[Op]) = {
    if(me < m) // A thread
      for(_ <- 0 until n) log(me, tf.syncA(me), ASync(me))
    else{ // B thread
      val b = me-m // the id to use
      for(_ <- 0 until m) log(me, tf.syncB(b), BSync(b))
    }
  }

  private var faulty = false

  /** Do a single test. */
  def doTest = {
    val tf = if(faulty) new FaultyTwoFamilies(m, n) else new TwoFamilies(m, n)
    val spec = new Spec()
    val bst = new synchronisationTesting.BinaryStatefulTester[Op,Spec](
      worker(tf), m+n, matching, spec)
    if(!bst()) sys.exit
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-m" => m = args(i+1).toInt; i += 2
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
  
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
  }
}
