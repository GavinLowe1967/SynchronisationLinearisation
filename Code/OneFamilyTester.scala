package synchronisationTester

import scala.util.Random

import synchronisationTesting.HistoryLog


/** A testing file for OneFamilyT objects. */
object OneFamilyTester{
  /** Number of threads to run. */
  var n = 4

  // Representation of operations within the log
  case class Sync(id: Int)

  type BitMap = Array[Array[Boolean]]

  /** The specification class.  bitMap shows which threads have already
    * synchronised. */
  class Spec(val bitMap: BitMap = Array.ofDim[Boolean](n,n)){
    def sync(a: Int, b: Int): (Spec, (Int, Int)) = { 
      // These two must not have sync'ed before
      require(!bitMap(a)(b) && !bitMap(b)(a)) 
      // Create updated bitmap.
      val newBitMap = bitMap.map(_.clone); 
      newBitMap(a)(b) = true; newBitMap(b)(a) = true
      (new Spec(newBitMap), (b,a)) 
    }

    // Equality is value equality of the bitmaps.
    override def equals(that: Any) = that match{
      case s: Spec => 
        (0 until n).forall(a => bitMap(a).sameElements(s.bitMap(a)))
    }

    // hashcode based on the bitmaps. 
    override def hashCode = {
      var h = 0
      for(a <- 0 until n; b <- 0 until n){ h = h << 1; if(bitMap(a)(b)) h += 1 }
      h
    }

    override def toString = "Spec("+
      bitMap.map(_.map(x => if(x) "T" else "F").mkString).mkString(";")+")"
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[(Sync,Sync), (Spec,(Any,Any))] = {
    case (Sync(a), Sync(b)) => spec.sync(a, b) 
  }

  /** A worker.   */
  def worker(of: OneFamilyT)(me: Int, log: HistoryLog[Sync]) = {
    for(_ <- 0 until n-1) log(me, of.sync(me), Sync(me))
  }

  var faulty = false

  /** Do a single test. */
  def doTest = {
    val of: OneFamilyT = if(faulty) new OneFamilyFaulty(n) else  new OneFamily(n)
    val spec = new Spec()
    val bst = new synchronisationTesting.BinaryStatefulTester[Sync,Spec](
      worker(of), n, matching, spec)
    if(!bst()) sys.exit
  }

  def main(args: Array[String]) = {
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
    
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
  }

}
