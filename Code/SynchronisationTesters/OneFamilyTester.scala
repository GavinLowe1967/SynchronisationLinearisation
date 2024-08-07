package synchronisationTester

import scala.util.Random

import synchronisationTesting.{HistoryLog,BinaryStatefulTester}
import synchronisationObject.{
  OneFamilyT, OneFamily, OneFamilyFaulty, DeadlockOneFamily}

/** A testing file for OneFamilyT objects. */
object OneFamilyTester extends Tester{
  /** Number of threads to run. */
  var n = p

  /** The timeout time with the progress check. */
  var timeout = -1

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
    case (Sync(a), Sync(b)) /* if !spec.bitMap(a)(b) && !spec.bitMap(b)(a) */ => 
      spec.sync(a, b)
  }

  /** A worker.  */
  def worker(of: OneFamilyT)(me: Int, log: HistoryLog[Sync]) = {
    for(_ <- 0 until n-1) log(me, of.sync(me), Sync(me))
  }

  /** A worker that might perform one operation fewer than the normal
    * number.  */
  def worker1(of: OneFamilyT)(me: Int, log: HistoryLog[Sync]) = {
    for(_ <- 0 until n-2+Random.nextInt(2)) log(me, of.sync(me), Sync(me))
  }

  /* Flags to decide which implementation to use. */
  var faulty = false // OneFamilyFaulty
  var deadlock = false // DeadlockOneFamily
  /* Default is OneFamily. */

  var doASAP = false

  /** Do a single test. */
  def doTest = {
    val of: OneFamilyT = 
      if(faulty) new OneFamilyFaulty(n) 
      else if(deadlock) new DeadlockOneFamily(n) 
      else new OneFamily(n)
    val spec = new Spec()
    if(progressCheck){
      val bst = new BinaryStatefulTester[Sync,Spec](
        worker1(of), n, matching, spec, doASAP)
      bst(timeout) // if(!bst(timeout)) sys.exit()
    }
    else{
      val bst = new BinaryStatefulTester[Sync,Spec](
        worker(of), n, matching, spec, doASAP)
      bst() // if(!bst()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    var reps = 5000; var i = 0
    var timing = false; var expectTrueTest = false
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => i += 2 // Ignore this! 

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case "--expectTrue" => expectTrueTest = true; i += 1 
      case "--doASAP" => doASAP = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    
    if(expectTrueTest) expectTrue(reps)
    else runTests(reps, timing)
  }

}
