package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,BinaryStatelessTester}
import synchronisationObject.{
 MenWomenT, MenWomen, FaultyMenWomen, DeadlockMenWomen}

/** A testing file. */
object MenWomenTester{
  /** Number of worker threads to run. */
  var p = 4

  /** Number of iterations per worker thread. */
  var iters = 20

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** Do we check the progress condition? */
  var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1

  // Representation of operations within the log
  trait Op
  case class ManSync(id: Int) extends Op
  case class WomanSync(id: Int) extends Op

  /** The specification class. */
  object Spec{
    def sync(x: Int, y: Int) = (y, x)
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[(Op,Op), (Any,Any)] = {
    case (ManSync(x), WomanSync(y)) => Spec.sync(x, y) 
  }

  /** A worker. */
  def worker(mw: MenWomenT)(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      val id = Random.nextInt(MaxVal)
      if(me%2 == 0) log(me, mw.manSync(id), ManSync(id)) 
      else log(me, mw.womanSync(id), WomanSync(id))
    }
  }

  /** A worker for checking the progress condition.  This worker chooses their
    * gender randomly! */
  def worker1(mw: MenWomenT)(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      val id = Random.nextInt(MaxVal)
      if(Random.nextBoolean()) log(me, mw.manSync(id), ManSync(id)) 
      else log(me, mw.womanSync(id), WomanSync(id))
    }
  }

  /* Flags to identify the implementation to use. */
  private var faulty = false // FaultyMenWomen
  private var deadlock = false // DeadlockMenWomen
  /* Default is MenWomen */
  // private var faulty2 = false

  /** Do a single test. */
  def doTest = {
    val mw: MenWomenT = 
      if(faulty) new FaultyMenWomen else if(deadlock) new DeadlockMenWomen
      else new MenWomen
    if(progressCheck){
      val bst = new BinaryStatelessTester[Op](worker1(mw), p, matching)
      if(!bst(timeout)) sys.exit()
    }
    else{
      val bst = new BinaryStatelessTester[Op](worker(mw), p, matching)
      if(!bst()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ 
      doTest; if(i%100 == 0 || progressCheck && i%10 == 0) print(".") 
    }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}
