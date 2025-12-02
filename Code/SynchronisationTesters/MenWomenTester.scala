package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,BinaryStatelessTester}
import synchronisationObject.{
 MenWomenT, MenWomen, FaultyMenWomen, FaultyMenWomen2, DeadlockMenWomen}

/** A testing file. */
object MenWomenTester extends Tester{
  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100
 
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
  private var faulty2 = false // FaultyMenWomen2
  private var deadlock = false // DeadlockMenWomen
  /* Default is MenWomen */

  /** Do a single test. */
  def doTest = {
    val mw: MenWomenT = 
      if(faulty) new FaultyMenWomen 
      else if(faulty2) new FaultyMenWomen2
      else if(deadlock) new DeadlockMenWomen
      else new MenWomen
    if(progressCheck){
      val bst = new BinaryStatelessTester[Op](worker1(mw), p, matching)
      bst(timeout) // if(!bst(timeout)) sys.exit()
    }
    else{
      val bst = new BinaryStatelessTester[Op](worker(mw), p, matching)
      bst() // if(!bst()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    var timing = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--timing" => timing = true; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0)

    runTests(reps, timing)
  }
}
