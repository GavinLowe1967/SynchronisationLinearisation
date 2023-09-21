// import io.threadcso._
import scala.util.Random

// import synchronisationTesting.{HistoryLog,BinaryStatelessTester}
import ox.cads.testing._

/** A testing using the two-step testing idea.
  * 
  * This doesn't work as it is, because the man's log of its return and the
  * subsequent exit operation might be delayed until after some other
  * synchronisation has taken place. */
object MenWomenTwoStepTester{
  /** Number of worker threads to run. */
  var p = 4

  /** Number of iterations per worker thread. */
  var iters = 20

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  // Representation of operations within the log
  // trait Op
  // case class ManSync(id: Int) extends Op
  // case class WomanSync(id: Int) extends Op

  /** The specification class. */
  // object Spec{
  //   def sync(x: Int, y: Int) = (y, x)
  // }

  trait SeqSpec
  case object Zero extends SeqSpec
  case class One(man: Int) extends SeqSpec
  case class Two(woman: Int) extends SeqSpec

  def seqManSync(man: Int)(ss: SeqSpec): (Unit, SeqSpec) = {
    require(ss == Zero); ((), One(man))
  }
  def seqWomanSync(woman: Int)(ss: SeqSpec): (Int, SeqSpec) = {
    require(ss.isInstanceOf[One]); val One(man) = ss; (man, Two(woman))
  }
  def seqManExit(ss: SeqSpec): (Int, SeqSpec) = {
    require(ss.isInstanceOf[Two]); val Two(woman) = ss; (woman, Zero)
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  // def matching: PartialFunction[(Op,Op), (Any,Any)] = {
  //   case (ManSync(x), WomanSync(y)) => Spec.sync(x, y) 
  // }

  /** A worker. */
  def worker(me: Int, log: GenericThreadLog[SeqSpec, MenWomenT]) = {
    for(i <- 0 until iters){
      val id = Random.nextInt(MaxVal)
      if(me%2 == 0) 
        log.log2(_.manSync(id), s"manSync($id)", seqManSync(id), seqManExit)
      else log.log(_.womanSync(id), s"womanSync($id)", seqWomanSync(id))
    }
  }

  /** Should we use the faulty channel implementation? */
  private var faulty = false
  // private var faulty2 = false

  /** Do a single test. */
  def doTest = {
    val mw: MenWomenT = if(faulty) new FaultyMenWomen else new MenWomen 
    val tester = LinearizabilityTester.JITGraph[SeqSpec, MenWomenT](
      Zero, mw, p, worker _, tsLog = false)
    if(tester() < 0) sys.exit
    // val bst = new BinaryStatelessTester[Op](worker(mw), p, matching)
    // if(!bst()) sys.exit
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
      // case "--faulty2" => faulty2 = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
    assert(p%2 == 0)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println; println(s"$duration ms")
  }
}
