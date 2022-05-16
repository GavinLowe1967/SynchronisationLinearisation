package synchronisationTester

import scala.util.Random

import synchronisationTesting.{HistoryLog}
import synchronisationObject.{ChanCounterT,ChanCounter,FaultyChanCounter}

/** A testing file for a synchronous channel with a sequence counter. */
object ChanCounterTester{
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
  trait Op
  case class Send(x: Int) extends Op
  case object Receive extends Op

  /** The specification class. */
  class Spec(counter: Int = 0){
    def sync(x: Int, u: Unit): (Spec, (Int, Int)) = { 
      (new Spec(counter+1), (counter+1, x)) 
    }

    override def toString = s"Spec($counter)"
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[(Op,Op), (Spec,(Any,Any))] = {
    case (Send(x), Receive) => spec.sync(x, ()) 
  }

  /** A worker. */
  def worker(c: ChanCounterT[Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters)
      if(me%2 == 0){ 
        val x = Random.nextInt(MaxVal); log(me, c.send(x), Send(x)) 
      }
      else log(me, c.receive(), Receive)
  }

  /** Should we use the faulty channel implementation? */
  private var faulty = false
  // private var faulty2 = false

  /** Do a single test. */
  def doTest = {
    val c: ChanCounterT[Int] = 
      if(faulty) new FaultyChanCounter[Int] else  new ChanCounter[Int]
    val spec = new Spec()
    val bst = new synchronisationTesting.BinaryStatefulTester[Op,Spec](
      worker(c), p, matching, spec)
    // assert(bst())
    if(!bst()) sys.exit()
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
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%2 == 0)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}
