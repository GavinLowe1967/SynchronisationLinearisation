package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog, StatelessTester}
import synchronisationObject.{ABCT, ABC, FaultyABC, DeadlockABC}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

/** A tester for the ABC synchronisation problem. */
object ABCTester extends Tester{
  /* Number of worker threads to run.  Needs to be a multiple of 3 here. */
  p = 6

  /** The timeout time with the progress check. */
  var timeout = -1

  // Representation of operations within the log
  trait Op 
  case class SyncA(id: Int) extends Op
  case class SyncB(id: Int) extends Op
  case class SyncC(id: Int) extends Op

  // The result type of an invocation.
  type IntPair = (Int,Int)

  /** The specification class. */
  object Spec{
    // Each of a, b, c get the identities of the other two
    def sync(a: Int, b: Int, c: Int) = List((b,c), (a,c), (a,b))
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[List[Op], List[IntPair]] = {
    case List(SyncA(a), SyncB(b), SyncC(c)) => Spec.sync(a, b, c) 
  }

  /** Is ops a suffix of a possible synchronisation?  Including this seems to
    * make very little difference. */
  def suffixMatching(ops: List[Op]) = ops match{
    case List() => true
    case List(SyncC(_)) => true
    case List(SyncB(_), SyncC(_)) => true
    case List(SyncA(_), SyncB(_), SyncC(_)) => true
    case _ => true
  }

  /** A worker with identity me. */
  def worker(abc: ABCT[Int,Int,Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      if(me%3 == 0) log(me, abc.syncA(me), SyncA(me)) 
      else if(me%3 == 1) log(me, abc.syncB(me), SyncB(me)) 
      else log(me, abc.syncC(me), SyncC(me))
    }
  }

  /** A worker with identity me who chooses invocations randomly. */
  def worker1(abc: ABCT[Int,Int,Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters) Random.nextInt(3) match{
      case 0 => log(me, abc.syncA(me), SyncA(me)) 
      case 1 => log(me, abc.syncB(me), SyncB(me)) 
      case 2 => log(me, abc.syncC(me), SyncC(me))
    }
  }

  /* Flags for which implementation to use.  Default is ABC */
  var faulty = false // FaultyABC
  var deadlock = false // DeadlockABC

  def doTest = {
    val abc: ABCT[Int,Int,Int] = 
      if(faulty) new FaultyABC[Int,Int,Int] 
      else if(deadlock) new DeadlockABC[Int,Int,Int]
      else new ABC[Int,Int,Int]
    if(progressCheck){
      val tester = new StatelessTester[Op](worker1(abc) _, p, List(3), matching)
      tester(timeout) // if(!tester(timeout)) sys.exit()
    }
    else{
      val tester = new StatelessTester[Op](
        worker(abc), p, List(3), matching /*, suffixMatching */)
      tester() // if(!tester()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false; // var countReps = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--timing" => timing = true; i += 1
      // case "--countReps" => countReps = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%3 == 0)

    val start = java.lang.System.nanoTime
    if(profiling){
      def filter(frame: StackTraceElement) : Boolean =
        SamplingProfiler.defaultFilter(frame) &&
          !frame.getClassName.contains("jdk.internal")
      // val printer = 
      def printer(data: ArrayBuffer[SamplingProfiler.StackTrace]) = {
        SamplingProfiler.print(filter = filter, length = 40)(data)+"\n"+
        SamplingProfiler.printTree(
          filter = filter, expand = ProfilerSummaryTree.expandToThreshold(0.1)
        )(data)
      }
      val profiler = new SamplingProfiler(interval = interval, print = printer)
      profiler{ runTests(reps, timing /*, countReps*/) }
    }
    else  runTests(reps, timing /*, countReps*/ )
    ()
  }


}
