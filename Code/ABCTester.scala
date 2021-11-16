package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatelessTester}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

/** A tester for the ABC synchronisation problem. */
object ABCTester{
  /** Number of worker threads to run. */
  var p = 6

  /** Number of iterations per worker thread. */
  var iters = 20

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

  /** A worker with identity me. */
  def worker(abc: ABCT[Int,Int,Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      if(me%3 == 0) log(me, abc.syncA(me), SyncA(me)) 
      else if(me%3 == 1) log(me, abc.syncB(me), SyncB(me)) 
      else log(me, abc.syncC(me), SyncC(me))
    }
  }

  var faulty = false

  def doTest = {
    val abc: ABCT[Int,Int,Int] = 
      if(faulty) new FaultyABC[Int,Int,Int] else new ABC[Int,Int,Int]
    val tester = new StatelessTester[Op](worker(abc) _, p, List(3), matching)
    if(!tester()) sys.exit
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      //case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1
      // case "--faulty2" => faulty2 = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
    assert(p%3 == 0)

    def run() = {
      for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
      println
    }
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
      profiler{ run() }
    }
    else run()
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println; println(s"$duration ms")
  }


}
