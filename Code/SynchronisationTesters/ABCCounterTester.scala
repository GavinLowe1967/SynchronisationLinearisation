package synchronisationTester
 
import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}
import synchronisationObject.{ABCCounterT,ABCCounter,FaultyABCCounter}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

/** A tester for the ABC synchronisation problem with a sequence counter. */
object ABCCounterTester{
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
  type Result = (Int,Int,Int)

  /** The specification class. */
  class Spec(val counter: Int = 0){
    /** Each of a, b, c get the identities of the other two and the value of
      * counter; the state of the specification object is incremented. */
    def sync(a: Int, b: Int, c: Int): (Spec, List[Result]) = {
      (new Spec(counter+1), List((b,c,counter), (a,c,counter), (a,b,counter)))
    }

    override def equals(that: Any) = that match{
      case spec: Spec => spec.counter == counter
    }
    override def hashCode = counter
    override def toString = s"Spec($counter)"
  } // end of spec

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]): Boolean = ops match{
    case List(SyncC(c)) => true
    case List(SyncB(b), SyncC(c)) => true
    case List(SyncA(a), SyncB(b), SyncC(c)) => true
    case _ => false
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[List[Op], (Spec,List[Result])] = {
    case List(SyncA(a), SyncB(b), SyncC(c)) => spec.sync(a, b, c) 
  }

  /** A worker with identity me. */
  def worker(abc: ABCCounterT[Int,Int,Int])(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters){
      if(me%3 == 0) log(me, abc.syncA(me), SyncA(me)) 
      else if(me%3 == 1) log(me, abc.syncB(me), SyncB(me)) 
      else log(me, abc.syncC(me), SyncC(me))
    }
  }

  var verbose = false
  var faulty = false
  var doASAP = false

  /** Do a single test. */
  def doTest = {
    val abc: ABCCounterT[Int,Int,Int] =
      if(faulty) new FaultyABCCounter[Int,Int,Int] 
      else new ABCCounter[Int,Int,Int]
    val spec = new Spec
    val tester = new StatefulTester[Op,Spec](
      worker(abc) _, p, List(3), matching _, suffixMatching _, 
      spec, doASAP, verbose)
    if(!tester()) sys.exit()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--doASAP" => doASAP = true; i += 1
      case "--faulty" => faulty = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%3 == 0)

    def run() = {
      for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
      println()
    }
    val start = java.lang.System.nanoTime
    if(profiling){
      def filter(frame: StackTraceElement) : Boolean =
        SamplingProfiler.defaultFilter(frame) &&
          !frame.getClassName.contains("jdk.internal") &&
          !frame.getClassName.contains("io.threadcso.semaphore")
      // val printer = 
      def printer(data: ArrayBuffer[SamplingProfiler.StackTrace]) = {
        SamplingProfiler.print(filter = filter, length = 30)(data)+"\n"+
        SamplingProfiler.printTree(
          filter = filter, expand = ProfilerSummaryTree.expandToThreshold(0.05)
        )(data)
      }
      val profiler = new SamplingProfiler(interval = interval, print = printer)
      profiler{ run() }
    }
    else run()
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }

}
