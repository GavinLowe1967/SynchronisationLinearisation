package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

/** A tester for a barrier synchronisation with a sequence counter. */
object BarrierCounterTester{
  /** The number of threads involved in each synchronisation. */
  var n = 3

  /** Number of worker threads to run.  Requires p%n == 0. */
  var p = 12

  // /** Number of iterations per worker thread. */
  // var iters = 20

  // Representation of operations within the log
  case class Sync()

  /** The specification class. */
  case class Spec(val counter: Int = 0){
    /** Each of the n threads gets the value of counter; the counter is
      * incremented. */
    def sync(syncs: List[Sync]) = (new Spec(counter+1), List.fill(n)(counter))
  }

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(syncs: List[Sync]) = true

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. Any n invocations can
    * synchronise, and all should receive the value of the counter.  */
  def matching(spec: Spec): PartialFunction[List[Sync], (Spec,List[Int])] = {
    case syncs if syncs.length == n => spec.sync(syncs)
  }

  /** A worker, which calls barrier.sync once. */
  def worker(barrier: BarrierCounterT)(me: Int, log: HistoryLog[Sync]) = {
    //Thread.sleep(Random.nextInt(20))
    log(me, barrier.sync, Sync())
  }
  
  var verbose = false
  var faulty = false
  var version2 = false
  var doASAP = false

  /** Run a single test. */
  def doTest = {
    val barrier: BarrierCounterT = 
      if(version2) new BarrierCounter2(n) else new BarrierCounter(n)
    val spec = new Spec
    val tester = new StatefulTester[Sync,Spec](
      worker(barrier) _, p, List(3), matching _, suffixMatching _, 
      spec, doASAP, verbose)
    if(!tester()) sys.exit 
  }


  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      // case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--doASAP" => doASAP = true; i += 1
      // case "--faulty" => faulty = true; i += 1
      case "--version2" => version2 = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
    assert(p%n == 0)


    def run() = {
      for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
      println
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
    println; println(s"$duration ms")
  }




}
