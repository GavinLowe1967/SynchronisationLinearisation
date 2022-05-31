package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}
import synchronisationObject.{
  BarrierCounterT, BarrierCounter, FaultyBarrierCounter,
  BarrierCounter2, BarrierCounter3}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree,Profiler}
import scala.collection.mutable.ArrayBuffer

/** A tester for a barrier synchronisation with a sequence counter. */
object BarrierCounterTester{
  /** The number of threads involved in each synchronisation. */
  var n = 3

  /** Number of worker threads to run. */
  var p = 12

  var iters = 3

  /** Do we check the progress condition? */
  var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1

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
  var version3 = false
  var doASAP = false

  /** Run a single test. */
  def doTest = {
    val barrier: BarrierCounterT = 
      if(faulty) new FaultyBarrierCounter(n)
      else if(version2) new BarrierCounter2(n)
      else if(version3) new BarrierCounter3(n)
      else new BarrierCounter(n)
    val spec = new Spec
    if(progressCheck){
      val tester = new StatefulTester[Sync,Spec](
        worker(barrier) _, p, List(n), matching _, suffixMatching _,
        spec, doASAP, verbose)
      if(!tester(timeout)) sys.exit()
    }
    else{
      val tester = new StatefulTester[Sync,Spec](
        worker(barrier) _, p, List(n), matching _, suffixMatching _,
        spec, doASAP, verbose)
      if(!tester()) sys.exit()
    }
  }


  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "-n" => n = args(i+1).toInt; i += 2
      // case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--doASAP" => doASAP = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--version2" => version2 = true; i += 1
      case "--version3" => version3 = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%n == 0, s"Requires p%n == 0.  p = $p, n = $n")


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
        SamplingProfiler.print(filter = filter, length = 20)(data)+"\n"+
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
    Profiler.report
  }

  /* Note, this works well for small values of n.  But the number of
   * synchronisations considered within StatefulTester.allSyncs grows as
   * O(n!). */
}
