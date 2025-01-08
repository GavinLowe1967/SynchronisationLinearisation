package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatelessTester}
import synchronisationObject.{
  BarrierT,Barrier,Barrier2,FaultyBarrier,FaultyBarrier2,FaultyBarrier3}
import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree}
import scala.collection.mutable.ArrayBuffer

object BarrierTester extends Tester{
  /** The number of threads involved in each synchronisation. */
  var n = 4

  /** Number of worker threads to run.  Requires p%n == 0 if all are to
    * terminate. */
  // var p = 4

  // var iters = 5

  /** Do we check the progress condition? */
  // var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1
  
  /** Representation of an operation in the log. */
  case class Sync(id: Int)

  /** Is syncs sorted by id's? */
  def isSorted(syncs: List[Sync]): Boolean = 
    syncs.length <= 1 || syncs(0).id < syncs(1).id && isSorted(syncs.tail)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. Any n invocations can
    * synchronise, and all should receive the unit value.  We require the id's
    * to be in increasing order to reduce the number of cases by a factor of
    * n!. */
  def matching: PartialFunction[List[Sync], List[Unit]] = {
    case syncs if syncs.length == n && isSorted(syncs) => List.fill(n)(())
  }

  /** A worker, which calls barrier.sync. */
  def worker(barrier: BarrierT)(me: Int, log: HistoryLog[Sync]) = {
    // # iterations to perform; maybe one short if progressCheck (probability
    // 1/n).
    val iters1 = if(progressCheck && Random.nextInt(n) == 0) iters-1 else iters
    for(i <- 0 until iters1){
      // Thread.sleep(Random.nextInt(5))
      log(me, barrier.sync(me), Sync(me))
    }
  }

  // /** Is ops a suffix of a possible synchronisation?  I.e. in increasing order
  //   * of id fields? */
  // def suffixMatching(ops: List[Sync]) : Boolean = 
  //   if(ops.isEmpty) true
  //   else{
  //     var n = ops.head.id; var ops1 = ops.tail
  //     while(ops1.nonEmpty && n < ops1.head.id){
  //       n = ops1.head.id; ops1 = ops1.tail
  //     }
  //     ops1.isEmpty
  //   }

  /** Is ops a suffix of a possible synchronisation?  I.e. are the ids fields
    * of the form [k..p) for some k? */
  def suffixMatching(ops: List[Sync]) : Boolean = {
    assert(ops.nonEmpty); var k = ops.head.id; var ops1 = ops.tail
    while(ops1.nonEmpty && ops1.head.id == k+1){
      k = ops1.head.id; ops1 = ops1.tail
    }
    ops1.isEmpty && k == p-1
  }

  var faulty = false; var faulty2 = false; 
  var faulty3 = false; var version2 = false

  /** Run a single test. */
  def doTest = {
    val barrier: BarrierT = 
      if(version2) new Barrier2(n)
      else if(faulty) new FaultyBarrier(n)
      else if(faulty2) new FaultyBarrier2(n) 
      else if(faulty3) new FaultyBarrier3(n) 
      else{ 
        assert(p == n, 
          s"For standard barrier, require p = n, but p = $p, n = $n")
        new Barrier(n) 
      }
    if(progressCheck){
      val tester = new StatelessTester[Sync](
        worker(barrier), p, List(n), matching, suffixMatching/*isSorted*/, verbose = false)
      tester(timeout) // if(!tester(timeout)) sys.exit()
    }
    else{
      val tester = new StatelessTester[Sync](
        worker(barrier), p, List(n), matching, suffixMatching/*isSorted*/, verbose = false)
      tester() // if(!tester()) sys.exit()
    }
  }

  def main(args: Array[String]): Unit = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2

      case "--version2" => version2 = true; i += 1
      case "--faulty" => faulty = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1 // spurious wakeups; error not found 
      case "--faulty3" => faulty3 = true; i += 1 // deadlocks

      case "--timing" => timing = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2

      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    // assert(p%n == 0)
    if(p != n) println("Warning: different values for p and n")

    if(profiling){
      def filter(frame: StackTraceElement) : Boolean =
        SamplingProfiler.defaultFilter(frame) &&
          !frame.getClassName.contains("jdk.internal")
      def printer(data: ArrayBuffer[SamplingProfiler.StackTrace]) = {
        SamplingProfiler.print(filter = filter, length = 20)(data)+
        "\n"+
        SamplingProfiler.printTree(
          filter = filter, expand = ProfilerSummaryTree.expandToThreshold(0.5)
        )(data)
      }
      val profiler = new SamplingProfiler(interval = interval, print = printer)
      profiler{ runTests(reps, timing) }
    }
    else runTests(reps, timing)
  }

}
