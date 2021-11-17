package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}
import synchronisationObject.{TerminatingQueueT,TerminatingQueue}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree,Profiler}
import scala.collection.mutable.ArrayBuffer

import scala.collection.immutable.Queue

/** A tester for a terminating queue. */
object TerminatingQueueTester{
  /** The number of threads to run. */
  var p = 4

  val MaxVal = 20

  // Representation of operations
  trait Op
  case class Enqueue(x: Int) extends Op
  case object Dequeue extends Op

  /** The specification class.  queue represents the contents.  done is true if
    * we've reached termination. */
  case class Spec(queue: Queue[Int], done: Boolean){
    def enqueue(x: Int) = {Profiler.count("enqueue")
      require(!done); (Spec(queue.enqueue(x), false), List(()))
    }

    def dequeue() = {Profiler.count("dequeue")
      require(!done && queue.nonEmpty); val (x,q1) = queue.dequeue
      (Spec(q1, false), List(Some(x)))
    }

    def terminate() = { Profiler.count("terminate")
      require(!done && queue.isEmpty); (Spec(queue, true), allNone)
    }
  }

  // Set to List.fill(p)(None) by doTest
  private var allNone: List[Option[Int]] = null

  // private def allNone(k: Int): List[Option[Int]] = 
  //   if(k == 0) List() else None :: allNone(k-1)
    
  /** Are all members of ops Dequeues? */
  private def allDequeues(ops: List[Op]) = ops.forall(_ == Dequeue)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[List[Op], (Spec,List[Any])] = {
    case List(Enqueue(x)) => spec.enqueue(x)
    case List(Dequeue) => spec.dequeue()
    case ops if ops.length == p && allDequeues(ops) => spec.terminate()
  }

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]) = ops.length == 1 || allDequeues(ops)

  def worker(queue: TerminatingQueueT[Int])(me: Int, log: HistoryLog[Op]) = {
    // Even-numbered workers always dequeue; odd-numbered workers randomly
    // choose whether to enqueue or dequeue (50% dequeues).
    val random = new Random(me) // independent RNGs
    var done = false
    while(!done){
      Profiler.count("worker step")
      if(me%2 == 0 || random.nextFloat < 0.50){
        // println(s"$me: dequeue")
        var res: Option[Int] = None
        log(me, { res = queue.dequeue; res }, Dequeue)
        done = res == None; // println(s"$me: $res")
      }
      else{
        val x = Random.nextInt(MaxVal)
        // println(s"$me: enqueue($x)")
        log(me, queue.enqueue(x), Enqueue(x))
      }
    }
  }

  var verbose = false
  var doASAP = false

  /** Run a single test. */
  def doTest = {
    allNone = List.fill(p)(None)
    val queue = new TerminatingQueue[Int](p)
    val spec = Spec(Queue[Int](), false)
    val tester = new StatefulTester[Op,Spec](
      worker(queue) _, p, List(1,p), matching _, suffixMatching _, 
      spec, doASAP, verbose)
    if(!tester()) sys.exit
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      //case "-n" => n = args(i+1).toInt; i += 2
      // case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--doASAP" => doASAP = true; i += 1
      //case "--faulty" => faulty = true; i += 1
      //case "--version2" => version2 = true; i += 1
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit
    }
    
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
    println; println(s"$duration ms")
    Profiler.report
  }

}
