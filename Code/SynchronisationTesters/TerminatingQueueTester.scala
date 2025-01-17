package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}
import synchronisationObject.{
  TerminatingQueueT,TerminatingQueue,FaultyTerminatingQueue}

import ox.gavin.profiling.{SamplingProfiler,ProfilerSummaryTree,Profiler}
import scala.collection.mutable.ArrayBuffer

import scala.collection.immutable.Queue

/** A tester for a terminating queue. */
object TerminatingQueueTester extends Tester{
  // /** The number of threads to run. */
  // var p = 4

  val MaxVal = 20

  // /** Do we check the progress condition? */
  // var progressCheck = false

  /** The type we test. */
  type TQueue = TerminatingQueueT[Int]

  /** The timeout time with the progress check. */
  var timeout = -1

  // Representation of operations
  trait Op
  case class Enqueue(x: Int) extends Op
  case class Dequeue(id: Int) extends Op

  /** The specification class.  queue represents the contents.  done is true if
    * we've reached termination. */
  case class Spec(queue: Queue[Int], done: Boolean){
    def enqueue(x: Int) = { // Profiler.count("enqueue")
      require(!done); (Spec(queue.enqueue(x), false), List(()))
    }

    def dequeue() = { // Profiler.count("dequeue")
      require(!done && queue.nonEmpty); val (x,q1) = queue.dequeue
      (Spec(q1, false), List(Some(x)))
    }

    def terminate() = { // Profiler.count("terminate")
      require(!done && queue.isEmpty); (Spec(queue, true), allNone)
    }
  }

  // Set to List.fill(p)(None) by doTest
  private var allNone: List[Option[Int]] = null

    
  /** Are all members of ops Dequeues and in increasing order? */
  private def allDequeues(ops: List[Op]): Boolean = 
    ops match{
      case List(Dequeue(_)) => true
      case Dequeue(id1) :: Dequeue(id2) :: _ => 
        id1 < id2 && allDequeues(ops.tail)
      case _ => false
    }
  // ops.forall(_ == Dequeue)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[List[Op], (Spec,List[Any])] = {
    case List(Enqueue(x)) => spec.enqueue(x)
    case List(Dequeue(_)) => spec.dequeue()
    case ops if ops.length == p && allDequeues(ops) => spec.terminate()
  }

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]) = ops.length == 1 || allDequeues(ops)

  // var iters = 0

  def addToIters(n: Int) = synchronized{ iters += n }

  val dequeueProb = 0.5

  def worker(queue: TQueue)(me: Int, log: HistoryLog[Op]) = {
    // Even-numbered workers always dequeue; odd-numbered workers randomly
    // choose whether to enqueue or dequeue (dequeueing with probability
    // dequeueProb).
    val random = new Random(me) // independent RNGs
    var done = false; var myIters = 0
    while(!done){
      myIters += 1
      // Profiler.count("worker step")
      if(me%2 == 0 ||  random.nextFloat() < dequeueProb){
        // println(s"$me: dequeue")
        var res: Option[Int] = None
        log(me, { res = queue.dequeue; res }, Dequeue(me))
        done = res == None; 
        // Profiler.count(s"dequeue $done")
      }
      else{ 
        val x = Random.nextInt(MaxVal)
        log(me, queue.enqueue(x), Enqueue(x))
        // Profiler.count("Thread enqueue") curiously, ~45% of invocations
        // take this branch (with dequeueProb = 0.5), when one would expect it
        // to be ~25%.  Maybe even-numbered threads spend longer waiting in
        // dequeue.
      }
    } // end of while loop
    addToIters(myIters)
  }

  var verbose = false
  var doASAP = false
  var faulty = false

  /** Run a single test. */
  def doTest = {
    allNone = List.fill(p)(None)
    val queue: TQueue =
      if(faulty) new FaultyTerminatingQueue[Int](p)
      else new TerminatingQueue[Int](p)
    val spec = Spec(Queue[Int](), false)
    val tester = new StatefulTester[Op,Spec](
      worker(queue) _, p, List(1,p), matching _, suffixMatching _, 
      spec, doASAP, verbose)
    // The progressCheck should make no difference here, since each run
    // terminates with probability 1.
    if(progressCheck) tester(timeout) // { if(!tester(timeout)) sys.exit() }
    else tester() // if(!tester()) sys.exit()
  }

  def main(args: Array[String]): Unit = {
    // Parse arguments
    var reps = 5000; var i = 0; var interval = 50; var profiling = false
    var timing = false; var itersBound = -1
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      //case "-n" => n = args(i+1).toInt; i += 2
      // case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--verbose" => verbose = true; i += 1
      case "--doASAP" => doASAP = true; i += 1
      case "--timing" => timing = true; i += 1
      case "--iters" => i += 2 // Ignore this flag!
      case "--untilIters" => itersBound = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    
    def run() = {
      if(itersBound < 0) runTests(reps, timing)
      //   for(i <- 0 until reps){ doTest; if(i%100 == 0) print(".") }
      //   println()
      // }
      else{
        val start = java.lang.System.nanoTime; var i = 0
        while(iters < itersBound && doTest){ i += 1 }
        println(s"\n$i runs")
        if(timing){
          val duration = java.lang.System.nanoTime - start // nanos
          println(); println(duration)
        }
        else{
          val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
          println(); println(s"$duration ms")
        }
      }
    }
    // val start = java.lang.System.nanoTime
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
      profiler{ run()  }
    }
    else run()
    // if(timing){
    //   val duration = java.lang.System.nanoTime - start // nanos
    //   println(); println(duration)
    // }
    // else{
    //   val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    //   println(); println(s"$duration ms")
    //   Profiler.report
    // }
  }

}
