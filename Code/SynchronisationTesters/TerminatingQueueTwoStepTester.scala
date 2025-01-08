package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationObject.{TerminatingQueueT,TerminatingQueue}

import scala.util.Random
import scala.collection.immutable.Queue

/** Two-step linearisation synchronisation tester for a terminating queue. */
object TerminatingQueueTwoStepTester extends Tester{

  val MaxVal = 20

  /** The type of objects tested. */
  type TQueue = TerminatingQueue[Int]

  type ThreadID = Int

  /** State of the specification object. */
  trait State

  /** A state before termination starts, where `queue` records the state of the
    * queue. */
  case class Zero(val queue: Queue[Int]) extends State{
    // Note: we need to override hashCode and equals, because Queue uses
    // reference hash codes and equality.

    override def hashCode = queue.foldLeft(0)((h1,x) => h1*17+x)

    override def equals(that: Any) = that match{
      case st: Zero => queue.sameElements(st.queue) 
      case _: One => false
    }
  }

  /** A state after termination starts, where numWaiting records the number of
    * threads that have performed an unsuccessful dequeue1. */
  case class One(numWaiting: Int) extends State

  /** The specification object. */
  class TerminatingQueueSpec(state: State){
    def this() = this(Zero(Queue[Int]()))

    /** Operation corresponding to a concrete enqueue. */
    def enqueue(x: Int): (Unit, TerminatingQueueSpec) = {
      require(state.isInstanceOf[Zero]); val Zero(queue) = state
      ((), new TerminatingQueueSpec(Zero(queue.enqueue(x))))
    }

    /** A successful dequeue, or the first part of a terminating dequeue. */
    def dequeue1(id: ThreadID): (Option[Int], TerminatingQueueSpec) = 
      state match{
        case Zero(queue) => 
          if(queue.nonEmpty){ // normal dequeue
            val (x, queue1) = queue.dequeue
            (Some(x), new TerminatingQueueSpec(Zero(queue1)))
          }
          else
            // if the dequeue is linearised here, it's the start of termination
            (None, new TerminatingQueueSpec(One(1)))
        case One(numWaiting) => 
          assert(numWaiting < p)
          (None, new TerminatingQueueSpec(One(numWaiting+1)))
      }

    /** Second part of a terminating dequeue. */
    def dequeue2(id: ThreadID): (Unit, TerminatingQueueSpec) = {
      // Can linearise once all threads have done their dequeue1.
      val One(numWaiting) = state; require(numWaiting == p); ((), this)
    }      
  }

  def addToIters(n: Int) = synchronized{ iters += n }

  val dequeueProb = 0.5

  def worker(me: Int, log: LinearizabilityLog[TerminatingQueueSpec,TQueue]) = {
    // Even-numbered workers always dequeue; odd-numbered workers randomly
    // choose whether to enqueue or dequeue (dequeueing with probability
    // dequeueProb).
    val random = new Random(me) // independent RNGs
    var done = false; var myIters = 0
    while(!done){
      myIters += 1
      if(me%2 == 0 ||  random.nextFloat() < dequeueProb){
        var res: Option[Int] = None
        log(q => { res = q.dequeue; res }, s"dequeue", _.dequeue1(me))
        if(res == None){
          log(q => (), s"dequeue2", _.dequeue2(me))
          done = true
        }
      }
      else{ 
        val x = Random.nextInt(MaxVal)
        log(_.enqueue(x), s"$me: enqueue($x)", _.enqueue(x))
      }
    } // end of while loop
    addToIters(myIters)
  }

  /** Do a single test. */
  def doTest: Boolean = {
    val queue = new TerminatingQueue[Int](p)
    val spec = new TerminatingQueueSpec()
    val tester = LinearizabilityTester[TerminatingQueueSpec,TQueue](
      spec, queue, p, worker _)
    tester() > 0
  }

  /** Test until at least itersBounds iterations have been performed. */
  def testFor(itersBound: Int, timing: Boolean) = {
    val start = java.lang.System.nanoTime; var i = 0
    while(iters < itersBound && doTest){ i += 1; if(i%100 == 0) print(".") }
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
 
  def main(args: Array[String]): Unit = {
    // Parse arguments
    var reps = 5000; var i = 0
    // ; var interval = 50; var profiling = false
    var timing = false; var itersBound = -1
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1
      case "--iters" => i += 2 // Ignore this flag!
      case "--untilIters" => itersBound = args(i+1).toInt; i += 2
      // case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    if(itersBound > 0) testFor(itersBound, timing) else runTests(reps, timing)
  }
}
