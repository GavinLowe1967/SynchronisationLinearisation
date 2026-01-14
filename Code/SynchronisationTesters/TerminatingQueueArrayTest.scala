package synchronisationTester

import synchronisationTesting.{HistoryLog,NondetStatefulTester}
import synchronisationObject.{
  TerminatingQueueArrayT, TerminatingQueueArray, FaultyTerminatingQueueArray}

import scala.util.Random
import scala.collection.immutable.Queue

object TerminatingQueueArrayTest extends Tester{
  /** Range of values enqueued. */
  val MaxVal = 20

  /** The type of objects being tested. */
  type TQA = TerminatingQueueArrayT[Int]  

  // Representation of operations
  trait Op
  case class Enqueue(id: Int, x: Int) extends Op
  case class Dequeue(id: Int) extends Op

  /** The specification class.  queue represents the contents.  `done` is true
    * if we've reached termination. */
  case class Spec(queues: Array[Queue[Int]], done: Boolean){
    assert(queues.length == p)

    def enqueue(me: Int, x: Int): List[(Spec,List[Unit])] = {
      require(!done); val q1 = queues(me).enqueue(x)
      List( (Spec(update(me, q1), false), List(())) )
    }

    def dequeue(me: Int): List[(Spec, List[Option[Int]])] = {
      require(!done && !queues.forall(_.isEmpty))
      // Can dequeue from any nonempty queue. 
      for(ix <- (0 until p).toList; if queues(ix).nonEmpty) yield{
        val (x,q1) = queues(ix).dequeue
        (Spec(update(ix, q1), false), List(Some(x)))
      }
    }

    def terminate() = { // Profiler.count("terminate")
      require(!done && queues.forall(_.isEmpty))
      List( (Spec(queues, true), List.fill(p)(None)) ) 
    }

    /** A new array of queues, updating ix to q. */ 
    private def update(ix: Int, q: Queue[Int]): Array[Queue[Int]] = 
      Array.tabulate(p)(i => if(i == ix) q else queues(i))

    override def hashCode = {
      var h = if(done) 1 else 0
      for(ix <- 0 until p; x <- queues(ix)) h = 2*h + x
      h
    }

    override def equals(other: Any) = other match{
      case Spec(qs1, d1) => 
        d1 == done && (0 until p).forall(ix => qs1(ix).sameElements(queues(ix)))
    }
  }

  /** Are all members of ops Dequeues and in increasing order? */
  private def allDequeues(ops: List[Op]): Boolean = 
    ops match{
      case List(Dequeue(_)) => true
      case Dequeue(id1) :: Dequeue(id2) :: _ => 
        id1 < id2 && allDequeues(ops.tail)
      case _ => false
    }

  /** Does ops represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]) = ops.length == 1 || allDequeues(ops)

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching(spec: Spec): PartialFunction[List[Op], List[(Spec,List[Any])]] = {
    case List(Enqueue(id,x)) => spec.enqueue(id,x)
    case List(Dequeue(id)) => spec.dequeue(id)
    case ops if ops.length == p && allDequeues(ops) => spec.terminate()
  }

  val dequeueProb = 0.7

  /** A single worker. */
  def worker(qa: TQA)(me: Int, log: HistoryLog[Op]) = {
    val random = new Random(me); var done = false // independent RNGs
    while(!done){
      if(random.nextFloat() < dequeueProb){
        var res: Option[Int] = None
        log(me, { res = qa.dequeue(me); res }, Dequeue(me))
        done = res == None
      }
      else{ 
        val x = random.nextInt(MaxVal)
        log(me, qa.enqueue(me, x), Enqueue(me, x))
      }
    } // end of while loop
  }

  var faulty = false
  var doASAP = false


  def doTest: Boolean = {
    val qa: TQA = 
      if(faulty) new FaultyTerminatingQueueArray[Int](p)
      else new TerminatingQueueArray[Int](p)
    val spec = Spec(Array.fill(p)(Queue[Int]()), false)
    val tester = new NondetStatefulTester[Op,Spec](
      worker(qa) _, p, List(1,p), matching _, suffixMatching _, spec, 
      doASAP = doASAP, false)
    tester() 
  }

  def main(args: Array[String]): Unit = {
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1
      case "--doASAP" => doASAP = true; i += 1 // Seems to help
    }

    var r = 0; val start = java.lang.System.nanoTime
    while(r < reps && doTest){ if(r%100 == 0) print("."); r += 1 }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}

