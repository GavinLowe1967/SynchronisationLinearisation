import io.threadcso._
import scala.util.Random

import synchronisationTesting.{HistoryLog,ThreadUtil}

/** A testing file. */
object ChanTester{
  // Number of threads
  val p = 4

  // Number of iterations per thread
  val iters = 4

  // The maximum value sent.  A larger value will make it easier to test
  // whether a matching exists.  And the implementation is data independent,
  // so this will not affect the likelihood of finding errors.
  val MaxVal = 2

  // The type of the synchronisation object we are testing.
  type C = Chan[Int]

  // Representation of operations within the log
  trait Op
  case class Send(x: Int) extends Op
  case class Receive(u: Unit) extends Op

  /** The specification class. */
  object Spec{
    def sync(x: Int, u: Unit) = ((), x)
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[(Op,Op), (Any,Any)] = {
    case (Send(x), Receive(_)) => Spec.sync(x, ()) 
  }

  /** A worker. */
  def worker(me: Int, c: C, log: HistoryLog[Op]) = {
    for(i <- 0 until iters)
      if(me%2 == 0){
        val x = Random.nextInt(MaxVal)
        log(me, c!x, Send(x))
      }
      else log(me, c?(), Receive(()))
  }

  import HistoryLog.{Event,CallEvent,ReturnEvent}

  // /** Can the invocations corresponding to ce1 and ce2 synchronise? */
  // private def canSync(ce1: CallEvent[Op,_], ce2: CallEvent[Op,_]): Boolean = {
  //   // Each is called before the other returns
  //   ce1.index < ce2.ret.index && ce2.index < ce1.ret.index &&
  //   matching.isDefinedAt(ce1.op, ce2.op) &&
  //   (ce1.ret.result, ce2.ret.result) == matching(ce1.op, ce2.op)
  // }

  /** Find which invocations could synchronise. 
    * @return a pair (isOp1, syncs) such that: isOp1 is a bitmap showing which
    * invocations correspond to the first operation; and syncs gives, for each
    * invocation, which other invocations it could synchronise with. */
  // private def findMatches(events: Array[Event])
  //     : (Array[Boolean], Array[List[Int]]) = {
  //   require(events.length%4 == 0); val numInvs = events.length/2
  //   // Find call  events.
  //   val calls = new Array[CallEvent[Op,_]](numInvs); var i = 0
  //   for(j <- 0 until events.length) events(j) match{
  //     case ce: CallEvent[Op,_] @unchecked => calls(i) = ce; i += 1
  //     case _ => {}
  //   }
  //   // Bitmaps showing whether each of calls is the first or second operation.
  //   val isOp1, isOp2 = new Array[Boolean](numInvs)
  //   // Array indicating whether calls(i) can synchronise with calls(j)
  //   // val candidates = Array.ofDim[Boolean](numInvs,numInvs)
  //   // Calls with which other calls each call could synchronise with
  //   val syncs = Array.fill(numInvs)(List[Int]())
  //   for(i <- 0 until numInvs; j <- 0 until numInvs)
  //     if(i != j && canSync(calls(i), calls(j))){
  //       // candidates(i)(j) = true; 
  //       syncs(i)::= j; syncs(j)::= i
  //       isOp1(i) = true; isOp2(j) = true
  //     }
  //   // Each invocation should be an op1 or op2, but not both
  //   for(i <- 0 until numInvs) assert(isOp1(i) ^ isOp2(i))

  //   // println
  //   for(i <- 0 until numInvs) 
  //     println(s"$i:\t"+calls(i).toString+"; returns "+calls(i).ret.result)
  //   // for(i <- 0 until numInvs) println(s"$i:\t"+candidates(i).mkString("\t"))
  //   // for(i <- 0 until numInvs) println(s"$i:\t"+syncs(i))
  //   (isOp1, syncs)
  // }

  /** Do a single test. */
  def doTest = {
    val bst = new synchronisationTesting.BinaryStatelessTester[Op](matching)
    val log = new HistoryLog[Op](p)
    val c = ManyMany[Int]
    ThreadUtil.runIndexedSystem(p, i => worker(i, c, log))
    val events = log.get
    // println(events.mkString("\n"))
    val (isOp1, syncs) = bst.findMatches(events)
    val ff = new synchronisationTesting.FordFulkerson(isOp1, syncs)
    if(ff()) println("Succeeded") else println("Failed")
    ()
    // TODO: analyse the log
  }

  def main(args: Array[String]) = {
    doTest
  }


}
