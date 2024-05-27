package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatefulTester}
import synchronisationObject.{ResignableBarrierT, ResignableBarrier}

import scala.collection.immutable.HashSet

/** A tester for ResignableBarriers. */
object ResignableBarrierTester{
  // We will use non-negative Ints to represent thread identities.

  /** The type of barriers. */
  type Barrier = ResignableBarrierT[Int]

  /** Type of the set of threads currently enrolled. */
  type Enrolled = HashSet[Int]

  /** Operations. */
  abstract class Op
  case class Enrol(id: Int) extends Op
  case class Resign(id: Int) extends Op
  case class Sync(id: Int) extends Op

  /** Does `ops` represent a suffix of a possible synchronisation? */
  def suffixMatching(ops: List[Op]) =
    ops.length <= 1 || suffixMatching1(ops, 0)
      
  /** Is ops a list of Sync values, with increasing values of id, all at least
    * n? */
  def suffixMatching1(ops: List[Op], n: Int): Boolean = 
    if(ops.isEmpty) true
    else ops.head match{
      case Sync(m) if m >= n => suffixMatching1(ops.tail, m+1)
      case _ => false
    }

  /** The specification class.
    * @param enrolled the set of threads currently enrolled.  */
  case class Spec(enrolled: Enrolled){
    def enrol(id: Int) = { 
      assert(!enrolled.contains(id)); (new Spec(enrolled + id), List(()))
    }

    def resign(id: Int) = {
      assert(enrolled.contains(id)); (new Spec(enrolled - id), List(()))
    }

    /** The list of Sync objects that would correspond to a synchronisation in 
      * the current state. */
    def getSyncs: List[Sync] = enrolled.toList.sorted.map(Sync)

    /** Each of the n threads gets the value of counter; the counter is
      * incremented. */
    def sync(syncs: List[Op]) = {
      val n = enrolled.size; assert(syncs.length == n)
      (this, List.fill(n)(()))
    }
  } // end of Spec

  /** Partial function showing how a list of invocations can synchronise, and
    * returning the expected next state and list of return values. */ 
  def matching(spec: Spec): PartialFunction[List[Op], (Spec, List[Unit])] = {
    ops => ops match{
      case List(Enrol(id)) => spec.enrol(id)
      case List(Resign(id)) => spec.resign(id)
      case syncs if syncs == spec.getSyncs => spec.sync(syncs)
    }
  }

  /** Number of iterations per worker. */
  var iters = 10

  var syncProb = 0.7

  def worker(barrier: Barrier)(me: Int, log: HistoryLog[Op]) = {
    var enrolled = false
    for(i <- 0 until iters){
      if(enrolled){
        if(Random.nextFloat() < syncProb) log(me, barrier.sync(me), Sync(me))
        else{ log(me, barrier.resign(me), Resign(me)); enrolled = false }
      }
      else{ log(me, barrier.enrol(me), Enrol(me)); enrolled = true }
    }
    // Resign at the end, to avoid deadlocks
    if(enrolled) log(me, barrier.resign(me), Resign(me))
  }

  var p = 4 // # workers
  var verbose = false
  var doASAP = false

  /** Do a single test. */
  def doTest() = {
    val barrier = new ResignableBarrier[Int]
    val spec = new Spec(new Enrolled)
    val tester = new StatefulTester[Op,Spec](
      worker(barrier) _, p, (1 to p).toList, matching _, suffixMatching _,
      spec, doASAP, verbose)
    if(!tester()) sys.exit()
  }

  def main(args: Array[String]) = {
    val reps = 10000

    for(i <- 0 until reps){
      doTest()
      if(i%100 == 0) print(".")
    }
    println()
  }

}
