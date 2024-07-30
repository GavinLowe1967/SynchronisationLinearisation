package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

package object LinABCCounterGlobal {
// No of threads. Must be a multiple of 3.
  val p = 12
}

// Common signature between concurrent and sequential implementations.
trait LinABCCounterT {
  def aSync(id: Int, value: Int): Unit
  // id is thread number, value is "result".
  def aExit(id: Int): (Int, Int, Int)
  // id is needed in the log to check the a gets the correct result.
  def bSync(id: Int, value: Int): Unit
  // id is thread number, value is "result".
  def bExit(id: Int): (Int, Int, Int)
  // id is needed in the log to check the b gets the correct result.
  def cSync(value: Int): (Int, Int, Int)
}

class ConcABCCounter(faulty: Boolean) extends LinABCCounterT {

  val abc =
    if (faulty) new FaultyABCCounter[Int, Int, Int]
    else
      new ABCCounter[
        Int,
        Int,
        Int
      ] // Datatype to test.  Note - it is unmodified.
  val results = // alternatively could use ThreadLocal.
    Array.ofDim[(Int, Int, Int)](LinABCCounterGlobal.p)

  // results array does not need to be protected from concurrent access,
  // because elements are only written and read by the owning thread.
  def aSync(id: Int, value: Int): Unit = { results(id) = abc.syncA(value); }
  def aExit(id: Int): (Int, Int, Int) = results(id)
  def bSync(id: Int, value: Int): Unit = { results(id) = abc.syncB(value); }
  def bExit(id: Int): (Int, Int, Int) = results(id)
  def cSync(value: Int): (Int, Int, Int) = abc.syncC(value)
}

object ABCCounterLinTest {
  var iters = 20 // Number of iterations by each worker
  val MaxVal = 10 // Maximum value placed in the queue
  val reps = 100

  // Type representing state of sequential ABCCounter system.
  // This could be a class, but here we use the common idiom of defining a tuple type.
  type SeqABCCounter = (Int, Int, Int, Int, Int, Int, Array[(Int, Int, Int)])
  // (state,counter,aid,aval,bid,bval,results)

// cSync() must be the last of 3 operations so must occur in state 2.
// The other two may occur in any order.
// We define bSync() to occur first, just to be different from the concurrent monitor implemention.

  def seqASync(id: Int, value: Int)(
      abc: SeqABCCounter
  ): (Unit, SeqABCCounter) = {
    val (state, counter, aid, aval, bid, bval, results) = abc
    require(state == 1)
    ((), (2, counter, id, value, bid, bval, results))
  }

  def seqBSync(id: Int, value: Int)(
      abc: SeqABCCounter
  ): (Unit, SeqABCCounter) = {
    val (state, counter, aid, aval, bid, bval, results) = abc
    require(state == 0)
    ((), (1, counter, aid, aval, id, value, results))
  }

  def seqAExit(
      id: Int
  )(abc: SeqABCCounter): ((Int, Int, Int), SeqABCCounter) = {
    val (state, counter, aid, aval, bid, bval, results) = abc
    require(state == 0)
    (results(id), abc)
  }

  def seqBExit(id: Int)(abc: SeqABCCounter): ((Int, Int, Int), SeqABCCounter) =
    seqAExit(id)(abc)

  def seqCSync(
      value: Int
  )(abc: SeqABCCounter): ((Int, Int, Int), SeqABCCounter) = {
    val (state, counter, aid, aval, bid, bval, results) = abc
    require(state == 2)
    val newresults = results.clone()
    newresults(aid) = (bval, value, counter)
    newresults(bid) = (aval, value, counter)
    ((aval, bval, counter), (0, counter + 1, aid, aval, bid, bval, newresults))
  }

  /** A worker for the ABCCounter LinTesters */
  def worker(id: Int, log: GenericThreadLog[SeqABCCounter, ConcABCCounter]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt() + id * 45207)
    for (i <- 0 until iters) {
      val v = random.nextInt(MaxVal) // a/b/c result.
      id % 3 match {
        case 0 => {
          log.log(
            _.aSync(id, v),
            "async(" + id + "," + v + ")",
            seqASync(id, v)
          )
          log.log(_.aExit(id), "aexit(" + id + ")", seqAExit(id))
        }
        case 1 => {
          log.log(
            _.bSync(id, v),
            "bsync(" + id + "," + v + ")",
            seqBSync(id, v)
          )
          log.log(_.bExit(id), "bexit(" + id + ")", seqBExit(id))
        }
        case 2 => {
          log.log(_.cSync(v), "csync(" + v + ")", seqCSync(v))
        }

      } // match {}
    }
  }

  def main(args: Array[String]) = {

    val faulty = false
//    val faulty = true

    for (r <- 0 until reps) {
      val seqABCCounter =
        (0, 0, 0, 0, 0, 0, Array.ofDim[(Int, Int, Int)](LinABCCounterGlobal.p))
      val concABCCounter = new ConcABCCounter(faulty)

      val tester =
        LinearizabilityTester.JITGraph[SeqABCCounter, ConcABCCounter](
          seqABCCounter,
          concABCCounter,
          LinABCCounterGlobal.p,
          worker _
        )
      assert(tester() > 0)

      if (r % 50 == 0) print(".")
    } // end of for loop
    println()
  }
}

