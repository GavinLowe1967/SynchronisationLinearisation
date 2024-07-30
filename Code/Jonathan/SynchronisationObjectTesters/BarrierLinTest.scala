package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

package object BarrierLinTestGlobal {
  val p = 12 // No of threads.
}

// Common signature between concurrent and sequential implementations.
trait LinBarrierT {
  def bSync(id: Int): Unit
  def bExit(id: Int): Unit
}

class ConcBarrier() extends LinBarrierT {

// Datatype to test.  Note - it is unmodified.
  val barrier = new synchronisationObject.Barrier(BarrierLinTestGlobal.p)
//  val barrier = new synchronisationObject.FaultyBarrier(BarrierLinTestGlobal.p)

  def bSync(id: Int): Unit = barrier.sync(id)
  def bExit(id: Int): Unit = {} // Return immediately

}

object BarrierLinTest {
  var iters = 20 // Number of iterations by each worker
  val reps = 100

  // Type representing state of sequential Barrier system.
  // This could be a class, but here we use the common idiom of defining a tuple type.
  type SeqBarrier = (Boolean, Int) // (filling,count)

  def seqBSync(id: Int)(barrier: SeqBarrier): (Unit, SeqBarrier) = {
    val (filling, count) = barrier
// We can impose our insisted order on the sync(id) events.
    require(filling && count == id)
// switch to emptying if all threads have synced.
    ((), (if (id == BarrierLinTestGlobal.p - 1) false else true, count + 1))
  }

  def seqBExit(id: Int)(barrier: SeqBarrier): (Unit, SeqBarrier) = {
    val (filling, count) = barrier
// Exits occur in any order and we don't check which ones have happened.
    require(!filling)
// switch to filling if all threads have exited.
    ((), (if (count == 1) true else false, count - 1))
  }

  /** A worker for the Barrier LinTesters */
  def worker(id: Int, log: GenericThreadLog[SeqBarrier, LinBarrierT]) = {
    for (i <- 0 until iters) {
      log.log(_.bSync(id), "bsync(" + id + ")", seqBSync(id))
      log.log(_.bExit(id), "bexit(" + id + ")", seqBExit(id))
    } // for{}
  }

  def main(args: Array[String]) = {

    for (r <- 0 until reps) {
      val seqBarrier = (true, 0)
      val concBarrier = new ConcBarrier()

      val tester = LinearizabilityTester.JITGraph[SeqBarrier, LinBarrierT](
        seqBarrier,
        concBarrier,
        BarrierLinTestGlobal.p,
        worker _,
        tsLog = true
      )
      assert(tester() > 0)

      if (r % 50 == 0) print(".")
    } // end of for loop
    println()
  }
}

