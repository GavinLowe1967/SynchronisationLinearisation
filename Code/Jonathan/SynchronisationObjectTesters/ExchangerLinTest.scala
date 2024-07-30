package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

// Common signature between concurrent and sequential implementations.
trait LinExchangerT {
  def exchangeSync(id: Int, value: Int): Unit // id is thread number
  // id is needed in the log to correlate the exchanges.
  def exchangeExit(id: Int): Int
}

class ConcExchanger(p: Int, faulty: Boolean) extends LinExchangerT {

  val exchanger =
    if (faulty) new FaultyExchanger[Int]
    else new Exchanger[Int] // Datatype to test.  Note - it is unmodified.
  val values = Array.ofDim[Int](p) // alternatively could use ThreadLocal.

  // values array does not need to be protected from concurrent access.
  def exchangeSync(id: Int, value: Int): Unit = {
    values(id) = exchanger.exchange(value);
  }
  def exchangeExit(id: Int): Int = values(id)

}

object ExchangerLinTest {
  var iters = 1 // Number of iterations by each worker
  val MaxVal = 10 // No of values in random set.

  // We need an immutable representation for the sequential state.
  // This could be a class, but here we use the common form of defining a tuple type.
  // Type representing state of sequential Exchanger system.
  type SeqExchanger = (Boolean, Int, Array[Int]) // (set,id,values)

  def seqExchangeSync(id: Int, value: Int)(
      ex: SeqExchanger
  ): (Unit, SeqExchanger) = {
    val (set, id1, values) = ex
    if (!set) { // we are 1st party
// Must not alter original values (because of immutability requirement).
      val newvalues = values.clone()
      newvalues(id) = value
      ((), (true, id, newvalues))
    } else { // set==true, so we are 2nd party
// Must not alter original values (because of immutability requirement).
      val newvalues = values.clone()
// Do the swap
      newvalues(id) = newvalues(id1) // Copy first value
      newvalues(id1) = value
      ((), (false, id, newvalues))
    }
  }

  def seqExchangeExit(id: Int)(ex: SeqExchanger): (Int, SeqExchanger) = {
    val (set, _, values) = ex
    require(!set) // this is outside of any syncronization "nucleus".
    (values(id), ex)
  }

  /** A worker for the Exchanger LinTesters */
  def worker(id: Int, log: GenericThreadLog[SeqExchanger, ConcExchanger]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt() + id * 45207)
    for (i <- 0 until iters) {
      val x = random.nextInt(MaxVal) // value to exchange.
      log.log(
        _.exchangeSync(id, x),
        "exchangesync(" + id + "," + x + ")",
        seqExchangeSync(id, x)
      )
      log.log(
        _.exchangeExit(id),
        "exchangeexit(" + id + ")",
        seqExchangeExit(id)
      )
    }
  }

  def main(args: Array[String]) = {
    val reps = 10 // Number of repetitions
    // Avoid deadlock because each thread only performs a single exchange.
    val p = 8

    val faulty = false

    for (r <- 0 until reps) {
      val seqExchanger = (false, 0, Array.ofDim[Int](p))
      val concExchanger = new ConcExchanger(p, faulty)

      val tester = LinearizabilityTester.JITGraph[SeqExchanger, ConcExchanger](
        seqExchanger,
        concExchanger,
        p,
        worker _
      )
      assert(tester() > 0)

      if (r % 50 == 0) print(".")
    } // end of for loop
    println()
  }
}

