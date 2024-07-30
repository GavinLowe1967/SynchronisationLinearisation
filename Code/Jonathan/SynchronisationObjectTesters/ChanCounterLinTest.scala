package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

object ChanCounterLinTest {

  val iters = 100 // Number of iterations by each worker
  val MaxVal = 10 // Maximum value placed in the queue

  type SeqChanCounter[A] = (Boolean, Int, A) // (full,count,data)

  def seqSend(x: Int)(c: SeqChanCounter[Int]): (Int, SeqChanCounter[Int]) = {
    val (full, count, data) = c
    require(!full)
    val newcount = count + 1
    (newcount, (true, newcount, x))
  }

  def seqReceive(c: SeqChanCounter[Int]): (Int, SeqChanCounter[Int]) = {
    val (full, count, data) = c
    require(full)
    (data, (false, count, data))
  }

  // NoOp method is needed to allow linearizability testing of synchronisation.
  def seqNoOp(c: SeqChanCounter[Int]): (Unit, SeqChanCounter[Int]) = {
    val (full, count, data) = c
    // A NoOp operation can't occur when the channel is full.
    require(!full)
    ((), c) // No-op.
  }

  class concChanCounter[A](faulty: Boolean) extends ChanCounterT[A] {

    val c = if (faulty) new FaultyChanCounter[A] else new ChanCounter[A]

    // send and receive are delegated directly to c:
    def send(x: A): Int = c.send(x)
    def receive(): A = c.receive()

    // noop is invoked after each send & receive,
    def noop(): Unit = {} // do nothing

  }

  /** A worker for the LinTesters */
  // There's no need to construct a wrapper fo r
  def worker(
      me: Int,
      log: GenericThreadLog[SeqChanCounter[Int], concChanCounter[Int]]
  ) = {
    val random = new scala.util.Random(scala.util.Random.nextInt() + me * 45207)
    for (i <- 0 until iters) {
      if (me % 2 == 0) { // Sender
        val x = random.nextInt(MaxVal)
        log.log(_.send(x), "send(" + x + ")", seqSend(x))
        log.log(_.noop(), "noop", seqNoOp)
      } else { // Receiver
        log.log(_.receive(), "receive", seqReceive)
        log.log(_.noop(), "noop", seqNoOp)
      }
    }
  }

  def main(args: Array[String]) = {
    val reps = 100 // Number of repetitions
    // Number of workers, must be even (to ensure termination of test system)
    val p = 8

    val faulty = false

    for (r <- 0 until reps) {
      val seqChan = (false, 0, 0)
      val concChan = new concChanCounter[Int](faulty)

      val tester = LinearizabilityTester
        .JITGraph[SeqChanCounter[Int], concChanCounter[Int]](
          seqChan,
          concChan,
          p,
          worker _
        )
      assert(tester() > 0)

      if (r % 50 == 0) print(".")
    } // end of for loop
    println()
  }
}

