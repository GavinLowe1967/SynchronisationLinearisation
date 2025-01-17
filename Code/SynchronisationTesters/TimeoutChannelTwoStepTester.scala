package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester, LinearizabilityLog}
import synchronisationObject.{
  TimeoutChannelT,
  TimeoutChannel,
  FaultyTimeoutChannel
}

/** States used in the encoded state machine. */
trait AState[A]
case class Zero[A]() extends AState[A]
case class One[A](x: A) extends AState[A]

object TimeoutChannelTwoStepTester extends Tester {

  /** The type of objects tested. */
  type TC = TimeoutChannelT[Int]

  /* Note: we have to turn off the optimisation in the linearisation tester that
   * tries to detect if the same invocation (i.e. with the same label) has
   * previously been performed from the same state.  This is because one
   * instance of that operation might have failed, and the other succeeded.
   */

  /** Specification object. The usual returns array is not required, because any
    * successful send always returns true. Only the state is necessary to ensure
    * the correct pattern of synchronisation linearisation.
    */
  class TCSpec(val state: AState[Int]) {

    /** Constructor for initial state. */
    def this() = this(Zero())

    /** The first part of a successful send synchronisation. */
    def sendWithin(x: Int): (Boolean, TCSpec) = {
      require(state == Zero())
      ( true, new TCSpec(One(x))) // Return true, move to state One(x)
    }

    /** The second part of a successful send synchronisation.  */
    def sendWithinX(): (Unit, TCSpec) = {
      require(state == Zero())
      ( () , this) // Return Unit, with no state change.
    }

    /** A successful receiveWithin */
    def receiveWithin(): (Option[Int], TCSpec) = {
      require(state != Zero()); val One(x) = state;
      (Some(x), new TCSpec(Zero()))
    }

    /** An attempt to send that failed. */
    def sendFail(x: Int): (Boolean, TCSpec) = {
      require(state == Zero())
      (false, this) // Return false, with no state change.
    }

    /** An attempt to receive that failed. This operation should be linearised
      * outside any synchronisation, and there is no state change. The return
      * value is None.
      */
    def receiveFail(): (Option[Int], TCSpec) = {
      require(state == Zero())
      (None, this)
    }

    // Produce a somewhat randomised hash of this spec object (not critical).
    // However it must output the same hash for the same *value* of the object.
    override def hashCode = state.hashCode

    // Must return true exactly iff the input has the same *value* as this object.
    override def equals(that: Any) = that match {
      case es: TCSpec => (es.state == state)
    }
  } // end of TCSpec

  /** The maximum value sent. */
  var MaxVal = 100

  /** A worker in the linearisability testing system. */
  def worker(me: Int, log: LinearizabilityLog[TCSpec, TC]) = {
    val random = new Random()
    for (i <- 0 until iters) {
      // Note: the delay below means we get a reasonable mix of successul and
      // unsuccessful invocations.
      Thread.sleep(random.nextInt(1))
      if (random.nextInt(2) == 0) { // Try to sendWithin() a random value
        val x = random.nextInt(MaxVal) // value sent
        var ok = false // value received
        log(
          tc => {ok = tc.sendWithin(x, 1 + random.nextInt(1));ok},
          s"sendWithin($x) ($me,$i)", // label depends on return
          spec => if ( !ok ) spec.sendFail(x) else spec.sendWithin(x)
        )
        // Bracket a successful send with a closing sendWithinX to check that
        // the state has returned to Zero().  
        if (ok) log(_ => (), s"sendWithinX", _.sendWithinX())
      } else { // Try to receiveWithin()
        var ok = null.asInstanceOf[Option[Int]] // value received
        log(
          tc => {ok = tc.receiveWithin(1 + random.nextInt(1));ok},
          s"receiveWithin ($me,$i)",
          spec => if (ok == None) spec.receiveFail() else spec.receiveWithin()
        )
      }
    }
  }

  private var faulty = false // FaultyChan

  /** Run a single test. */
  def doTest: Boolean = {
    val channel =
      if (faulty) new FaultyTimeoutChannel[Int]
      else new TimeoutChannel[Int]
    val spec = new TCSpec()
    val tester = LinearizabilityTester[TCSpec, TC](spec, channel, p, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false
    while (i < args.length) args(i) match {
      case "-p" => p = args(i + 1).toInt; i += 2
      // case "--verbose" => verbose = true; i += 1
      case "--reps"   => reps = args(i + 1).toInt; i += 2
      case "--iters"  => iters = args(i + 1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i + 1).toInt; i += 2
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1

      // case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      // case "--countProfile" => countProfile = true; i += 1
      case arg => //println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }
}
