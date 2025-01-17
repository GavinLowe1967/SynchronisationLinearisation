package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester, LinearizabilityLog}
import synchronisationObject.{
  ClosedException, CloseableChan, CloseableSyncChan, FaultyCloseableSyncChan, 
  WrappedCloseableChan, FaultyWrappedCloseableChan} //, CloseableNonSyncChan

  /** States used in the encoded state machine. */
  trait AState[A]
  case class Zero[A]() extends AState[A]  // No synchronisation in progress
  case class One[A](x: A) extends AState[A]  // Op1 but not Op1X has occurred.
  case class Closed[A]() extends AState[A]  // System has ended (no further synchronisation.)

object CloseableChannelTwoStepTester extends Tester {

  /** The type of objects tested. */
  type CC = CloseableChan[Int]

  /** Is state an instance of One?   */
  private def isOne[A](state: AState[A]) = 
  state match{ case One(_) => true; case _ => false }

  /** Specification object. The usual returns array is not required, because any
    * successful send always returns the same value (Unit). 
    * Only the state is necessary to ensure
    * the correct pattern of synchronisation linearisation.
    * This is true provided that send(!) is the split operation rather than receive(?).
    */
  class CCSpec(val state: AState[Int]) {

    /** Constructor for the initial state. */
    def this() = this(Zero())

    /** The first part of a send synchronisation. */
    def send(x: Int): (Boolean, CCSpec) = {
      require(!isOne(state))  // Must be Zero or Closed.
      if (state == Zero()) ( true, new CCSpec(One(x)))
      else (false, this) // Already closed, no state change.
    }

    /** The second part of a successful send synchronisation. 
      */
    def sendX(): (Unit, CCSpec) = {
      require(!isOne(state))  // A close() may intervene before sendX().
      ( () , this) // Return with no state change.
    }

    /** A receive(?), exception converted to return None. */
    def receive: (Option[Int], CCSpec) = {
      require(state != Zero())  // May be One(_) or Closed
      if (state != Closed()) {  // => state == One(_)
        val One(x) = state;
        (Some(x), new CCSpec(Zero()))  // Successful receive
      }
      else (None, this) // Already closed, no state change.
    }

    /** close(). */
    def close: (Unit, CCSpec) = {
      // a close() cannot occur between a successful send and receive pair.
      require(!isOne(state))  // Must be Zero or Closed.
      if (state == Zero()) ((), new CCSpec(Closed()))  // Mark as Closed
      else ((), this) // Already closed, no state change.
    }

    // Produce a somewhat randomised hash of this spec object (not critical).
    // However it must output the same hash for the same *value* of the object.
    override def hashCode = state.hashCode

    // Must return true exactly iff the input has the same *value* as this object.
    override def equals(that: Any) = that match {
      case es: CCSpec => (es.state == state)
    }
  } // end of CCSpec

  /** The maximum value sent. */
  var MaxVal = 20

  /** The probability of an operation by a worker being a close. */
  private var closeProb = 0.05  // Matches CloseableChanTester.

  /** Try to send x on chan, catching a ClosedException.  Return true if
  * successful. */ 
  @inline private def trySend(chan: CloseableChan[Int], x: Int): Boolean = 
    try{ chan!x; true } catch { case _: ClosedException => false }

  /** Try to receive on chan, catching a ClosedException.  Optionally return the
    * value received. */ 
  @inline private def tryReceive(chan: CloseableChan[Int]): Option[Int] = 
    try{ Some(chan?()) } catch { case _: ClosedException => None }

  /** A worker in the linearisability testing system. */
  def worker(me: Int, log: LinearizabilityLog[CCSpec, CC]) = {
    val random = new Random()
    for(i <- 0 until iters){
      if(random.nextFloat() < closeProb) log(_.close, s"close", _.close) 
      else if(me%2 == 0){
        val x = random.nextInt(MaxVal); 
        var ok = false
        log(c => {ok = trySend(c,x);ok}, s"send($x)", _.send(x)) // Perform the send
        log(c => (), s"sendX", _.sendX()) // Register the second step
      }
      else log(tryReceive(_), s"receive", _.receive)
    }
  }

  private var faulty = false // FaultyChan
  private var wrapped = false // Wrapped SCL channel
  private var nonsync = false // Use non-synchronising channel.

  /** Run a single test. */
  def doTest: Boolean = {
    val channel =
    // if (nonsync) new CloseableNonSyncChan[Int]
    // else
      if (faulty) {
        if (wrapped) new FaultyWrappedCloseableChan[Int]
        else new FaultyCloseableSyncChan[Int] }
      else {
        if (wrapped) new WrappedCloseableChan[Int]
        else new CloseableSyncChan[Int] }
    val spec = new CCSpec()
    val tester = LinearizabilityTester[CCSpec, CC](spec, channel, p, worker _)
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
      case "--faultyWrapped" => faulty = true; wrapped = true; i += 1
      case "--wrapped" => wrapped = true; i += 1
      case "--nonsync" => nonsync = true; i += 1

      // case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      // case "--countProfile" => countProfile = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }
}
