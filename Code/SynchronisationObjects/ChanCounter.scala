package synchronisationObject

/** A channel with a sequence counter, where the sender receives the current value of the sequence counter. */
trait ChanCounterT[A]{
  /** Send x on the channel. */
  def send(x: A): Int

  /** Receive a value that satisfies p. */
  def receive(): A
}

// =======================================================

/** Correct implementation. */
class ChanCounter[A] extends ChanCounterT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  /** The sequence counter. */
  private var counter = 0

  def send(x: A): Int = synchronized{
    while(stage != 0) wait()          // (1)
    data = x; stage = 1; 
    counter += 1; val res = counter; notifyAll()  // signal to receive at (2)
    while(stage == 1) wait()          // (3)
    stage = 0; notifyAll()            // signal to next send at (1)
    res
  }

  def receive(): A = synchronized{
    while(stage != 1) wait()          // (2)
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }
}

/** Another correct version.  This handles the sequence counter at a different
  * point. */
class ChanCounter2[A] extends ChanCounterT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  /** The sequence counter. */
  private var counter = 0

  def send(x: A): Int = synchronized{
    while(stage != 0) wait()          // (1)
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(stage == 1) wait()          // (3)
    stage = 0; notifyAll()            // signal to next send at (1)
    counter += 1; counter
  }

  def receive(): A = synchronized{
    while(stage != 1) wait()          // (2)
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }
}

// ==================================================================

/** This one gives the wrong sequence numbers. */
class ChanCounter3[A] extends ChanCounterT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  /** The sequence counter. */
  private var counter = 0

  def send(x: A): Int = synchronized{
    counter += 1; val res = counter
    while(stage != 0) wait()          // (1)
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(stage == 1) wait()          // (3)
    stage = 0; notifyAll()            // signal to next send at (1)
    res
  }

  def receive(): A = synchronized{
    while(stage != 1) wait()          // (2)
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }

  /* Two sends might read the sequence counter in a different order from how
   * they are linearised, i.e. they synchronise with two receives that do not
   * overlap in time and that take place in the *opposite* order. */
}

// =======================================================

/** An implementation where threads can get stuck.  If two threads call send
  * and one thread calls receive, both senders can end up waiting.  However,
  * another call of receive will unblock both. */
class DeadlockingChanCounter[A] extends ChanCounterT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  /** The sequence counter. */
  private var counter = 0

  def send(x: A): Int = synchronized{
    while(full) wait()
    data = x; full = true; 
    counter += 1; val res = counter; notifyAll()
    while(full) wait()
    res 
  }

  def receive(): A = synchronized{
    while(!full) wait()
    full = false
    notifyAll()
    data
  }
}

// =======================================================

/** A faulty implementation. */
class FaultyChanCounter[A] extends ChanCounterT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  /** The sequence counter. */
  private var counter = 0

  def send(x: A): Int = synchronized{
    while(full) wait()
    data = x; full = true; 
    counter += 1; notifyAll()
    while(full) wait()
    counter // Note: It is wrong to read the counter here. 
  }

  def receive(): A = synchronized{
    while(!full) wait()
    full = false
    notifyAll()
    data
  }
}
