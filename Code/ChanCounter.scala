package synchronisationTester

/** A channel with a sequence counter, where the sender receives the current value of the sequence counter. */
trait ChanCounterT[A]{
  /** Send x on the channel. */
  def send(x: A): Int

  /** Receive a value that satisfies p. */
  def receive(): A
}

// =======================================================

/** A correct implementation. */
class ChanCounter[A] extends ChanCounterT[A]{
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
