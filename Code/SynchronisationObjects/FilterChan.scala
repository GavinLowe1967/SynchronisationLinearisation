package synchronisationObject

/** A filtering channel, where the receiver specifies a predicate over the values it is willing to receive. */
trait FilterChanT[A]{
  /** Send x on the channel. */
  def send(x: A): Unit

  /** Receive a value that satisfies p. */
  def receive(p: A => Boolean): A
}

// =======================================================

/** A correct implementation. */
class FilterChan[A] extends FilterChanT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  def send(x: A): Unit = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    while(full) wait()
  }

  def receive(p: A => Boolean): A = synchronized{
    while(!full || !p(data)) wait()
    val result = data; full = false
    notifyAll()
    result
  }
}

// =======================================================

/** A faulty implementation. */
class FaultyFilterChan[A] extends FilterChanT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  def send(x: A): Unit = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    while(full) wait()
  }

  def receive(p: A => Boolean): A = synchronized{
    while(!full /* || !p(data) */) wait()
    val result = data; full = false
    notifyAll()
    result
  }
}
