package synchronisationObject

import ox.scl._

/** An extension of a SCL Chan[A], giving stub definitions for everything
  * except ! and ?. */
trait BasicChan[A] extends Chan[A]{
  
  // Leave following undefined.

  // Members declared in ox.scl.channel.Chan
  def reopen(): Unit = ???

  // Members declared in ox.scl.channel.InPort
  def ?(u: Unit): A = ???
  protected def canReceive: Boolean = ???
  def close(): Unit = ???
  protected def completeReceive(): A = ???
  def receiveBeforeNanos(nanos: Long): Option[A] = ???

  // Members declared in ox.scl.channel.OutPort
  def endOfStream(): Unit = ???
  protected def isClosedOut: Boolean = ???
  def sendBeforeNanos(nanos: Long)(x: A): Boolean = ???
  protected def trySend(value: () => A): Boolean = ???

  // Members declared in ox.scl.channel.Port
  protected val lock: ox.scl.lock.Lock = ???

}

// ==================================================================

/** A deliberately incorrect implementation of a shared synchronous channel. */
class FaultyChan[A] extends BasicChan[A]{
  private var data: A = _
  private var full = false

  def ?(): A = synchronized{
    while(!full) wait()
    val result = data; full = false
    notifyAll()
    result
  }

  def !(x: A) = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    // while(full) wait()
  }
}

// ==================================================================

/*
/** A faulty many-many synchronous channel passing data of type A, implemented
  * using a monitor.  Actually, this deadlocks so isn't useful. */
class FaultyChan2[A] extends BasicChan[A]{
  /** The current or previous value. */
  private var value = null.asInstanceOf[A]

  /** Is the current value of value valid, i.e. ready to be received? */
  private var full = false

  /** Monitor for controlling synchronisations. */
  private val monitor = new Monitor

  /** Condition for signalling to sender that a value has been deposited. */
  private val slotFull = monitor.newCondition

  /** Condition for signalling to current receiver that it can continue. */
  private val continue = monitor.newCondition

  /** Condition for signalling to the next sender that the previous value has
    * been read. */
  private val slotEmptied = monitor.newCondition

  def !(x: A) = monitor.withLock{
    slotEmptied.await(!full) // wait for previous value to be consumed
    // Deposit my value, and signal to receiver
    value = x; full = true
    slotEmptied.signal()
    // Wait for receiver
    continue.await(); 
  }

  def ?(): A = monitor.withLock{
    // wait for sender
    slotFull.await(full)
    // notify current sender
    continue.signal()
    // clear value, and notify next sender
    full = false // ; slotEmptied.signal()
    value
  }

}
 */
