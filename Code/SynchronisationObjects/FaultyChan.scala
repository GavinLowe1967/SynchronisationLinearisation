package synchronisationObject

import io.threadcso._

/** An extension of a CSO Chan[A], giving stub definitions for everything
  * except ! and ?. */
trait BasicChan[A] extends Chan[A]{
  // Leave following undefined.

  // Members declared in channel.Chan
  def close(): Unit = ???

  // Members declared in alternation.channel.InPort
  def registerIn(alt: alternation.Runnable, theIndex: Int)
      : channel.PortState = ???
  def unregisterIn(): channel.PortState = ???

  // Members declared in channel.InPort
  def ?[U](f: A => U): U = ???
  def ??[U](f: A => U): U = ???
  def canInput: Boolean = ???
  def closeIn(): Unit = ???
  def inPortState: channel.PortState = ???
  def readBefore(ns: Long): Option[A] = ???

  // Members declared in basis.Named
  def nameGenerator: basis.NameGenerator = ???

  // Members declared in channel.OutPort
  def canOutput: Boolean = ???
  def closeOut(): Unit = ???
  def outPortState: channel.PortState = ???
  def writeBefore(nsWait: Long)(value: A): Boolean = ???

  // Members declared in alternation.channel.OutPort
  def registerOut(alt: alternation.Runnable, theIndex: Int)
      : channel.PortState = ???
  def unregisterOut(): channel.PortState = ???
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
