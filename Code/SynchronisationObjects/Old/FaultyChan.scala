package synchronisationObject


trait Chan[A]{
  def ?(u:Unit): A
  def !(x: A): Unit
}

// ==================================================================

/** A deliberately incorrect implementation of a shared synchronous channel. */
class FaultyChan[A] extends Chan[A]{
  private var data: A = _
  private var full = false

  def ?(u:Unit): A = synchronized{
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

class WrappedSCLChan[A] extends  Chan[A]{
  private val c = new ox.scl.SyncChan[A] 
 
  def ?(u:Unit) = c?()

  def !(x: A) = c!x
}

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
