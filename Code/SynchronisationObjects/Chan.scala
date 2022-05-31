package synchronisationObject


trait Chan[A]{
  def ?(u:Unit): A
  def !(x: A): Unit
}

// =========================================================================

/** Correct implementation. */
class SyncChan[A] extends Chan[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  def !(x: A) = synchronized{
    while(stage != 0) wait()          // (1)
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(stage == 1) wait()          // (3)
    stage = 0; notifyAll()            // signal to next send at (1)
  }

  def ?(u: Unit): A = synchronized{
    while(stage != 1) wait()          // (2)
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }
}

// ==================================================================

/** A deliberately incorrect implementation of a shared synchronous channel.
  * Here the sender doesn't wait. */
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

/** An SCL channel, wrapped to implement Chan. */
class WrappedSCLChan[A] extends Chan[A]{
  private val c = new ox.scl.SyncChan[A] 
 
  def ?(u:Unit) = c?()

  def !(x: A) = c!x
}

// =======================================================

/** A channel that can deadlock. 
  * If two threads call ! and one thread calls ?, both senders can end up
  * waiting.  However, another call of ? will unblock both. */
class DeadlockingChan[A] extends Chan[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  def !(x: A) = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    while(full) wait()
  } 

  def ?(u: Unit): A = synchronized{
    while(!full) wait()
    full = false; notifyAll()
    data
  }
}

// =============================================================================

/** A faulty many-many synchronous channel passing data of type A, implemented
  * using a monitor.  An extra signal is necessary.  This can get into a state
  * where both a sender and a receiver are waiting. */
class FaultyChan2[A] extends Chan[A]{
  /** The current or previous value. */
  private var value = null.asInstanceOf[A]

  /** Is the current value of value valid, i.e. ready to be received? */
  private var full = false

  /** Monitor for controlling synchronisations. */
  private val monitor = new ox.scl.lock.Lock

  /** Condition for signalling to sender that a value has been deposited. */
  private val slotFull = monitor.newCondition

  /** Condition for signalling to current receiver that it can continue. */
  private val continue = monitor.newCondition

  /** Condition for signalling to the next sender that the previous value has
    * been read. */
  private val slotEmptied = monitor.newCondition

  def !(x: A) = monitor.mutex{
    slotEmptied.await(!full) // wait for previous value to be consumed
    // Deposit my value, and signal to receiver
    value = x; full = true
    slotEmptied.signal()
    // Wait for receiver
    continue.await(); 
  }

  def ?(u: Unit): A = monitor.mutex{
    // wait for sender
    slotFull.await(full)
    // notify current sender
    continue.signal()
    // clear value, and notify next sender
    full = false // ; slotEmptied.signal()
    value
  }

}

