package synchronisationObject

class ClosedException extends Throwable

trait CloseableChan[A]{
  def ?(u:Unit): A
  def !(x: A): Unit
  def close: Unit
}

// =========================================================================

/** Correct implementation. */
class CloseableSyncChan[A] extends CloseableChan[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  private var isClosed = false

  @inline private def isOpen = !isClosed

  private def checkOpen = if(isClosed) throw new ClosedException

  def !(x: A) = synchronized{
    while(isOpen && stage != 0) wait()   // (1)
    checkOpen
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(isOpen && stage == 1) wait()  // (3)
    if(stage == 2){ stage = 0; notifyAll() }  // signal to next send at (1)
    else{ assert(isClosed); throw new ClosedException }
  }

  def ?(u: Unit): A = synchronized{
    while(isOpen && stage != 1) wait()          // (2)
    checkOpen
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }

  def close = synchronized{
    isClosed = true; notifyAll()
  }
}

// -------------------------------------------------------

/** A faulty implementation. */
class FaultyCloseableSyncChan[A] extends CloseableChan[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  private var isClosed = false

  @inline private def isOpen = !isClosed

  private def checkOpen = if(isClosed) throw new ClosedException

  def !(x: A) = synchronized{
    while(isOpen && stage != 0) wait()   // (1)
    checkOpen
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(isOpen && stage == 1) wait()  // (3)
    // Following is wrong
    checkOpen
    stage = 0; notifyAll()   // signal to next send at (1)
  }

  def ?(u: Unit): A = synchronized{
    while(isOpen && stage != 1) wait()          // (2)
    checkOpen
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }

  def close = synchronized{
    isClosed = true; notifyAll()
  }
}


// ==================================================================

/** An SCL channel, wrapped to implement Chan. */
class WrappedCloseableChan[A] extends CloseableChan[A]{
  private val c = new ox.scl.SyncChan[A] 

  /** Wrap comp, converting SCL Closed exceptions into ClosedExceptions. */
  private def wrap[A](comp: => A) =  
    try{ comp } 
    catch{ case _: ox.scl.channel.Closed => throw new ClosedException }
 
  def ?(u:Unit) = wrap(c?())

  def !(x: A) = wrap(c!x)
 
  def close = c.close
}

// ==================================================================

/** The old version of an SCL channel, wrapped to implement Chan.  This
  * version is faulty. */
class FaultyWrappedCloseableChan[A] extends CloseableChan[A]{
  private val c = new ox.scl.channel.OldSyncChan[A] 

  /** Wrap comp, converting SCL Closed exceptions into ClosedExceptions. */
  private def wrap[A](comp: => A) =  
    try{ comp } 
    catch{ case _: ox.scl.channel.Closed => throw new ClosedException }
 
  def ?(u:Unit) = wrap(c?())

  def !(x: A) = wrap(c!x)
 
  def close = c.close
}

