package synchronisationTester

import java.lang.System.nanoTime

/** The trait of a timeout channel. */
trait TimeoutChannelT[A]{
  /** Try to send x for up to duration ms.  Return true if successful. */
  def sendWithin(x: A, duration: Int): Boolean

  /** Try to receive for up to duration ms.  Optionally return result
    * received. */
  def receiveWithin(duration: Int): Option[A]

  protected val Million = 1_000_000L

  protected val HalfMillion = Million/2

  /** Perform a wait until time end, or notified (or a spurious wake-up). */
  @inline protected def waitUntil(end: Long) = {
    val waitTime = (end-nanoTime+HalfMillion)/Million // to nearest ms
    if(waitTime > 0) wait(waitTime)
  }
  // Note: wait(0) is equivalent to a standard wait, so we need to wait for at
  // least 1 ms.
}

// ==================================================================

class TimeoutChannel[A] extends TimeoutChannelT[A]{
  /** The current value; valid if full = true. */
  private var data: A = _

  /** Is this currently storing a value?  Cleared by the sender on
    * completion. */
  private var full = false

  /** Has the receiver consumed the current value?  Set by the receiver on
    * consumption. */
  private var consumed = true

  /** Try to send x for up to duration ms.  Return true if successful. */
  def sendWithin(x: A, duration: Int): Boolean = synchronized{
    val start = nanoTime; val durationNS = duration*Million
    val end = start+durationNS
    while(full && nanoTime-start < durationNS) 
      waitUntil(end)                            // wait for previous sender
    if(full){ assert(nanoTime-start >= durationNS); false }
    else{
      data = x; full = true; 
      consumed = false; notifyAll()            // notify receiver (and others)
      while(!consumed && nanoTime-start <  durationNS) 
        waitUntil(end)                        // wait for receiver
      if(!consumed){ 
        assert(nanoTime-start >= durationNS); consumed = true 
        full = false; notifyAll(); false      // notify next sender (and others)
      }
      else{ 
        full = false; notifyAll(); true      // notify next sender (and others)
      }
    }
  }

  /** Try to receive for up to duration ms.  Optionally return result
    * received. */
  def receiveWithin(duration: Int): Option[A] = synchronized{
    val start = nanoTime; val durationNS = duration*Million
    val end = start+durationNS
    while(consumed && nanoTime-start < durationNS) 
      waitUntil(end)                              // wait for sender
    if(consumed){ assert(nanoTime-start >= durationNS); None }
    else{ 
      consumed = true; notifyAll(); Some(data)   // notify receiver
    }
  }
}

// ==================================================================

/** A faulty implementation. */
class FaultyTimeoutChannel[A] extends TimeoutChannelT[A]{
  /** The current value; valid if full = true. */
  private var data: A = _

  /** Is this currently storing a value? */
  private var full = false

  /** Try to send x for up to duration ms.  Return true if successful. */
  def sendWithin(x: A, duration: Int): Boolean = synchronized{
    val start = nanoTime; val durationNS = duration*Million
    val end = start+durationNS
    while(full && nanoTime-start < durationNS) 
      waitUntil(end)                            // wait for previous sender
    if(full){ assert(nanoTime-start >= durationNS); false }
    else{
      data = x; full = true; notifyAll()       // notify receiver (and others)
      while(full && nanoTime-start <  durationNS) 
        waitUntil(end)                        // wait for receiver
      if(full){ assert(nanoTime-start >= durationNS); full = false; false }
      else true
    }
  }

  /** Try to receive for up to duration ms.  Optionally return result
    * received. */
  def receiveWithin(duration: Int): Option[A] = synchronized{
    val start = nanoTime; val durationNS = duration*Million
    val end = start+durationNS
    while(!full && nanoTime-start <  durationNS) 
      waitUntil(end)                              // wait for sender
    if(!full){ assert(nanoTime-start >= durationNS); None }
    else{ full = false; notifyAll; Some(data) }
  }

  /* This doesn't work.  1. A sender A deposits its value and waits.  2. A
   * receiver B consumes the value, sets full = false, wakes the sender and
   * successfully returns.  3. A second sender C stores its value, sets full =
   * true and waits. 4. The first sender A reaches its timeout time, sees full
   * = true, and returns false.  There is no successful send corresponding to
   * A's receive. */
}
 
