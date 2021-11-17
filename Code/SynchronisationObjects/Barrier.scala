package synchronisationObject

/** The trait for a barrier synchronisation. */
trait BarrierT{
  def sync: Unit
}

// ==================================================================

/** The CSO barrier. */
class Barrier(n: Int) extends io.threadcso.Barrier(n) with BarrierT

// ==================================================================

/** An obviously faulty barrier. */
class FaultyBarrier(n: Int) extends BarrierT{
  def sync = {} // return immediately!
}

// ==================================================================

/** A faulty version that doesn't guard against spurious wake-ups. */
class FaultyBarrier2(n: Int) extends BarrierT{
  private var count = 0

  def sync = synchronized{
    count += 1
    if(count == n){ count = 0; notifyAll() }
    else wait() // This doesn't guard against spurious wake-ups, so is an
                // error, but testing struggles to find it.
  }
}

// ==================================================================

/** Another faulty version.  */
class FaultyBarrier3(n: Int) extends BarrierT{
  private var count = 0
  private var leaving = false

  def sync = synchronized{
    count += 1
    if(count == n){ leaving = true; /* count -= 1;*/ notifyAll() }
    else{ 
      while(!leaving) wait()
      count -= 1
      if(count == 0) leaving = false
    }
  }
  /* Note: if count is decremented in the "count == n" branch, then deadlock can
   * arise, as follows.  (1) n-1 threads call sync and wait; (2) another
   * thread calls sync and performs notifyAll(); (3) another thread calls sync
   * and sets count = n and calls notifyAll(); (4) the waiting n-1 threads all
   * leave, setting count = 0.  Then the number of other threads is not a
   * multiple of n, so they deadlock. */
} 

// ==================================================================

/** Another correct version (I think). */
class Barrier2(n: Int) extends BarrierT{
  private var count = 0 // The number of waiting threads.
  private var leaving = false // Are we in the leaving phase?

  def sync = synchronized{
    while(leaving) wait() // Wait for previous round to finish
    count += 1
    if(count == n){ leaving = true; count -= 1; notifyAll() }
    else{ 
      while(!leaving) wait()
      count -= 1
      if(count == 0){ 
        leaving = false; notifyAll() // Allow next round to continue. 
      }
    }
  }
}
