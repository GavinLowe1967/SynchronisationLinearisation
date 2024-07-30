package synchronisationObject

/** The trait for a barrier synchronisation with sequence number. */
trait BarrierCounterT{
  def sync: Int
}

// ==================================================================

/** An implementation using a monitor. */
class BarrierCounter(n: Int) extends BarrierCounterT{
  private var seqNumber = 0 // the current sequence number
  private var count = 0 // The number of waiting threads.
  private var leaving = false // Are we in the leaving phase?

  def sync = synchronized{
    while(leaving) wait() // Wait for previous round to finish
    count += 1
    if(count == n){ leaving = true; count -= 1; notifyAll(); seqNumber }
    else{ 
      while(!leaving) wait()
      count -= 1
      if(count == 0){ 
        leaving = false; notifyAll() // Allow next round to continue
        seqNumber += 1; seqNumber-1 // Increment sequence number for next round
      }
      else seqNumber
    }
  }
}

// ==================================================================

/** A faulty version.  */
class FaultyBarrierCounter(n: Int) extends BarrierCounterT{
  private var seqNumber = 0 // the current sequence number
  private var count = 0
  private var leaving = false

  def sync = synchronized{
    count += 1
    if(count == n){ leaving = true; /* count -= 1;*/ notifyAll(); seqNumber }
    else{ 
      while(!leaving) wait()
      count -= 1
      if(count == 0){
        leaving = false; seqNumber += 1; seqNumber-1 
      }
      else seqNumber
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

import ox.scl._

/* An implementation using semaphores. */
class BarrierCounter2(n: Int) extends BarrierCounterT{
  assert(n>1)
  private var seqNumber = 0 // the current sequence number
  private var waiting = 0 // number of processes currently waiting
  private val waitSem = new SignallingSemaphore()
  private val mutex = new MutexSemaphore()

  def sync = {
    mutex.down
    val result = seqNumber
    if(waiting == n-1){ waitSem.up; result }
    else{ 
      waiting += 1; mutex.up; waitSem.down // Wait until woken
      waiting -= 1
      if(waiting==0){ seqNumber += 1; mutex.up} else waitSem.up // pass the baton
      result
    }
  }
}
    
