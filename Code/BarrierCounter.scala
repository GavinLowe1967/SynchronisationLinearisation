package synchronisationTester

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

import io.threadcso._

/* An implementation using semaphores. */
class BarrierCounter2(n: Int) extends BarrierCounterT{
  assert(n>1)
  private var seqNumber = 0 // the current sequence number
  private var waiting = 0 // number of processes currently waiting
  private val waitSem = SignallingSemaphore()
  private val mutex = MutexSemaphore()

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
    
