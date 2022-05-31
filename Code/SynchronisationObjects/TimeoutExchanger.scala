package synchronisationObject

import ox.gavin.profiling.Profiler

/** Objects that allow threads to exchange values, but timeout if no exchange
  * is possible. */
trait TimeoutExchangerT[A]{
  /** Try to exchange x with another thread.  Optionally return the value from
    * the other thread. */
  def exchange(x: A): Option[A]
}

// ==================================================================

class TimeoutExchanger[A](timeout: Int) extends TimeoutExchangerT[A]{
  private var stage = 0

  private var slot: A = _

  /** Try to exchange x with another thread.  Optionally return the value from
    * the other thread. */
  def exchange(x: A): Option[A] = synchronized{
    if(stage == 2) wait(timeout)    // wait for previous round to finish (1)
    if(stage == 0){
      slot = x; stage = 1
      wait(timeout)         // (2)
      if(stage == 2){
        stage = 0; notify() // signal to thread at (1)
        Profiler.count("sync")
        Some(slot)
      }
      else{ 
        assert(stage == 1); stage = 0 // signal to thread at (1)
        Profiler.count("non-sync")
        notifyAll(); None 
      }
    }
    else if(stage == 1){
      val res = slot; slot = x; stage = 2
      notifyAll()   // signal to thread at (2)
      Profiler.count("sync")
      Some(res)
    }
    else{    // stage == 2; this branch seems unlikely
      Profiler.count("non-sync")
      None
    }
  }
}

// ==================================================================

/** A faulty implementation. */
class FaultyTimeoutExchanger[A](timeout: Int) extends TimeoutExchangerT[A]{
  private var stage = 0

  private var slot: A = _

  /** Try to exchange x with another thread.  Optionally return the value from
    * the other thread. */
  def exchange(x: A): Option[A] = synchronized{
    if(stage == 2) wait(timeout)    // wait for previous round to finish (1)
    if(stage == 0){
      slot = x; stage = 1
      wait(timeout)         // (2)
      if(stage == 2){
        stage = 0; notify() // signal to thread at (1)
        Some(slot)
      }
      else{ assert(stage == 1); None } // This is an error: need to reset stage
    }
    else if(stage == 1){
      val res = slot; slot = x; stage = 2
      notifyAll()   // signal to thread at (2)
      Some(res)
    }
    else    // stage == 2; this branch seems unlikely
      None
  }
}
