package synchronisationObject

/** A filtering channel, where the receiver specifies a predicate over the
  * values it is willing to receive. */
trait FilterChanT[A]{
  /** Send x on the channel. */
  def send(x: A): Unit

  /** Receive a value that satisfies p. */
  def receive(p: A => Boolean): A
}

// =======================================================

/** An implementation where each send waits for the previous send to complete,
  * so doesn't satisfy the progress property.
  * 
  * Note: this is *not* the default for FilterChanTester, but is selected
  * using the --nonProgressing flag. */
class FilterChan[A] extends FilterChanT[A]{

  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Which stage of the exchange are we at?  Takes values in {0,1,2}. */
  private var stage = 0

  def send(x: A) = synchronized{
    while(stage != 0) wait()          // (1)
    data = x; stage = 1; notifyAll()  // signal to receive at (2)
    while(stage == 1) wait()          // (3)
    stage = 0; notifyAll()            // signal to next send at (1)
  }

  def receive(p: A => Boolean): A = synchronized{
    while(stage != 1 || !p(data)) wait()          // (2)
    stage = 2; notifyAll()            // signal to send at (3)
    data
  }
}

// =======================================================

/** An implementation where threads can get stuck. */
class DeadlockingFilterChan[A] extends FilterChanT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  def send(x: A): Unit = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    while(full) wait()
  }

  def receive(p: A => Boolean): A = synchronized{
    while(!full || !p(data)) wait()
    full = false; notifyAll()
    data
  }
}

// =======================================================

/** A faulty implementation that doesn't check the predicate! */
class FaultyFilterChan[A] extends FilterChanT[A]{
  /** The current value being sent, if full = true. */
  private var data: A = _

  /** Is the value in data valid? */
  private var full = false

  def send(x: A): Unit = synchronized{
    while(full) wait()
    data = x; full = true; notifyAll()
    while(full) wait()
  }

  def receive(p: A => Boolean): A = synchronized{
    while(!full /* || !p(data) */) wait()
    val result = data; full = false
    notifyAll()
    result
  }
}

// =======================================================

import scala.collection.mutable.Queue

/** A version using semaphores.  I think this version is correct. 
  * 
  * Nonte: this is the default from FilterChanTester. */
class SemaphoreFilterChan[A] extends FilterChanT[A]{
  // println("SemaphoreFilterChan")
  /** A queue of values being sent together with a semaphore for signalling to
    * the sender. */
  private val senderQueue = new Queue[(A, Semaphore)]

  /** A queue of tests for receipt together with a semaphore for signalling to
    * the receiver. */
  private val receiverQueue = new Queue[(A => Boolean, Semaphore)]

  /* Note: we don't necessarily preserve queue order: we treat the queues as
   * sets.  It might be better to use linked lists. */

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore

  /** Slot used for passing data to a waiting receive. */
  private var slot: A = _

  def send(x: A) = {
    mutex.down
    // search receiverQueue
    val len = receiverQueue.length; var i = 0; var done = false
    while(i < len && !done){
      val (p,sem) = receiverQueue.dequeue()
      if(p(x)){
        done = true; slot = x; sem.up // pass baton to the receiver at (2)
      }
      else{ receiverQueue.enqueue((p,sem)); i += 1 }
    } // end of while
    if(i == len){ // have to wait
      val sem = new SignallingSemaphore
      senderQueue.enqueue((x, sem))
      mutex.up; sem.down            // wait for signal (1)
      mutex.up
    }
    // else done is true, we found a match, so return
  }

  def receive(p: A => Boolean): A = {
    mutex.down
    // search senderQueue
    val len = senderQueue.length; var i = 0
    var done = false; var result = null.asInstanceOf[A]
    while(i < len && !done){
      val (x,sem) = senderQueue.dequeue()
      if(p(x)){
        done = true; result = x; sem.up // pass baton to the sender at (1)
      }
      else{ senderQueue.enqueue((x,sem)); i += 1 }
    } // end of while
    if(done) result
    else{ // have to wait
      val sem = new SignallingSemaphore
      receiverQueue.enqueue((p,sem))
      mutex.up; sem.down               // wait for signal (2)
      val result = slot; assert(p(result))
      mutex.up; result
    }
  }

}
