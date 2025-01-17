package synchronisationObject

/** The trait for a barrier synchronisation. 
  * @tparam T the type of threads.*/
trait ResignableBarrierT[T]{
  /** Enrol with the barrier. */
  def enrol(id: T): Unit

  /** Resign from the barrier. */
  def resign(id: T): Unit

  /** Synchronise.  This blocks until all currently enrolled threads call
    * `sync`. */
  def sync(id: T): Unit
}

// ==================================================================

/** An implementation based on a JVM monitor. 
  * @param faulty if true, use a faulty implementation*/
class ResignableBarrier[T](faulty: Boolean) extends ResignableBarrierT[T]{
  /** Threads currently enrolled in the barrier. */
  private var current = new scala.collection.mutable.HashSet[T]

  /** Number of threads currently enrolled in the barrier.  Equals
    * current.size. */
  private var currentCount = 0

  /** The number of waiting threads. */
  private var count = 0 

  /** Are we in the leaving phase? */
  private var leaving = false 

  /** Enrol with the barrier. */
  def enrol(id: T) = synchronized{
    // while(leaving) wait() // wait for current sync to finish
    val added = current.add(id); assert(added); currentCount += 1
  }

  /** Resign from the barrier. */
  def resign(id: T) = synchronized{
    //while(leaving) wait() // wait for current sync to finish
    val removed = current.remove(id); assert(removed); currentCount -= 1
    if(count == currentCount && count > 0){ leaving = true; notifyAll() }
  }

  /** Perform a barrier synchronisation. */
  def sync(id: T) = synchronized{
    assert(current.contains(id))
    if(!faulty) while(leaving) wait() // Wait for previous round to finish
    // Note: the faulty version will not wait above, and so might incorrectly 
    // return immediately
    if(currentCount > 1){ // if currentCount == 1, can return immediately
      count += 1
      if(count == currentCount){ leaving = true; count -= 1; notifyAll() }
      else{
        while(!leaving) wait()
        count -= 1
        if(count == 0){
          leaving = false; notifyAll() // Allow next round to continue.
        }
      }
    }
  }
}
/* Note: the faulty version doesn't lead to deadlocks when used with the
 * tester: the tester causes all threads to resign at the end, which unsticks
 * remaining threads. */

// =======================================================

/** A faulty implementation, based on FaultyBarrier4. */
/*
class FaultyResignableBarrier[T](faulty: Boolean) extends ResignableBarrierT[T]{
  /** Threads currently enrolled in the barrier. */
  private var current = new scala.collection.mutable.HashSet[T]

  /** Number of threads currently enrolled in the barrier.  Equals
    * current.size. */
  private var currentCount = 0

  /** The number of waiting threads. */
  private var count = 0 

  /** Are we in the leaving phase? */
  private var leaving = false 

  /** Enrol with the barrier. */
  def enrol(id: T) = synchronized{
    // while(leaving) wait() // wait for current sync to finish
    val added = current.add(id); assert(added); currentCount += 1
  }

  /** Resign from the barrier. */
  def resign(id: T) = synchronized{
    //while(leaving) wait() // wait for current sync to finish
    val removed = current.remove(id); assert(removed); currentCount -= 1
    if(count == currentCount && count > 0){ leaving = true; notifyAll() }
  }

  /** Perform a barrier synchronisation. */
  def sync(id: T) = synchronized{
    assert(current.contains(id))
    if(!faulty) while(leaving) wait() // Wait for previous round to finish
    // Note: the faulty version will not wait above, and so might incorrectly 
    // return immediately
    if(currentCount > 1){ // if currentCount == 1, can return immediately
      count += 1
      if(count == currentCount){ leaving = true; count -= 1; notifyAll() }
      else{
        while(!leaving) wait()
        count -= 1
        if(count == 0){
          leaving = false; notifyAll() // Allow next round to continue.
        }
      }
    }
  }
}
 */
