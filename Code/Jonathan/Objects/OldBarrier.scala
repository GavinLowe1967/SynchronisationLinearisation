package synchronisationObject

/** The trait for a barrier synchronisation. */
trait OldBarrierT {
  def sync: Unit
}

// ==================================================================

/** Implementation using the SCL barrier. */
// Assumes this object is used by the same n threads, maybe multiple times.
class OldBarrier(n: Int) extends OldBarrierT {

  val idBar = new ox.scl.Barrier(n) 

  private var nextID = 0
  // Returns successive values in range [1..n]
  private def getNext: Int =
    synchronized { // Called exactly once by each client thread.
      nextID += 1
      nextID
    }

  private val myID = new ThreadLocal[Int]() // Will return 0 if uninitialized.

  private def getMyID: Int = {
    var id = myID.get
    if (id == 0) {
      id = getNext
      myID.set(id)
    }
    id - 1
  }

  // Require an implementation of sync:Unit
  def sync = {

    val me = getMyID 
    idBar.sync(me) // Call sync from SCL OldBarrier

  }
}

// ==================================================================

/** An obviously faulty barrier. */
class FaultyOldBarrier(n: Int) extends OldBarrierT {
  def sync = {} // return immediately!
}

// ==================================================================

/** A faulty version that doesn't guard against spurious wake-ups. */
class FaultyOldBarrier2(n: Int) extends OldBarrierT {
  private var count = 0

  def sync = synchronized {
    count += 1
    if (count == n) { count = 0; notifyAll() }
    else wait() // This doesn't guard against spurious wake-ups, so is an
    // error, but testing struggles to find it.
  }
}

// ==================================================================

/** Another faulty version. */
class FaultyOldBarrier3(n: Int) extends OldBarrierT {
  private var count = 0
  private var leaving = false

  def sync = synchronized {
    count += 1
    if (count == n) {
      leaving = true; /* count -= 1;*/
      notifyAll()
    } else {
      while (!leaving) wait()
      count -= 1
      if (count == 0) leaving = false
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
class OldBarrier2(n: Int) extends OldBarrierT {
  private var count = 0 // The number of waiting threads.
  private var leaving = false // Are we in the leaving phase?

  def sync = synchronized {
    while (leaving) wait() // Wait for previous round to finish
    count += 1
    if (count == n) { leaving = true; count -= 1; notifyAll() }
    else {
      while (!leaving) wait()
      count -= 1
      if (count == 0) {
        leaving = false; notifyAll() // Allow next round to continue.
      }
    }
  }
}

