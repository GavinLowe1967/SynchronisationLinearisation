package synchronisationObject

// import io.threadcso._

/** Synchronisation object for the following problem.  There are two families
  * of processes, of types A and B, of known sizes, and with integer
  * identities.  Each member of A must synchronise with each member of B,
  * precisely once. */
trait TwoFamiliesT{
  def syncA(a: Int) : Int

  def syncB(b: Int) : Int
}

// ==================================================================

/** A synchronisation object for use by m A-threads and n B-threads. */
class TwoFamilies(m: Int, n: Int) extends TwoFamiliesT{
  /** synced(i)(j) is true if threads i (of family A) and j (of family B) have
    * synchronised. */
  private val synced = Array.ofDim[Boolean](m,n)

  /** Semaphores on which to signal to A processes. */
  private val aSemaphores = Array.fill(m)(new SignallingSemaphore())

  /** Semaphores on which to signal to B processes. */
  private val bSemaphores = Array.fill(n)(new SignallingSemaphore())

  /** Flags showing which A processes are waiting. */
  private val aWaiting = Array.ofDim[Boolean](m)

  /** Flags showing which B processes are waiting. */
  private val bWaiting = Array.ofDim[Boolean](n)

  /** Identity of thread with which an awakening thread is paired. */
  private var lastSync = -1

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore()

  def syncA(a: Int): Int = {
    require(0 <= a && a < m)
    // There must be some b with which this a has not yet synced. 
    require((0 until n).exists(b => !synced(a)(b)))
    mutex.down
    var b = 0
    while(b < n && (synced(a)(b) || !bWaiting(b))) b += 1
    if(b < n){ // sync with b
      assert(!synced(a)(b) && bWaiting(b))
      lastSync = a; synced(a)(b) = true 
      bSemaphores(b).up                              // Pass baton to b at (2)
      b
    }
    else{ // need to wait
      aWaiting(a) = true; mutex.up; aSemaphores(a).down // wait (1)
      aWaiting(a) = false; val b = lastSync; mutex.up; b
    }
  }

  def syncB(b: Int) = {
    require(0 <= b && b < n)
    // There must be some a with which this b has not yet synced. 
    require((0 until m).exists(a => !synced(a)(b)))
    mutex.down
    var a = 0
    while(a < m && (synced(a)(b) || !aWaiting(a))) a += 1
    if(a < m){ // sync with a
      assert(!synced(a)(b) && aWaiting(a))
      lastSync = b; synced(a)(b) = true
      aSemaphores(a).up                             // Pass baton to a at (1)
      a
    }
    else{ // need to wait
      bWaiting(b) = true; mutex.up; bSemaphores(b).down // wait (2)
      bWaiting(b) = false; val a = lastSync; mutex.up; a
    }
  }
}

// ==================================================================

/** A synchronisation object for use by m A-threads and n B-threads. */
class FaultyTwoFamilies(m: Int, n: Int) extends TwoFamiliesT{
  /** synced(i)(j) is true if threads i (of family A) and j (of family B) have
    * synchronised. */
  private val synced = Array.ofDim[Boolean](m,n)

  /** Semaphores on which to signal to A processes. */
  private val aSemaphores = Array.fill(m)(new SignallingSemaphore())

  /** Semaphores on which to signal to B processes. */
  private val bSemaphores = Array.fill(n)(new SignallingSemaphore())

  /** Flags showing which A processes are waiting. */
  private val aWaiting = Array.ofDim[Boolean](m)

  /** Flags showing which B processes are waiting. */
  private val bWaiting = Array.ofDim[Boolean](m)

  /** Identity of A thread with which an awakening B thread is paired. */
  private var lastSyncA = -1

  /** Identity of B thread with which an awakening A thread is paired. */
  private var lastSyncB = -1

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore()

  def syncA(a: Int): Int = {
    require(0 <= a && a < m)
    // There must be some b with which this a has not yet synced. 
    require((0 until n).exists(b => !synced(a)(b)))
    mutex.down
    var b = 0
    while(b < n && (synced(a)(b) || !bWaiting(b))) b += 1
    if(b < n){ // sync with b
      assert(!synced(a)(b) && bWaiting(b))
      lastSyncA = a; synced(a)(b) = true 
      bSemaphores(b).up                  // Pass baton to b at (2)
      b
    }
    else{ // need to wait
      aWaiting(a) = true; mutex.up; aSemaphores(a).down // wait (1)
      aWaiting(a) = false; mutex.up; lastSyncB // this is an error
    }
  }

  def syncB(b: Int) = {
    require(0 <= b && b < n)
    // There must be some a with which this b has not yet synced. 
    require((0 until m).exists(a => !synced(a)(b)))
    mutex.down
    var a = 0
    while(a < m && (synced(a)(b) || !aWaiting(a))) a += 1
    if(a < m){ // sync with a
      assert(!synced(a)(b) && aWaiting(a))
      lastSyncB = b; synced(a)(b) = true
      aSemaphores(a).up                      // Pass baton to a at (1)
      a
    }
    else{ // need to wait
      bWaiting(b) = true; mutex.up; bSemaphores(b).down // wait (2)
      bWaiting(b) = false; mutex.up; lastSyncA // this is an error
    }
  }
}
