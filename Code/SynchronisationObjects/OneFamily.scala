package synchronisationObject

// import io.threadcso._

/** Synchronisation object for the following problem.  Threads have integer
  * identities in a known range.  Each thread must synchronise with each
  * other, precisely once. */
trait OneFamilyT{
  def sync(me: Int): Int
}

// ==================================================================

class OneFamily(n: Int) extends OneFamilyT{
  /** synced(i)(j) is true if threads i and j have synchronised. */
  private val synced = Array.ofDim[Boolean](n,n)

  /** Semaphores on which to signal to processes. */
  private val semaphores = Array.fill(n)(new SignallingSemaphore())

  /** Flags showing which processes are waiting. */
  private val waiting = Array.ofDim[Boolean](n)

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore()

  /** Identity of thread with which an awakening thread is paired. */
  private var lastSync = -1

  def sync(me: Int): Int = {
    require(0 <= me && me < n)
    require((0 until n).exists(him => him != me && !synced(me)(him)))
    mutex.down
    var him = 0
    while(him < n && (him == me || synced(me)(him) || !waiting(him))) him += 1
    if(him < n){ // sync with him
      lastSync = me; synced(me)(him) = true
      semaphores(him).up; him                     // signal to him at (1)
    }
    else{   // need to wait
      waiting(me) = true; mutex.up; semaphores(me).down // wait (1)
      waiting(me) = false; val him = lastSync
      mutex.up; synced(me)(him) = true; him
    }
  }
}

// ==================================================================

class OneFamilyFaulty(n: Int) extends OneFamilyT{
  /** synced(i)(j) is true if threads i and j have synchronised. */
  private val synced = Array.ofDim[Boolean](n,n)

  /** Semaphores on which to signal to processes. */
  private val semaphores = Array.fill(n)(new SignallingSemaphore())

  /** Flags showing which processes are waiting. */
  private val waiting = Array.ofDim[Boolean](n)

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore()

  /** Identity of thread with which an awakening thread is paired. */
  private var lastSync = -1

  def sync(me: Int): Int = {
    require(0 <= me && me < n)
    require((0 until n).exists(him => him != me && !synced(me)(him)))
    mutex.down
    var him = 0
    while(him < n && (him == me || synced(me)(him) || !waiting(him))) him += 1
    if(him < n){ // sync with him
      lastSync = me; synced(me)(him) = true
      semaphores(him).up; him                     // signal to him at (1)
    }
    else{   // need to wait
      waiting(me) = true; mutex.up; semaphores(me).down // wait (1)
      waiting(me) = false; synced(me)(lastSync) = true
      mutex.up; lastSync // this is an error
    } 
  }
}
