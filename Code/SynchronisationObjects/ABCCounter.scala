package synchronisationObject
import io.threadcso._

/** A trait for three-way synchronisation objects, concerning families A, B
  * and C.  Each thread should call the sync method matching their type, and
  * gets back the identities of the other threads with which it synchronised,
  * and also a sequence counter. */ 
trait ABCCounterT[A,B,C]{
  def syncA(a: A): (B,C,Int)
  def syncB(b: B): (A,C,Int)
  def syncC(c: C): (A,B,Int)
}

// ==================================================================

/** Implementation using semaphores. */
class ABCCounter[A,B,C] extends ABCCounterT[A,B,C]{
  // The identities of the current (or previous) threads.
  private var a: A = _
  private var b: B = _
  private var c: C = _

  private var counter = 0

  // Semaphores to signal that threads can write their identities.
  private val aClear = MutexSemaphore()
  private val bClear, cClear = SignallingSemaphore()

  // Semaphores to signal that threads can collect their results. 
  private val aSignal, bSignal, cSignal = SignallingSemaphore()

  def syncA(me: A) = {
    aClear.down         // (A1)
    a = me; bClear.up   // signal to b at (B1)
    aSignal.down        // (A2)
    val result = (b,c,counter)
    bSignal.up          // signal to b at (B2)
    result
  }

  def syncB(me: B) = {
    bClear.down         // (B1)
    b = me; cClear.up   // signal to C at (C1)
    bSignal.down        // (B2)
    val result = (a,c,counter)
    cSignal.up          // signal to c at (C2)
    result
  }

  def syncC(me: C) = {
    cClear.down         // (C1)
    c = me; aSignal.up  // signal to A at (A2)
    cSignal.down        // (C2)
    val result = (a,b,counter)
    counter += 1
    aClear.up           // signal to an A on the next round at (A1)
    result
  }
}

// ==================================================================

/** A faulty implementation using semaphores. */
class FaultyABCCounter[A,B,C] extends ABCCounterT[A,B,C]{
  // The identities of the current (or previous) threads.
  private var a: A = _
  private var b: B = _
  private var c: C = _

  private var counter = 0

  // Semaphores to signal that threads can write their identities.
  private val aClear = MutexSemaphore()
  private val bClear, cClear = SignallingSemaphore()

  // Semaphores to signal that threads can collect their results. 
  private val aSignal, bSignal, cSignal = SignallingSemaphore()

  def syncA(me: A) = {
    aClear.down         // (A1)
    a = me; bClear.up   // signal to b at (B1)
    aSignal.down        // (A2)
    // val result = (b,c,counter)
    bSignal.up          // signal to b at (B2)
    // result
    (b,c,counter)
  }

  def syncB(me: B) = {
    bClear.down         // (B1)
    b = me; cClear.up   // signal to C at (C1)
    bSignal.down        // (B2)
    val result = (a,c,counter)
    cSignal.up          // signal to c at (C2)
    result
  }

  def syncC(me: C) = {
    cClear.down         // (C1)
    c = me; aSignal.up  // signal to A at (A2)
    cSignal.down        // (C2)
    val result = (a,b,counter)
    counter += 1
    aClear.up           // signal to an A on the next round at (A1)
    result
  }
}
