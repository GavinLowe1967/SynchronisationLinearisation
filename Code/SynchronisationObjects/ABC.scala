package synchronisationObject
// import io.threadcso._

/** A trait for three-way synchronisation objects, concerning families A, B
  * and C.  Each thread should call the sync method matching their type, and
  * gets back the identities of the other threads with which it
  * synchronised. */ 
trait ABCT[A,B,C]{
  def syncA(a: A): (B,C)
  def syncB(b: B): (A,C)
  def syncC(c: C): (A,B)
}

// ==================================================================

/** Implementation using semaphores. */
class ABC[A,B,C] extends ABCT[A,B,C]{
  // The identities of the current (or previous) threads.
  private var a: A = _
  private var b: B = _
  private var c: C = _

  // Semaphores to signal that threads can write their identities.
  private val aClear = new MutexSemaphore()
  private val bClear, cClear = new SignallingSemaphore()

  // Semaphores to signal that threads can collect their results. 
  private val aSignal, bSignal, cSignal = new SignallingSemaphore()

  def syncA(me: A) = {
    aClear.down         // (A1)
    a = me; bClear.up   // signal to b at (B1)
    aSignal.down        // (A2)
    val result = (b,c)
    bSignal.up          // signal to b at (B2)
    result
  }

  def syncB(me: B) = {
    bClear.down         // (B1)
    b = me; cClear.up   // signal to C at (C1)
    bSignal.down        // (B2)
    val result = (a,c)
    cSignal.up          // signal to c at (C2)
    result
  }

  def syncC(me: C) = {
    cClear.down         // (C1)
    c = me; aSignal.up  // signal to A at (A2)
    cSignal.down        // (C2)
    val result = (a,b)
    aClear.up           // signal to an A on the next round at (A1)
    result
  }
}

// ==================================================================

/** Incorrect implementation using semaphores. */
class FaultyABC[A,B,C] extends ABCT[A,B,C]{
  // The identities of the current (or previous) threads.
  private var a: A = _
  private var b: B = _
  private var c: C = _

  // Semaphores to signal that threads can write their identities.
  private val aClear = new MutexSemaphore()
  private val bClear, cClear = new SignallingSemaphore()

  // Semaphores to signal that threads can collect their results. 
  private val aSignal, bSignal, cSignal = new SignallingSemaphore()

  def syncA(me: A) = {
    aClear.down         // (A1)
    a = me; bClear.up   // signal to b at (B1)
    aSignal.down        // (A2)
    bSignal.up          // signal to b at (B2)
    (b,c)               // This is an error
  }

  def syncB(me: B) = {
    bClear.down         // (B1)
    b = me; cClear.up   // signal to C at (C1)
    bSignal.down        // (B2)
    val result = (a,c)
    cSignal.up          // signal to c at (C2)
    result
  }

  def syncC(me: C) = {
    cClear.down         // (C1)
    c = me; aSignal.up  // signal to A at (A2)
    cSignal.down        // (C2)
    val result = (a,b)
    aClear.up           // signal to an A on the next round at (A1)
    result
  }
}

// ==================================================================

import scala.util.Random

/** An implementation that can get stuck.  Here threads can randomly forget to
  * signal. */
class DeadlockABC[A,B,C] extends ABCT[A,B,C]{
  // The identities of the current (or previous) threads.
  private var a: A = _
  private var b: B = _
  private var c: C = _

  // Semaphores to signal that threads can write their identities.
  private val aClear = new MutexSemaphore()
  private val bClear, cClear = new SignallingSemaphore()

  // Semaphores to signal that threads can collect their results. 
  private val aSignal, bSignal, cSignal = new SignallingSemaphore()

  def syncA(me: A) = {
    aClear.down         // (A1)
    a = me; bClear.up   // signal to b at (B1)
    aSignal.down        // (A2)
    val result = (b,c)
    if(Random.nextInt(50) != 0)
      bSignal.up        // signal to b at (B2)
    result
  }

  def syncB(me: B) = {
    bClear.down         // (B1)
    b = me; cClear.up   // signal to C at (C1)
    bSignal.down        // (B2)
    val result = (a,c)
    if(Random.nextInt(50) != 0)
      cSignal.up        // signal to c at (C2)
    result
  }

  def syncC(me: C) = {
    cClear.down         // (C1)
    c = me; aSignal.up  // signal to A at (A2)
    cSignal.down        // (C2)
    val result = (a,b)
    if(Random.nextInt(50) != 0)
      aClear.up         // maybe signal to an A on the next round at (A1)
    result
  }
}
