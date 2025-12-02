package synchronisationObject

trait MenWomenT{
  def manSync(me: Int): Int
  def womanSync(me: Int): Int
}

// ==================================================================

class MenWomen extends MenWomenT{
  /* We proceed in stages.  Stage 0: man writes name and waits.  Stage 1: woman
   * writes name, returns man's name.  Stage 2: man reads woman's name. */
  private var stage = 0
  private var him = -1
  private var her = -1

  def manSync(me: Int): Int = synchronized{
    while(stage != 0) wait()         // (1)
    him = me; stage = 1; notifyAll() // signal to waiting woman at (3)
    while(stage != 2) wait           // (2)
    stage = 0; notifyAll(); her      // signal to next man at (1)
  }

  def womanSync(me: Int): Int = synchronized{
    while(stage != 1) wait                // (3)
    her = me; stage = 2; notifyAll(); him // signal to man at (2)
  }
}

// =======================================================

/** This version is faulty. */
class FaultyMenWomen extends MenWomenT{
  //println("FaultyMenWomen")
  private var him: Option[Int] = None
  private var her: Option[Int] = None

  def manSync(me: Int): Int = synchronized{
    while(him.nonEmpty) wait()  // (1)
    him = Some(me); notifyAll() // signal to waiting woman at (4)
    //println(s"Written $me")
    while(her.isEmpty) wait()   // (2)
    val Some(res) = her
    her = None; notifyAll()     // signal to waiting woman at (3)
    //println(s"$me read $res")
    res
  }

  def womanSync(me: Int): Int = synchronized{
    while(her.nonEmpty) wait()  // (3)
    her = Some(me); notifyAll() // signal to waiting man at (2)
    //println(s"Written $me")
    while(him.isEmpty) wait()   // (4)
    val Some(res) = him
    him = None; notifyAll()     // signal to waiting man at (1)
    //println(s"$me read $res")
    res
  }

  /* This goes wrong as follows: (1) man1 writes his name and waits; (2) woman1
   * writes her name, returns man1's name; (3) man2 writes his name, returns
   * woman1's name; (4) woman2 writes her name, returns man2's name; (5) man1
   * returns woman2's name.  Alternatively, can get stuck after step (3) if no
   * more women try. */
}

// =======================================================

/** This version deadlocks because it uses notify() rather than notifyAll(). */
class DeadlockMenWomen extends MenWomenT{
  /* We proceed in stages.  Stage 0: man writes name and waits.  Stage 1: woman
   * writes name, returns man's name.  Stage 2: man reads woman's name. */
  private var stage = 0
  private var him = -1
  private var her = -1

  def manSync(me: Int): Int = synchronized{
    while(stage != 0) wait()            // (1)
    him = me; stage = 1; notify()       // signal to waiting woman at (3)
    while(stage != 2) wait()            // (2)
    stage = 0; notify(); her            // signal to next man at (1)
  }

  def womanSync(me: Int): Int = synchronized{
    while(stage != 1) wait()             // (3)
    her = me; stage = 2; notify(); him   // signal to man at (2)
  }
}


// =======================================================

import ox.scl.{MutexSemaphore,SignallingSemaphore}

class FaultyMenWomen2 extends MenWomenT{
  /* Semaphores to provide mutual exclusion between men, resp. women.  Note: a
   * man and a woman may run concurrently. */ 
  private val manMutex, womanMutex = new MutexSemaphore

  /* Semaphores for signalling by a man, resp. a woman. */
  private val manSignal, womanSignal = new SignallingSemaphore

  /* Identities of current man, woman. */
  private var man, woman = -1

  def manSync(me: Int) = {
    manMutex.down
    man = me; manSignal.up
    womanSignal.down // wait for signal
    val her = woman; womanMutex.up; her
  }

  def womanSync(me: Int) = {
    womanMutex.down
    woman = me; womanSignal.up
    manSignal.down // wait for signal
    val him = man; manMutex.up; him
  }


}
