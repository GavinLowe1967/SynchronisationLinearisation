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
  private var him: Option[Int] = None
  private var her: Option[Int] = None

  def manSync(me: Int): Int = synchronized{
    while(him.nonEmpty) wait()  // (1)
    him = Some(me); notifyAll() // signal to waiting woman at (4)
    while(her.isEmpty) wait()   // (2)
    val Some(res) = her
    her = None; notifyAll()     // signal to waiting woman at (3)
    res
  }

  def womanSync(me: Int): Int = synchronized{
    while(her.nonEmpty) wait()  // (3)
    her = Some(me); notifyAll() // signal to waiting man at (2)
    while(him.isEmpty) wait()   // (4)
    val Some(res) = him
    him = None; notifyAll()     // signal to waiting man at (1)
    res
  }

}
