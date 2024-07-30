package synchronisationObject

/** The trait for exchangers. */
trait ExchangerT[A]{
  /** Exchange x with another thread. */
  def exchange(x: A): A
}

// =======================================================

class Exchanger[A] extends ExchangerT[A]{
  /* The protocol proceeds in steps as follows.  Step 0: the first thread
   * deposits its value and waits.  Step 1: the second thread reads the
   * deposited value, deposits its value, and returns.  Step 2: the first
   * thread reads the other thread's value. */

  /** The step to happen next. */
  private var step = 0

  /** The current value being exchanged. */
  private var data: A = _

  def exchange(x: A) = synchronized{
    while(step == 2) wait()  // (1)
    if(step == 0){
      data = x; step = 1 // deposit my value
      while(step == 1) wait() // (2)
      assert(step == 2); step = 0; notifyAll() // signal to thread at (1)
      data
    }
    else{ // step = 1
      val result = data; data = x; step = 2; 
      notifyAll() // signal to thread at (2)
      result
    }
  }
}

// =======================================================

class FaultyExchanger[A] extends ExchangerT[A]{

  /** The step to happen next. */
  private var step = 0

  /** The current value being exchanged. */
  private var data: A = _

  def exchange(x: A) = synchronized{
    if(step == 0){
      data = x; step = 1 // deposit my value
      while(step == 1) wait() // (1)
      step = 0; 
      data
    }
    else{ // step = 1
      val result = data; data = x; step = 0; 
      notifyAll() // signal to thread at (1)
      result
    }
  }
}
