package synchronisationObject

// import ox.scl._
import scala.collection.mutable.Queue

/** Trait defining terminating queue arrays. */
trait TerminatingQueueArrayT[A]{
  /** Add x, performed by thread me. */
  def enqueue(me: Int, x: A): Unit  

  /** Try to obtain a value, performed by thread me.  Returns None in the
    * termination case. */
  def dequeue(me: Int): Option[A]
}

// =======================================================

/** A correct implementation of TerminatingQueueArrayT. */
class TerminatingQueueArray[A](n: Int) extends TerminatingQueueArrayT[A]{
  /** Queues holding the data.  Each queues(t) is added to only be thread t, but
    * may be dequeued from by any thread.  Each operation on queues(t) is
    * protected by a synchronized block on itself. */
  private val queues = Array.fill(n)(new Queue[A]) 

  /** The number of threads currently trying to terminate.  Protected by a
    * synchronized block on the TerminatingQueueArray object.  */
  private var tryingToTerminateCount = 0

  private def incTryingToTerminateCount() = 
    synchronized{ tryingToTerminateCount += 1 }

  private def decTryingToTerminateCount() = 
    synchronized{ tryingToTerminateCount -= 1 }

  private def canTerminate = synchronized{ tryingToTerminateCount } == n 

  /** Add x, performed by thread me. */
  def enqueue(me: Int, x: A): Unit = {
    require(0 <= me && me < n); val q = queues(me)
    q.synchronized{ q.enqueue(x) }
  }

  /** Try to obtain a value, performed by thread me. */
  def dequeue(me: Int): Option[A] = {
    require(0 <= me && me < n)
    var result = null.asInstanceOf[A]; var done = false; val q = queues(me)
    // Try to dequeue from queues(me)
    q.synchronized{ if(q.nonEmpty){ result = q.dequeue(); done = true } }
    if(done) Some(result)
    else{
      // This thread is trying to terminate.  But continue trying to dequeue
      // from other queues.
      incTryingToTerminateCount(); var ix = me; var terminating = false
      while(!done){
        val q = queues(ix)
        q.synchronized{
          if(q.nonEmpty){
            result = q.dequeue(); done = true; decTryingToTerminateCount()
          }
        } // end of "q.synchronized" block.
        if(!done && canTerminate){ terminating = true; done = true }
        else ix = (ix+1)%n
      } // end of while loop
      if(terminating) None else Some(result)
    } // end of "else" branch
  }
}

// =======================================================


/** A correct implementation of TerminatingQueueArrayT. */
class FaultyTerminatingQueueArray[A](n: Int) extends TerminatingQueueArrayT[A]{
  /** Queues holding the data.  Each queues(t) is added to only be thread t, but
    * may be dequeued from by any thread.  Each operation on queues(t) is
    * protected by a synchronized block on itself. */
  private val queues = Array.fill(n)(new Queue[A]) 

  /** The number of threads currently trying to terminate.  Protected by a
    * synchronized block on the TerminatingQueueArray object.  */
  private var tryingToTerminateCount = 0

  private def incTryingToTerminateCount() = 
    synchronized{ tryingToTerminateCount += 1 }

  private def decTryingToTerminateCount() = 
    synchronized{ tryingToTerminateCount -= 1 }

  private def canTerminate = synchronized{ tryingToTerminateCount } == n 

  /** Add x, performed by thread me. */
  def enqueue(me: Int, x: A): Unit = {
    require(0 <= me && me < n); val q = queues(me)
    q.synchronized{ q.enqueue(x) }
  }

  /** Try to obtain a value, performed by thread me. */
  def dequeue(me: Int): Option[A] = {
    require(0 <= me && me < n)
    var result = null.asInstanceOf[A]; var done = false; val q = queues(me)
    // Try to dequeue from queues(me)
    q.synchronized{ if(q.nonEmpty){ result = q.dequeue(); done = true } }
    if(done) Some(result)
    else{
      // This thread is trying to terminate.  But continue trying to dequeue
      // from other queues.
      incTryingToTerminateCount(); var ix = me; var terminating = false
      while(!done){
        val q = queues(ix)
        q.synchronized{
          if(q.nonEmpty){
            result = q.dequeue(); done = true // ; decTryingToTerminateCount()
          }
        } // end of "q.synchronized" block.
        // The following is a bug.  The decTryingToTerminateCount should be
        // done before releasing the lock on q.
        if(done) decTryingToTerminateCount()
        if(!done && canTerminate){ terminating = true; done = true }
        else ix = (ix+1)%n
      } // end of while loop
      if(terminating) None else Some(result)
    } // end of "else" branch
  }
}

