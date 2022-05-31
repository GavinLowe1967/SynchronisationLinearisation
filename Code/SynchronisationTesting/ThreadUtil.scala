package synchronisationTesting

/** A general utility for making systems of parallel components */
object ThreadUtil{
  /** Create a thread that performs comp */
  def mkThread(comp: => Unit) : Thread = 
    new Thread(new Runnable{ def run = comp })  

  /** Create a system of processes `proc(i)` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate. */
  def runIndexedSystem(p: Int, proc: Int => Unit) = {
    val threads = Array.tabulate(p)(i => mkThread(proc(i)))
    threads.foreach(_.start)
    threads.foreach(_.join)
  }


  /** Create a system of processes `proc(i)` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate, but if any thread throws
    * an exception, then throw an exception. */
  def runIndexedSystemStrict(p: Int, proc: Int => Unit) = {
    var done = new java.util.concurrent.atomic.AtomicInteger
    val threads = Array.tabulate(p)(i => mkThread{proc(i); done.getAndIncrement})
    threads.foreach(_.start)
    threads.foreach(_.join)
    assert(done.get == p)
  }

  /** The run time (in millis) that is taken to constitute a deadlock. */
  val Duration = 1_000

  import java.lang.System.currentTimeMillis

  /** Create a system of processes `proc(i)` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate, or after duration ms.
    * @return true if the run took longer than duration milliseconds, which is
    * taken to represent a deadlock.  */
  def runIndexedSystemDetectDeadlock(
    p: Int, proc: Int => Unit, duration: Int = Duration)
      : Boolean = {
    // The number of completed threads
    var done = new java.util.concurrent.atomic.AtomicInteger
    // The threads.  Catch interrupts.  Increment done when finished.
    val threads = Array.tabulate(p)(i => mkThread{
      try{ proc(i); done.getAndIncrement }
      catch{ case _: InterruptedException =>  /*println("interrupted");*/ }
    })
    threads.foreach(_.start)
    val startTime = currentTimeMillis()
    while(done.get < p && currentTimeMillis()-startTime < duration){ }
    if(done.get < p){ 
      // println("ThreadUtil: Deadlock!"); 
      threads.foreach(_.interrupt)
      // I think the following (or similar) is necessary as a memory fence, to
      // ensure the logs of interrupted threads are available to this main
      // thread.
      threads.foreach(_.join)     // while(done.get < p){ }
      true
    }
    else false 
    // In the else case, the write/read of `done` acts as a memory fence.
  }

}
