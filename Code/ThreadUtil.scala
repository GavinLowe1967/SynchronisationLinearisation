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
}
