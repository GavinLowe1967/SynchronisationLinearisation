package synchronisationTester

// import io.threadcso._
import scala.util.Random

import synchronisationTesting.{HistoryLog,ThreadUtil,BinaryStatelessTester}
import synchronisationObject.{
  FilterChanT, FilterChan, FaultyFilterChan, SemaphoreFilterChan, 
  DeadlockingFilterChan
}

/** A testing file. */
object FilterChanTester{
  /** Number of worker threads to run. */
  var p = 4

  /** Number of iterations per worker thread. */
  var iters = 20

  /** The maximum value sent.  A larger value will make it easier to test
    * whether a matching exists.  And the implementation is data independent,
    * so this will not affect the likelihood of finding errors.  A smaller
    * value stresses the tester more. */
  var MaxVal = 100

  /** Do we check the progress condition? */
  var progressCheck = false

  /** The timeout time with the progress check. */
  var timeout = -1

  trait Parity
  case object Even extends Parity
  case object Odd extends Parity

  // Representation of operations within the log
  trait Op
  case class Send(x: Int) extends Op
  case class Receive(parity: Parity)  extends Op

  type FChan = FilterChanT[Int]

  /** The specification class. */
  object Spec{
    def sync(x: Int, parity: Parity) = {
      require(parity == Even && x%2 == 0 || parity == Odd && x%2 == 1)
      ((), x)
    }
  }

  /** Mapping showing how synchronisations of concrete operations correspond to
    * operations of the specification object. */
  def matching: PartialFunction[(Op,Op), (Any,Any)] = {
    case (Send(x), Receive(parity: Parity)) => Spec.sync(x, parity) 
  }

  /** Do a send on c, and store in log, associated with me.  Send an even number
    * iff even. */
  @inline def doSend(c: FChan, even: Boolean, me: Int, log: HistoryLog[Op]) = {
    val x = 2*Random.nextInt(MaxVal/2)+(if(even) 0 else 1)
    log(me, c.send(x), Send(x))
  }

  /** Do a send on c, and store in log, associated with me.  Require even
    * numbers if even, otherwise odd. */
  @inline def doReceive(c: FChan, even: Boolean, me: Int, log: HistoryLog[Op]) = 
    log(me, c.receive(x => x%2 == (if(even) 0 else 1)),
      Receive(if(even) Even else Odd))

  /** A worker.  The behaviour of worker me depends upon me%4: if me%4 = 0, it
    * sends even numbers; if me%4 = 1, it receives even numbers; if me%4 = 2,
    * it sends odd numbers; if me%4 = 3, it receives odd numbers. */
  def worker(c: FChan)(me: Int, log: HistoryLog[Op]) = {
    val me4 = me%4
    for(i <- 0 until iters)
      if(me%2 == 0) // Send even or odd, if me4 = 0 or 2, respectively.
        doSend(c, me4 == 0, me, log)
      else // Receive even or odd, if me4 = 1 or 3, respectively
        doReceive(c, me4 == 1, me, log)
  }

  /** A worker that performs randomly chosen operations.  A system built from
    * these is likely to deadlock. */
  def worker1(c: FChan)(me: Int, log: HistoryLog[Op]) = {
    for(i <- 0 until iters)
      if(Random.nextBoolean()) // Send even or odd
        doSend(c, Random.nextBoolean(), me, log)
      else // Receive even or odd
        doReceive(c, Random.nextBoolean(), me, log)
  }

  /* Flags to decide which implementation to use. */
  private var faulty = false // FaultyFilterChan
  private var semaphore = false // SemaphoreFilterChan
  private var deadlock = false // DeadlockingChan

  /* Default is FilterChan. */

  /** Do a single test. */
  def doTest = {
    val c: FChan = 
      if(faulty) new FaultyFilterChan[Int] 
      else if(semaphore) new SemaphoreFilterChan[Int]
      else if(deadlock) new DeadlockingFilterChan[Int]
      else new FilterChan[Int]
    if(progressCheck){
      val bst = new BinaryStatelessTester[Op](worker1(c), p, matching)
      if(!bst(timeout)) sys.exit()
    }
    else{
      val bst = new BinaryStatelessTester[Op](worker(c), p, matching)
      if(!bst()) sys.exit()
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2

      case "--faulty" => faulty = true; i += 1
      case "--semaphore" => semaphore = true; i += 1
      case "--deadlock" => deadlock = true; i += 1

      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }
    assert(p%4 == 0 || progressCheck)

    val start = java.lang.System.nanoTime
    for(i <- 0 until reps){ 
      doTest; if(i%100 == 0 || progressCheck && i%10 == 0) print(".") 
    }
    val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
    println(); println(s"$duration ms")
  }
}
