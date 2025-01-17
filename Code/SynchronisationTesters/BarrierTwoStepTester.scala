package synchronisationTester

import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationObject.{
  BarrierT,Barrier,Barrier2,FaultyBarrier,FaultyBarrier2,FaultyBarrier3, 
  FaultyBarrier4}

/** Two-step linearisation synchronisation tester for a barrier. */
object BarrierTwoStepTester extends Tester{

  /** Number of worker threads to run. */
  var numThreads = 4

  /** Specification object for a barrier for `n` threads. 
    * @param count the number of `sync1` invocations that have occurred in the 
    * current synchronisation.
    * @param canReturn array showing which threads can return, i.e. for which 
    * `sync2` can be performed.  */
  class BarrierSpec(n: Int, val count: Int, val canReturn: Array[Boolean]){
    /** Constructor for initial state. */
    def this(n: Int) = this(n, 0, new Array[Boolean](n))

    /** The first part of the synchronisation. */
    def sync1(id: Int): (Unit, BarrierSpec) = {
      assert(!canReturn(id) && count < n)
      if(count == n-1) ((), new BarrierSpec(n, 0, Array.fill(n)(true)))
      else ((), new BarrierSpec(n, count+1, canReturn))
    }

    /** The second part of the synchronisation. */
    def sync2(id: Int): (Unit, BarrierSpec) = {
      require(canReturn(id))
      val newCanReturn = canReturn.clone; newCanReturn(id) = false
      ((), new BarrierSpec(n, count, newCanReturn))
    }

    override def hashCode = {
      // Create hashCode by interpreting entries in `canReturn` as bits and
      // shifting four places left, and adding `count`.  This is an injection
      // for n < 16.
      var h = count; var x = 16
      for(i <- 0 until n){ if(canReturn(i)) h += x; x *= 2 }
      h
    }

    override def equals(that: Any) = that match{
      case bs: BarrierSpec => 
        bs.count == count && bs.canReturn.sameElements(canReturn)
    }
  }

  /** A worker in the testing system. */
  def worker(me: Int, log: LinearizabilityLog[BarrierSpec,BarrierT]) = {
    for(i <- 0 until iters){
      log(_.sync(me), s"$me: sync", _.sync1(me))
      log((b: BarrierT) => (), s"$me: sync2", _.sync2(me))
    } 
  }

  var faulty = false; var faulty2 = false; 
  var faulty3 = false; var faulty4 = false; var version2 = false

  /** Run a single test. */
  def doTest: Boolean = {
    val barrier: BarrierT = 
      if(version2) new Barrier2(numThreads)
      else if(faulty) new FaultyBarrier(numThreads)
      else if(faulty2) new FaultyBarrier2(numThreads) 
      else if(faulty3) new FaultyBarrier3(numThreads) 
      else if(faulty4) new FaultyBarrier4(numThreads) 
      else new Barrier(numThreads)
    val spec = new BarrierSpec(numThreads)
    val tester = LinearizabilityTester[BarrierSpec,BarrierT](
      spec, barrier, numThreads, worker _)
    tester() > 0
  }

  // override def runsPerDot = 1

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    var profiling = false; var interval = 50; var timing = false; 
    while(i < args.length) args(i) match{
      case "-p" => numThreads = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1
      case "--faulty3" => faulty3 = true; i += 1
      case "--faulty4" => faulty4 = true; i += 1
      case "--version2" => version2 = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }
}
