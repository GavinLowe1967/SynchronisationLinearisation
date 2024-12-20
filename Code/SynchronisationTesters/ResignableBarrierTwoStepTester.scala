package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationObject.{ResignableBarrierT, ResignableBarrier}
import scala.collection.immutable.HashSet

/** Two-step synchronisation linearisation tester for a resignable barrier. */
object ResignableBarrierTwoStepTester extends Tester{

  type RB = ResignableBarrierT[Int]

  /** Number of worker threads to run. */
  var numThreads = 4

  class ResignableBarrierSpec(
    val enrolled: HashSet[Int], val count: Int, val canReturn: Array[Boolean]){

    def this() = this(new HashSet[Int], 0, new Array[Boolean](numThreads))

    /** The first part of the barrier synchronisation linearisation. */
    def sync1(id: Int): (Unit, ResignableBarrierSpec) = {
      assert(enrolled(id) && count < enrolled.size && !canReturn(id))
      if(count == enrolled.size-1){
        assert(canReturn.forall(!_))
        val newCanReturn = Array.tabulate(numThreads)(x => enrolled(x))
        ((), new ResignableBarrierSpec(enrolled, 0, newCanReturn))
      }
      else ((), new ResignableBarrierSpec(enrolled, count+1, canReturn))
    }

    def sync2(id: Int): (Unit, ResignableBarrierSpec) = {
      require(canReturn(id))
      val newCanReturn = canReturn.clone; newCanReturn(id) = false
      ((), new ResignableBarrierSpec(enrolled, count, newCanReturn))
    }

    def enrol(id: Int): (Unit, ResignableBarrierSpec) = {
      assert(!enrolled(id) && !canReturn(id))
      ((), new ResignableBarrierSpec(enrolled + id, count, canReturn))
    }

    def resign(id: Int): (Unit, ResignableBarrierSpec) = {
      assert(enrolled(id) && !canReturn(id))
      // This is unnecessary.  Under the circumstances it tries to capture,
      // the resign will happen before any concrete sync returns, so the
      // resign will be linearised before any of the sync1s.
      // if(count == enrolled.size-1 && count > 0){
      //   val newCanReturn = 
      //     Array.tabulate(numThreads)(x => x != id && enrolled(x))
      //   ((), new ResignableBarrierSpec(enrolled - id, 0, newCanReturn))
      // }
      // else 
      ((), new ResignableBarrierSpec(enrolled - id, count, canReturn))
    }

    override def hashCode = {
      var h = count+enrolled.hashCode; var x = 16
      for(i <- 0 until numThreads){ if(canReturn(i)) h += x; x *= 2 }
      h
    }

    override def equals(that: Any) = that match{
      case rbs: ResignableBarrierSpec => 
        rbs.enrolled == enrolled && rbs.count == count && 
        rbs.canReturn.sameElements(canReturn)
    }
  } // end of ResignableBarrierSpec

  /** The probability of an enrolled worker choosing to call `sync`. */
  var syncProb = 0.7

  /** A worker. */
  def worker(me: Int, log: LinearizabilityLog[ResignableBarrierSpec,RB]) = {
    var enrolled = false
    for(i <- 0 until iters){
      if(enrolled){
        if(Random.nextFloat() < syncProb){
          log(_.sync(me), s"$me: sync", _.sync1(me))
          log((b: RB) => (), s"$me: sync2", _.sync2(me))
        }
        else{ log(_.resign(me), s"resign($me)", _.resign(me)); enrolled = false }
      }
      else{ log(_.enrol(me), s"enrol($me)", _.enrol(me)); enrolled = true }
    }
    // Resign at the end, to avoid deadlocks
    if(enrolled) log(_.resign(me), s"resign($me)", _.resign(me))
    // else{
    //   log(_.enrol(me), s"enrol($me)", _.enrol(me))
    //   if(me == 0){
    //     log(_.sync(me), s"$me: sync", _.sync1(me))
    //     log((b: RB) => (), s"$me: sync2", _.sync2(me))
    //   }
    //   else{ Thread.sleep(1); log(_.resign(me), s"resign($me)", _.resign(me)) }
    // }
  }

  var faulty = false

  /** Do a single test. */
  def doTest = {
    val barrier = new ResignableBarrier[Int](faulty)
    val spec = new ResignableBarrierSpec()
    val tester =  LinearizabilityTester[ResignableBarrierSpec,RB](
      spec, barrier, numThreads, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 5000; var i = 0
    var profiling = false; var interval = 50; var timing = false; 
    while(i < args.length) args(i) match{
      case "-p" => numThreads = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1
      case "--syncProb" => syncProb = args(i+1).toDouble; i += 2

      case "--faulty" => faulty = true; i += 1

      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }

}


