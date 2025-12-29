package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationObject.{AtomicBroadcastT, AtomicBroadcast}

/** Two-step testing for an atomic broadcast object. */
object AtomicBroadcastTwoStepTester extends Tester{
  type AB = AtomicBroadcastT[Int]

  /** The number of receivers involved in each synchronisation. */
  var n = 3

  /** Specification object for a barrier for `n` receivers.  Each receive is
    * linearised by receive1 and receive2; each send is linearised by send.  
    * @param count the number of receive1 invocations that have occurred in the 
    * current synchronisation.
    * @param returns optionally the values to be returned by each receiver. */
  class AtomicBroadcastSpec(val count: Int, val returns: Array[Option[Int]]){
    /** The first half of a receive: increment count. */
    def receive1(id: Int): (Unit, AtomicBroadcastSpec) = {
      assert(returns(id) == None && count < n, returns.mkString(", ")+"\n"+count)
      ((), new AtomicBroadcastSpec(count+1, returns))
    }

    /** The second half of a receive: pick up the result. */
    def receive2(id: Int): (Int, AtomicBroadcastSpec) = {
      require(returns(id).isInstanceOf[Some[Int]])
      val newReturns = returns.clone; newReturns(id) = None
      (returns(id).get, new AtomicBroadcastSpec(count, newReturns))
    }

    /** The send, done after all the receive1 invocations.  Store the results for
      * the receivers. */ 
    def send(x: Int): (Unit, AtomicBroadcastSpec) = {
      require(count == n); assert(returns.forall(_ == None))
      ((), new AtomicBroadcastSpec(0, Array.fill(n)(Some(x))))
    }

    override def hashCode = {
      var h = count
      for(i <- 0 until n) h = h*3 + returns(i).hashCode
      h
    }

    override def equals(that: Any) = that match{
      case abs: AtomicBroadcastSpec => 
        abs.count == count && abs.returns.sameElements(returns)
    }
  }

  /** A worker in the testing system. */
  def worker(me: Int, log: LinearizabilityLog[AtomicBroadcastSpec,AB]) = {
    for(i <- 0 until iters){
      if(me == n){ 
        val x = Random.nextInt(100); log(_.send(x), s"send($x)", _.send(x)) 
      }
      else{    
        var y = -1
        log((ab: AB) => {y = ab.receive()}, s"$me: receive1", _.receive1(me))
        log(ab => y, s"$me: receive2", _.receive2(me))
      }
    }
  }

  /** Perform a single test. */
  def doTest = {
    val ab = new AtomicBroadcast[Int](n)
    val spec = new AtomicBroadcastSpec(0, Array.fill(n)(None))
    val tester = LinearizabilityTester[AtomicBroadcastSpec, AB](
      spec, ab, n+1, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    var reps = 1000; var i = 0
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
    }

    runTests(reps, false)
  }
  

}
