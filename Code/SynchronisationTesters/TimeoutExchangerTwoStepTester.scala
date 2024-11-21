package synchronisationTester

import scala.util.Random
import ox.scl.{LinearizabilityTester,LinearizabilityLog}
import synchronisationObject.{
  TimeoutExchangerT, TimeoutExchanger, FaultyTimeoutExchanger, 
  FaultyTimeoutExchanger2}
import synchronisationTesting.TwoStepLinSpec.{ThreadID,AState,Zero,One}

object TimeoutExchangerTwoStepTester extends Tester{
  /** The type of objects tested. */
  type TE = TimeoutExchangerT[Int]

  /* Note: we have to turn off the optimisation in the linearisation tester that
   * tries to detect if the same invocation (i.e. with the same label) has
   * previously been performed from the same state, at least for the
   * "exchange" label.  This is because one instance of that operation might
   * have failed, and the other succeeded.  We do this by ensuring every
   * invocation has a distinct label. */

  /** Specification object. */
  class ExchangerSpec(val state: AState[Int], val returns: Array[Option[Int]]){
    /** Constructor for initial state. */
    def this(n: Int) = this(Zero(), Array.fill(n)(None))

    /** The result of t successfully exchanging. */
    private def getFromReturns(t: ThreadID) = {
      assert(returns(t) != None, s"Error with thread $t")
      val Some(x) = returns(t)
      val newReturns = returns.clone; newReturns(t) = None
      (Some(x), new ExchangerSpec(state, newReturns))
    }

    /** The first part of the synchronisation. */
    def sync1(t: ThreadID, x: Int): (Unit, ExchangerSpec) = state match{
      case Zero() => ((), new ExchangerSpec(One(t,x), returns))
      case One(t1,y) =>
        assert(t != t1)
        val newReturns = returns.clone
        newReturns(t) = Some(y); newReturns(t1) = Some(x)
        ((), new ExchangerSpec(Zero(), newReturns))
    }

    /** The second part of the synchronisation. */
    def sync2(t: ThreadID): (Option[Int], ExchangerSpec) = state match{
      case Zero() => getFromReturns(t)
      case One(t1,y) => require(t1 != t); getFromReturns(t)
    }

    /** An attempt to exchange that failed. */
    def syncFail(t: ThreadID, x: Int): (Unit, ExchangerSpec) = ((), this)

    override def hashCode = {
      var h = state.hashCode
      for(x <- returns) h = h*157+x.hashCode
      h
    }

    override def equals(that: Any) =  that match{
      case es: ExchangerSpec =>
        if(es.state == state){
          var i = 0
          while(i < returns.length && returns(i) == es.returns(i)) i += 1
          i == returns.length
        }
        else false
    }
  } // end of ExchangerSpec

  /** The maximum value sent. */
  var MaxVal = 100

  /** A worker in the testing system. */
  def worker(me: Int, log: LinearizabilityLog[ExchangerSpec,TE]) = {
    val random = new Random()
    for(i <- 0 until iters){
      Thread.sleep(random.nextInt(3))
      val x = random.nextInt(MaxVal) // value sent
      var y = null.asInstanceOf[Option[Int]] // value received
      log(te => y = te.exchange(x), s"exchange($x) ($i, $me)",
        spec => if(y == None) spec.syncFail(me,x) else spec.sync1(me, x) )
      if(y != None) log(_ => y, s"exchange2($x)", _.sync2(me))
    }
  }

  var faulty = false; var faulty2 = false 

  /** Run a single test. */ 
  def doTest: Boolean = {
    val exchanger: TimeoutExchangerT[Int] =
      if(faulty) new FaultyTimeoutExchanger[Int](1) 
      else if(faulty2) new FaultyTimeoutExchanger2[Int](1) 
      else new TimeoutExchanger[Int](1)
    val spec = new ExchangerSpec(p)
    val tester = LinearizabilityTester[ExchangerSpec,TE](
      spec, exchanger, p, worker _)
    tester() > 0
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var reps = 1000; var i = 0; var interval = 50; var profiling = false
    var timing = false
    while(i < args.length) args(i) match{
      case "-p" => p = args(i+1).toInt; i += 2
      // case "--verbose" => verbose = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--MaxVal" => MaxVal = args(i+1).toInt; i += 2
      case "--timing" => timing = true; i += 1

      case "--faulty" => faulty = true; i += 1
      case "--faulty2" => faulty2 = true; i += 1

      // case "--progressCheck" => // false positives with 100
      //   progressCheck = true; timeout = args(i+1).toInt; i += 2

      // case "--profile" => profiling = true; interval = args(i+1).toInt; i += 2
      // case "--countProfile" => countProfile = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    runTests(reps, timing)
  }
}

/*
The straightforward approach doesn't work.

0: 0 invokes exchange(48)
1: 0 returns ()
2: 3 invokes exchange(60)
3: 1 invokes exchange(88)
4: 1 returns ()
5: 3 returns ()
6: 2 invokes exchange(72)
7: 0 invokes exchange2(48)
8: 0 returns None
-- Previous event not linearized
-- Allowed return values: Some(88), Some(60)
9: 1 invokes exchange2(88)
10: 1 returns Some(60)
11: 2 returns ()
12: 2 invokes exchange2(72)
13: 2 returns None
14: 3 invokes exchange2(60)
15: 3 returns Some(88)

0 failed to exchange.  Then 3 and 1 exchanged.  But the above approach would
have 0 exchanging with either 3 or 1.

Idea: if worker fails to exchange, it calls a different op that requires the
state to be Zero and its return value None.
 */
