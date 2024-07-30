package synchronisationTesting

import HistoryLog.{Event,CallEvent,ReturnEvent}
import scala.collection.mutable.ArrayBuffer

/** Base class of all testers.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to. 
  * @param p the number of threads to run. */
abstract class Tester[Op](worker: (Int, HistoryLog[Op]) => Unit, p: Int){

  /** The CallEvents used here. */
  type CallEvent1 = CallEvent[Op,_]

  /** Run a system of `p` `worker` threads.  Return the contents of the log. */
  protected def getLog(): Array[Event] = {
    val log = new HistoryLog[Op](p)
    ThreadUtil.runIndexedSystem(p, i => worker(i, log))
    log.get
  }

  /** The run time (in nanos) that is taken to constitute a deadlock. */
  val Duration = ThreadUtil.Duration

  /** Run a system of `p` `worker` threads.  Return the contents of the log
    * together with a boolean that indicates if the run took longer than
    * duration milliseconds (indicating a possible deadlock).  */
  protected def getLogDetectDeadlock(duration: Int = Duration)
      : (Boolean, Array[Event]) = {
    val log = new HistoryLog[Op](p)
    val deadlocked = 
      ThreadUtil.runIndexedSystemDetectDeadlock(p, i => worker(i, log), duration)
    (deadlocked, log.get)
  }

  /** Get two arrays: (1) enumerating the CallEvents for which there is a
    * corresponding return; (2) enumerating the CallEvents for which there is
    * not a corresponding return.  Each CallEvent and corresponding
    * ReturnEvent is labelled with its index. */
  protected def getCalls(events: Array[Event])
      : (Array[CallEvent[Op,_]], Array[CallEvent[Op,_]]) = {
    // Find call events for which there is a matching return
    val calls = new ArrayBuffer[CallEvent[Op,Any]]; var i = 0
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,Any] @unchecked if ce.ret != null  => 
        calls += ce; ce.opIndex = i; ce.ret.opIndex = i; i += 1
      case _ => {}
    }
    // Find pending calls
    val numPending = events.length-2*i
    val pending = new Array[CallEvent[Op,_]](numPending); var k = 0
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,Any] @unchecked if ce.ret == null =>
        if(k == numPending){ // This shouldn't happen  
          println(s"k = $k, calls = ${calls.length}, numPending = $numPending")
          println(HistoryLog.showHistory(events))
          sys.exit()
        }
        pending(k) = ce; ce.opIndex = i; i += 1; k += 1
      case _ => {}
    }
    assert(k == numPending, 
      s"i = $i; k = $k; length = "+events.length)
    (calls.toArray, pending)
  }

  /** A representation of a synchronisation: the list of CallEvents for the
    * invocations that synchronise. */
  type SyncEs = List[CallEvent[Op,_]]

  /** All lists of potential synchronisations from pending.  All lists of length
    * up to arities.max of pending CallEvents. */
  protected 
  def allPotentialPendingSyncs(pending: Array[CallEvent[Op,_]], maxArity: Int)
      : Array[List[SyncEs]] = {
    val result = new Array[List[SyncEs]](maxArity+1)
    result(0) = List(List[CallEvent[Op,_]]())
    for(arity <- 1 to maxArity){
      result(arity) = List(); var tails = result(arity-1)
      while(tails.nonEmpty){
        val tail = tails.head; tails = tails.tail; var i = 0
        // Try extending tail with another pending call
        while(i < pending.length){
          val e = pending(i); i += 1
          if(!tail.contains(e)) result(arity) ::= e::tail
        }
      }
    }
    result
  }
  
  /** Run the tester.  Return true if successful.
    * @param delay if positive, run a progress check, with a timeout time of 
    * `delay` ms. */
  def apply(delay: Int = -1): Boolean

}

// ==================================================================

import ox.gavin.profiling.Profiler

object Tester{
  /** Insert x into xs so as to keep the list sorted by index fields.  Pre: xs
    * is so sorted. */
  def insert[E <: Event](x: E, xs: List[E]): List[E] = 
    if(xs.isEmpty) List(x)
    else{
      val y = xs.head; assert(x.index != y.index)
      if(x.index < y.index) x :: xs else y :: insert(x, xs.tail)
    }

  /** A class representing a configuration in a search.
    * 
    * @tparam Op the type of operations.
    * @tparam S the type of the specification.
    * 
    * @param the index in the history reached so far. 
    * @param spec the state of the specification object. 
    * @param canReturn invocations that have been linearised so can return, 
    * sorted by index.
    * @param pending invocations that have been called but not yet linearised,
    * sorted by index. 
    */
  abstract class Config[Op,S](val index: Int, val spec: S, // length: Int,
    val canReturn: List[ReturnEvent[Op,_]], 
    val pending: List[CallEvent[Op,_]]
  ){
    /** Next configurations in the search graph.  Defined in concrete classes. */
    def nexts: List[Config[Op,S]]

    /** Does this represent a complete linearisation? */
    def done: Boolean 
    // = index == length && canReturn.isEmpty && pending.isEmpty

    /** Equality test. */
    override def equals(that: Any) = that match{
      case conf: Config[Op,S] @unchecked => 
        conf.index == index && conf.spec == spec &&
        conf.canReturn == canReturn && conf.pending == pending
    } 

    /** Hash code.   */
    override def hashCode = {
      @inline def f(x: Int, y: Int): Int = (x<<5) + (x<<3) + x + y // x*41+y
      f(f(f(index, spec.hashCode), mkHash(canReturn)), mkHash(pending))
    }
  } // end of Config

  @inline private def mkHash(es: List[Event]) = {
    var es1 = es; var h = 0
    while(es1.nonEmpty){
      h = ((h<<5)+(h<<3)+h) ^ es1.head.hashCode; es1 = es1.tail
    }
    h
  }

  /** Perform a depth-first search, starting from config0.  Apply atEnd to the
    * final configuration, if successful. */
  def search[Op, S, C <: Config[Op,S]](config0: C, atEnd: C => Unit): Boolean = {
    if(config0.done){ /* println("Initial configuration done");*/ return true }
    val stack = new scala.collection.mutable.Stack[Config[Op,S]] // C
    stack.push(config0)
    val seen = new scala.collection.mutable.HashSet[Config[Op,S]]
    seen += config0

    while(stack.nonEmpty){
      // Profiler.count("DFS step")
      val config = stack.pop(); var nexts = config.nexts
      while(nexts.nonEmpty){
        val next = nexts.head; nexts = nexts.tail
        if(seen.add(next)){
          if(next.done){ atEnd(next.asInstanceOf[C]); return true }
          // IMPROVE: the above is ugly.  Can we specify that nexts returns
          // Configs of the same concrete type?
          else stack.push(next)
        }
      }
    }

    false
  }
}
