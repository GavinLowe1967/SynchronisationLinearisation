package synchronisationTesting

import HistoryLog.{Event,CallEvent,ReturnEvent}

/** Base class of all testers.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to. */
class Tester[Op](worker: (Int, HistoryLog[Op]) => Unit, p: Int){
  /** Run a system of `p` `worker` threads.  Return the contents of the log. */
  protected def getLog(): Array[Event] = {
    val log = new HistoryLog[Op](p)
    ThreadUtil.runIndexedSystem(p, i => worker(i, log))
    log.get
  }

  /** Get an array, enumerating the CallEvents.  Each CallEvent and
    * corresponding ReturnEvent is labelled with its index. */
  protected def getCalls(events: Array[Event]):  Array[CallEvent[Op,_]] = {
    require(events.length%4 == 0); val numInvs = events.length/2
    // Find call  events.
    val calls = new Array[CallEvent[Op,_]](numInvs); var i = 0
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,_] @unchecked => 
        calls(i) = ce; ce.opIndex = i; ce.ret.opIndex = i; i += 1
      case _ => {}
    }
    calls
  }

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
    * @param length the length of the corresponding history.
    * @param canReturn invocations that have been linearised so can return, 
    * sorted by index.
    * @param pending invocations that have been called but not yet linearised,
    * sorted by index. 
    */
  abstract class Config[Op,S](val index: Int, val spec: S, length: Int,
    val canReturn: List[ReturnEvent[Op,Any]], 
    val pending: List[CallEvent[Op,Any]]
  ){
    /** Next configurations in the search graph.  Defined in concrete classes. */
    def nexts: List[Config[Op,S]]

    /** Does this represent a complete linearisation? */
    def done = index == length && canReturn.isEmpty && pending.isEmpty

    /** Equality test. */
    override def equals(that: Any) = that match{
      case conf: Config[Op,S] => 
        conf.index == index && conf.spec == spec &&
        conf.canReturn == canReturn && conf.pending == pending
    }

    /** Hash code.   */
    override def hashCode = {
      @inline def f(x: Int, y: Int): Int = (x<<5) + (x<<3) + x + y // x*41+y
      f(f(f(index, spec.hashCode), mkHash(canReturn)), mkHash(pending))
    }
      //((index*41+spec.hashCode)*41 + canReturn.hashCode)*41 + pending.hashCode 
  }

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
    val stack = new scala.collection.mutable.Stack[Config[Op,S]] // C
    stack.push(config0)
    val seen = new scala.collection.mutable.HashSet[Config[Op,S]]
    seen += config0

    while(stack.nonEmpty){
      Profiler.count("DFS step")
      val config = stack.pop; var nexts = config.nexts
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
