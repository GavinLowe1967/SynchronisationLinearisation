package synchronisationTesting

/** A tester for synchronisation linearisation in the case of binary
  * synchronisations with a stateless synchronisation object.
  * @tparam Op the type representing operations on the synchronisation object.
  * @tparam S the type of the specification object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to.
  * @param a description of the results that should be given by a particular
  * pair of operations synchronising and the corresponding state of the
  * specification object. 
  * @param spec0 the initial state of the specification object. */
class BinaryStatefulTester[Op,S](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  specMatching: S => PartialFunction[(Op,Op), (S,(Any,Any))],
  spec0: S)
    extends Tester(worker, p){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** The events in the log.  Set by apply. */
  private var events: Array[Event] = null

  /** The number of events in the log.  Set by apply. */
  private var length = -1

  /** The number of invocations in the log.  Set by apply. */
  private var invocs = -1

  /** Try to synchronise the invocations corresponding to ce1 and ce2, given
    * state spec of the specification object.  If successful, return the
    * resulting state of the specification.  */
  protected def trySync(spec: S, ce1: CallEvent[Op,_], ce2: CallEvent[Op,_])
      : Option[S] = {
    // Each is called before the other returns
    if(ce1.index < ce2.ret.index && ce2.index < ce1.ret.index &&
      specMatching(spec).isDefinedAt(ce1.op, ce2.op)){
      try{
        val (spec1, (ret1,ret2)) = specMatching(spec)(ce1.op, ce2.op)
        if((ce1.ret.result, ce2.ret.result) == (ret1, ret2)) Some(spec1) 
        else None
      }
      catch{ case _: IllegalArgumentException => None }
    }
    else None
  }

  /** Array representing invocations that have been matched so far.  A negative
    * value represents an unmatched invocation. */
  type Matching = Array[Int]

  /** The matching found so far that is maximal, in the sense that the largest
    * prefix of invocations are paired. */
  private var maxMatching: Matching = null

  /** The length of the initial matched invocations in maxMatching. */
  private var maxMatchingSize = 0

  /** The linearisation indices (giving the order of synchronisations)
    * corresponding to maxMatching. */
  private var maxLinIndices: Array[Int] = null

  /** Show the history, together with the matching represented by matching and
    * the linearisation order given by linIndices. */
  private def showMatching(matching: Matching, linIndices: Array[Int]) = 
    println(HistoryLog.showHistoryWith(events, e => e match{
      case _: CallEvent[Op,_] => ""
      case _: ReturnEvent[Op,_] => 
        val syncIndex = matching(e.opIndex)
        if(syncIndex >= 0) 
          s":  matched with $syncIndex; linearisation index "+
            linIndices(e.opIndex)
        else ":  unmatched"
    }))

  /** Insert x into xs so as to keep the list sorted by index fields.  Pre: xs
    * is so sorted. */
  private def insert[E <: Event](x: E, xs: List[E]): List[E] = 
    if(xs.isEmpty) List(x)
    else{
      val y = xs.head; assert(x.index != y.index)
      if(x.index < y.index) x :: xs else y :: insert(x, xs.tail)
    }

  /** A configuration in the search. 
    * @param the index in the history reached so far. 
    * @param spec the state of the specification object. 
    * @param canReturn invocations that have been linearised so can return, 
    * sorted by index.
    * @param pending invocations that have been called but not yet, sorted by 
    * index.
    * @param matching array showing which invocations have been matched so far.
    * linearised.  
    * @param matchingSize the length of the longest prefix of the history that 
    * has been matched. 
    * @param linIndices the linearisation indices, giving the order in which
    * invocations synchronised.
    * @param nextLinIndex the number of synchronisations so far. */
  private class Config(
    val index: Int, val spec: S, 
    val canReturn: List[ReturnEvent[Op,Any]], 
    val pending: List[CallEvent[Op,Any]],
    val matching: Array[Int], val matchingSize: Int,
    val linIndices: Array[Int], val nextLinIndex: Int
  ){
    def nexts: List[Config] = {
      var result = List[Config]()
      if(index < length) events(index) match{
        case ce: CallEvent[Op,Any] @unchecked =>
          // add to pending
          result ::= new Config(index+1, spec, canReturn, insert(ce,pending), 
            matching, matchingSize, linIndices, nextLinIndex)
        case re: ReturnEvent[Op,Any] @unchecked =>
          // maybe allow this event to return
          if(canReturn.contains(re)){
            val newCanReturn = canReturn.filter(_ != re)
            result ::= new Config(index+1, spec, newCanReturn, pending,
              matching, matchingSize, linIndices, nextLinIndex)
          }
      }
      // Consider linearisations
      for(e1 <- pending; e2 <- pending; if e1 != e2) trySync(spec, e1, e2) match{
        case Some(spec1) =>
          val newPending = pending.filter(e => e != e1 && e != e2)
          val newCanReturn = insert(e1.ret, insert(e2.ret, canReturn))
          val index1 = e1.opIndex; val index2 = e2.opIndex
          val newMatching = matching.clone; newMatching(index1) = index2
          newMatching(index2) = index1
          val newLinIndices = linIndices.clone
          newLinIndices(index1) = nextLinIndex
          newLinIndices(index2) = nextLinIndex
          // Is this a new maximum matching?
          var newMatchingSize = matchingSize
          while(newMatchingSize < invocs && newMatching(newMatchingSize) >= 0)
            newMatchingSize += 1
          if(newMatchingSize > maxMatchingSize){
            maxMatching = newMatching; maxMatchingSize = newMatchingSize
            maxLinIndices = newLinIndices
          }
          // println(s"Pairing $e1 and $e2")
          result ::= new Config(index, spec1, newCanReturn, newPending, 
            newMatching, newMatchingSize, newLinIndices, nextLinIndex+1)
        case None => {}
      }
    result
    }

    def done = index == length && canReturn.isEmpty && pending.isEmpty

    /** Equality test.  Note: this ignores matching and matchingSize:  */
    override def equals(that: Any) = that match{
      case conf: Config => 
        conf.index == index && conf.spec == spec &&
        conf.canReturn == canReturn && conf.pending == pending
    }

    /** Hash code.  Based on the same principle as equals. */
    override def hashCode = 
      ((index*41+spec.hashCode)*41 + canReturn.hashCode)*41 + pending.hashCode 
  } // end of Config
  // IMPROVE: use a partial order reduction


  /** Perform a DFS. */
  private def search(): Boolean = {
    maxMatching = Array.fill(invocs)(-1); maxLinIndices = Array.fill(invocs)(-1)
    val config0 = new Config(0, spec0, List(), List(), 
      Array.fill(invocs)(-1), 0, Array.fill(invocs)(-1), 0)
    val stack = new scala.collection.mutable.Stack[Config]; stack.push(config0)
    val seen = new scala.collection.mutable.HashSet[Config]; seen += config0

    while(stack.nonEmpty){
      val config = stack.pop
      for(next <- config.nexts; if seen.add(next)){
        if(next.done){ 
          if(false) showMatching(next.matching, next.linIndices)
          assert(next.matchingSize == invocs && next.nextLinIndex == invocs/2)
          return true 
        }
        else stack.push(next)
      }
    }

    false
  }

  /** Main function. */
  def apply(): Boolean = {
    events = getLog() // From Tester.class
    length = events.length; invocs = length / 2
    val calls = getCalls(events) // From Tester; adds opIndex fields 
    if(search()) true
    else{ 
      println("Error found.")
      showMatching(maxMatching, maxLinIndices); false 
    }
  }
}
