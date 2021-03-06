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

  /** The number of returns in the log.  Set by apply. */
  private var numReturns = -1

  /** Try to synchronise the invocations corresponding to ce1 and ce2, given
    * state spec of the specification object.  If successful, return the
    * resulting state of the specification.  */
  protected def trySync(spec: S, ce1: CallEvent[Op,_], ce2: CallEvent[Op,_])
      : Option[S] = {
    // Each is called before the other returns
    if(ce1.ret != null && ce2.ret != null && 
        ce1.index < ce2.ret.index && ce2.index < ce1.ret.index &&
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

  /** Could the two pending operations ce1 and ce2 synchronise, given
    * specification spec? */
  private def canSyncPending(spec: S, ce1: CallEvent[Op,_], ce2: CallEvent[Op,_])
      : Boolean =
    if(ce1 != ce2 && specMatching(spec).isDefinedAt(ce1.op, ce2.op)){
      try{ specMatching(spec)(ce1.op, ce2.op); true }
      catch{ case _: IllegalArgumentException => false }
    }
    else false

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

  /** The state of the spec corresponding to maMatching if it is a total
    * matching of returned invocations. */
  private var totalMatchingSpec: S = null.asInstanceOf[S]

  /** Show the history, together with the matching represented by matching and
    * the linearisation order given by linIndices. */
  private def showMatching(matching: Matching, linIndices: Array[Int]) = {
    // Annotation on e
    def annotate(e: Event) = e match{
      case ce: CallEvent[Op,_] @unchecked => 
        if(ce.ret == null) ":  no return" else ""
      case _: ReturnEvent[Op,_] @unchecked => 
        val syncIndex = matching(e.opIndex)
        if(syncIndex >= 0) 
          s":  matched with $syncIndex; linearisation index "+
            linIndices(e.opIndex)
        else ":  unmatched"
    }
    println(HistoryLog.showHistoryWith(events, annotate))
  }

  /** A configuration in the search. 
    * @param the index in the history reached so far. 
    * @param spec the state of the specification object. 
    * @param canReturn invocations that have been linearised so can return, 
    * sorted by index.
    * @param pending invocations that have been called but not yet linearised,
    * sorted by index.
    * @param matching array showing which invocations have been matched so far.
    * matching(ix1) = ix2 indicates that the invocations with indices ix1 and
    * ix2 synchronised.
    * @param matchingSize the length of the longest prefix of the calls of the 
    * history that has been matched.
    * @param linIndices the linearisation indices, giving the order in which
    * invocations synchronised.  If linIndices(ix) >= 0, it gives the
    * linearisation index for the invocation with index ix.
    * @param nextLinIndex the number of synchronisations so far. */
  private class Config(
    index: Int, spec: S, 
    canReturn: List[ReturnEvent[Op,Any]], pending: List[CallEvent[Op,Any]],
    val matching: Array[Int], val matchingSize: Int,
    val linIndices: Array[Int], val nextLinIndex: Int) 
      extends Tester.Config(index, spec, canReturn, pending){
    /** Next configurations in the search graph. */
    def nexts: List[Config] = {
      var result = List[Config]()
      if(index < length) events(index) match{
        case ce: CallEvent[Op,Any] @unchecked =>
          // add to pending
          val newPending =  Tester.insert(ce,pending)
          result ::= new Config(index+1, spec, canReturn, newPending, 
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
          val newCanReturn = 
            Tester.insert(e1.ret, Tester.insert(e2.ret, canReturn))
          val index1 = e1.opIndex; val index2 = e2.opIndex
          val newMatching = matching.clone; newMatching(index1) = index2
          newMatching(index2) = index1
          val newLinIndices = linIndices.clone
          newLinIndices(index1) = nextLinIndex
          newLinIndices(index2) = nextLinIndex
          // Set newMatchingSize to be the size of the longestprefix that is
          // matched
          var newMatchingSize = matchingSize
          while(newMatchingSize < invocs && newMatching(newMatchingSize) >= 0)
            newMatchingSize += 1
          // Is this a new maximum matching?
          if(newMatchingSize > maxMatchingSize){
            maxMatching = newMatching; maxMatchingSize = newMatchingSize
            maxLinIndices = newLinIndices
            if(maxMatchingSize == numReturns) totalMatchingSpec = spec1
          }
          // println(s"Pairing $e1 and $e2")
          result ::= new Config(index, spec1, newCanReturn, newPending, 
            newMatching, newMatchingSize, newLinIndices, nextLinIndex+1)
        case None => {}
      }
    result
    }

    def done: Boolean = 
      if(index == length && canReturn.isEmpty){
        for(e <- pending) assert(e.ret == null)
        // Check that no calls in pending could synchronise here
        for(e1 <- pending; e2 <- pending)
          if(canSyncPending(spec, e1, e2)) return false
        true 
      }
      else false
  } // end of Config
  // IMPROVE: use a partial order reduction

  /** Perform a DFS. */
  private def search(): Boolean = {
    length = events.length    // if(length == 0) println("Empty log")
    val (_, pending) = getCalls(events) // From Tester; adds opIndex fields 
    invocs = (length+pending.length)/2; numReturns = (length-pending.length)/2
    maxMatching = Array.fill(invocs)(-1); maxLinIndices = Array.fill(invocs)(-1)
    // Starting configuration
    val config0 = new Config(0, spec0, List(), List(), 
      Array.fill(invocs)(-1), 0, Array.fill(invocs)(-1), 0)
    if(numReturns == 0) totalMatchingSpec = spec0
    def atEnd(c: Config) = {
      if(false) showMatching(c.matching, c.linIndices)
      assert(c.matchingSize == numReturns && 2*c.nextLinIndex == numReturns,
        s"matchingSize = ${c.matchingSize}; invocs = $invocs; "+
          s"nextLinIndex = ${c.nextLinIndex}; numReturns = $numReturns")
    }

    // Perform the search
    if(Tester.search[Op,S,Config](config0, atEnd))true
    else{ // Give debugging information
      println("Error found.")
      showMatching(maxMatching, maxLinIndices)
      if(maxMatchingSize == numReturns){
        assert(totalMatchingSpec != null)
        println(s"Final specification state: $totalMatchingSpec")
        // See if any of the pending invocations could have synchronised
        var done = false
        for(e1 <- pending; e2 <- pending; if !done)
          if(canSyncPending(totalMatchingSpec, e1, e2)){
            println(s"${e1.opIndex} and ${e2.opIndex} could synchronise.")
            done = true
          }
      }
      false
    }
  }

  /** Main function. */
  def apply(delay: Int = -1): Boolean = {
    if(delay > 0){
      val (deadlock, events0) = getLogDetectDeadlock(delay) // From Tester
      events = events0; search()
    }
    else{
      events = getLog() // From Tester
      search()
    }
  }
}
