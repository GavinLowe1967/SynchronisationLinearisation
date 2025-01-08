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
  * @param spec0 the initial state of the specification object.
  * @param doASAP should the ASAP partial-order reduction be used? */
class BinaryStatefulTester[Op,S](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  specMatching: S => PartialFunction[(Op,Op), (S,(Any,Any))],
  spec0: S,
  doASAP: Boolean = false)
    extends Tester(worker, p){
  // println(doASAP)

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** The events in the log.  Set by apply. */
  private var events: Array[Event] = null

  /** The number of events in the log.  Set by search. */
  private var length = -1

  /** The number of invocations in the log.  Set by search. */
  private var invocs = -1

  /** The number of returns in the log.  Set by search. */
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
        if(ce1.ret.result == ret1 && ce2.ret.result == ret2) Some(spec1) 
        else None
      }
      catch{ case _: IllegalArgumentException => None } // precondition fails
    }
    else None
  }

  /** Test if the invocations corresponding to ce1 and ce2 can synchronise, that
    * way round, given state `spec` of the specification.  Optionally return
    * the resulting state of the specification.  Pre: both return.  */
  private def canSync(spec: S, ce1: CallEvent1, ce2: CallEvent1): Option[S] = {
    if(specMatching(spec).isDefinedAt(ce1.op, ce2.op)){
      try{
        val (spec1, (ret1,ret2)) = specMatching(spec)(ce1.op, ce2.op)
        if(ce1.ret.result == ret1 && ce2.ret.result == ret2) Some(spec1) 
        else None
      }
      catch{ case _: IllegalArgumentException => None } // precondition fails
    }
    else None
  }

  // private def canSync(spec: S, ce1: CallEvent1, ce2: CallEvent1): Option[S] = 
  //   canSync0(spec, ce1, ce2) orElse canSync0(spec, ce2, ce1)
// FIXME: what if both are possible?

  /** Could the two pending operations ce1 and ce2 synchronise, given
    * specification spec? */
  private def canSyncPending(spec: S, ce1: CallEvent[Op,_], ce2: CallEvent[Op,_])
      : Boolean =
    if(ce1 != ce2 && specMatching(spec).isDefinedAt(ce1.op, ce2.op)){
      // check precondition
      try{ specMatching(spec)(ce1.op, ce2.op); true }
      catch{ case _: IllegalArgumentException => false }
    }
    else false

  /** All possible synchronisations between invocations from `calls` given state
    * `spec` of the specification.  For each, return the resulting state of
    * the specification, and the two events. */
  private def allSyncs(spec: S, calls: List[CallEvent1]): 
      List[(S, CallEvent1, CallEvent1)] = {
    var result = List[(S, CallEvent1, CallEvent1)]()
    for(e1 <- calls; if e1.ret != null; 
        e2 <- calls; if e1 != e2 && e2.ret != null){
      // Test if e1 and e2 can synchronise, either way round
      canSync(spec, e1, e2) match{
        case Some(spec1) => result ::= (spec1, e1, e2); case None => {}
      }
      // canSync(spec, e2, e1) match{
      //   case Some(spec1) => result ::= (spec1, e2, e1); case None => {}
      //}
    }
    result
  }

  /** All possible synchronisations involving e and another invocation from
    * `calls` given state `spec` of the specification.  For each, return the
    * resulting state of the specification and the other event. */
  private def allSyncsWith(spec: S, calls: List[CallEvent1], e: CallEvent1)
      : List[(S, CallEvent1)] = {
    require(doASAP && e.ret != null)
    var result = List[(S, CallEvent1)]()
    for(e1 <- calls; if e1 != e && e1.ret != null){
      // Test if e and e1 can sync, either way round.  Note that the two cases
      // might give different values for spec1.
      canSync(spec, e, e1) match{
        case Some(spec1) => result ::= (spec1, e1); case None => {}
      }
      canSync(spec, e1, e) match{
        case Some(spec1) => result ::= (spec1, e1); case None => {}
      }
    }
    result
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
    canReturn: Array[ReturnEvent1], pending: List[CallEvent1],
    val matching: Array[Int], val matchingSize: Int,
    val linIndices: Array[Int], val nextLinIndex: Int) 
      extends Tester.Config[Op,S](index, spec, canReturn, pending){

    /** The next configuration from this after the synchronisation corresponding
      * to events ce1 and ce2 producing specification spec1. */
    private def mkNext(spec1: S, e1: CallEvent1, e2: CallEvent1): Config = {
      val newPending = pending.filter(e => e != e1 && e != e2)
      val r1 = e1.ret; val r2 = e2.ret
      val rs = 
        if(r1.index < r2.index) Array[ReturnEvent1](r1,r2)
        else Array[ReturnEvent1](r2,r1)
      val newCanReturn = merge(canReturn, rs)
        //Tester.insert(e1.ret, Tester.insert(e2.ret, canReturn)).toArray
      val index1 = e1.opIndex; val index2 = e2.opIndex
      val newMatching = matching.clone
      newMatching(index1) = index2; newMatching(index2) = index1
      val newLinIndices = linIndices.clone
      newLinIndices(index1) = nextLinIndex; newLinIndices(index2) = nextLinIndex
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
      new Config(index, spec1, newCanReturn, newPending,
        newMatching, newMatchingSize, newLinIndices, nextLinIndex+1)
    }

// IMPROVE: in Tester.Config? 
    /** Find all configurations reachable by 0 or more synchronisations from
      * this, and add to buffer. */
    private def iterateLinearisations(buffer: List[Config]): List[Config] = {
      //println(s"iterateLinearisations($pending)")
      var buff = this::buffer
      for((spec1, e1, e2) <- allSyncs(spec,pending)){
        val config1 = mkNext(spec1,e1,e2)
          //println(s"synchronising $e1 and $e2")
        buff = config1.iterateLinearisations(buff)
      }
      buff
    }

    /** Find all configurations reachable by performing a linearisation involving
      * ce, and then zero or more additional linearisations.  Add all such
      * configurations to buffer. */
    private def iterateLinearisationsWith(ce: CallEvent1, buffer: List[Config])
        : List[Config] = 
      if(ce.ret == null) buffer
      else{
        //println(s"iterateLinearisationsWith($ce, $pending)")
        var buff = buffer
        for((spec1,ce1) <- allSyncsWith(spec,pending,ce)){
          val config1 = mkNext(spec1,ce,ce1)
          //println(s"synchronising $ce and $ce1")
          buff = config1.iterateLinearisations(buff)
        }
        buff
      }

    /** Next configurations in the search graph. */
    def nexts: List[Config] = {
      var result = List[Config]()
      if(index < length) events(index) match{
        case ce: CallEvent[Op,Any] @unchecked =>
          // add to pending
          val newPending =  Tester.insert(ce,pending)
          val config1 = new Config(index+1, spec, canReturn, newPending, 
            matching, matchingSize, linIndices, nextLinIndex)
          result ::= config1
          // Consider linearisations, starting with one involving ce.
          if(doASAP) result = config1.iterateLinearisationsWith(ce, result)

        case re: ReturnEvent[Op,Any] @unchecked =>
          // maybe allow this event to return
          maybeRemoveReturn(re) match{
            case Some(newCanReturn) =>
          // if(canReturn.contains(re)){
          //   val newCanReturn = canReturn.filter(_ != re)
              result ::= new Config(index+1, spec, newCanReturn, pending,
                matching, matchingSize, linIndices, nextLinIndex)
            case None => {}
          }
      }
      // Consider linearisations
      if(!doASAP) for(e1 <- pending; e2 <- pending; if e1 != e2) 
        trySync(spec, e1, e2) match{
          case Some(spec1) => result ::= mkNext(spec1, e1, e2)
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
  } 
  // end of Config

  // IMPROVE: use a partial order reduction

  /** Perform a DFS. */
  private def search(): Boolean = {
    length = events.length    // if(length == 0) println("Empty log")
    val (_, pending) = getCalls(events) // From Tester; adds opIndex fields 
    invocs = (length+pending.length)/2; numReturns = (length-pending.length)/2
    maxMatching = Array.fill(invocs)(-1); maxLinIndices = Array.fill(invocs)(-1)
    // Starting configuration
    val config0 = new Config(0, spec0, Array(), List(), 
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
