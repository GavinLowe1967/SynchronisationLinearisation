package synchronisationTesting

import scala.collection.mutable.ArrayBuffer

/** The base class of StatefulTester and NondetStatefulTester, collecting
  * common code. 
  * 
  * @tparam Op the type representing operations on the synchronisation object.
  * @tparam S the type of the specification object.
  * 
  * @param worker definition of a worker on the synchronisation object,
  *  parameterised by its identity and the log it will write to.
  * @param p the number of threads to run.
  * @param arities the list of arities of synchronisations. 
  * @param specMatching a description of the results that should be given by a 
  *  particular list of operations synchronising and the corresponding state
  *  of the specification object.  The value returned by the function is
  *  ignored in this abstract class.
  * @param suffixMatching a predicate saying whether a list of invocations could
  *  form a suffix of a synchronisation.
  * @param spec0 the initial state of the specification object.
  * @param doASAP should the ASAP optimisation be used?
  * @param verbose flag to give verbose output. */
abstract class StatefulTesterBase[Op,S](
  worker: (Int, HistoryLog[Op]) => Unit, p: Int, arities: List[Int],
  specMatching: S => PartialFunction[List[Op], Any],
  suffixMatching: List[Op] => Boolean = (es: List[Op]) => true,
  spec0: S, doASAP: Boolean, verbose: Boolean)
    extends Tester[Op](worker, p){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** The events in the log.  Set by apply. */
  private var events: Array[Event] = null

  /** The number of events in the log.  Set by apply. */
  private var length = -1

  /** The number of invocations in the log that return.  Set by apply. */
  private var invocs = -1

  protected val maxSyncArity = arities.max

  @inline private def contains(es: List[CallEvent1], e: CallEvent1): Boolean = 
    es.nonEmpty && ((es.head eq e) || contains(es.tail, e))

  /** Get all lists of potential synchronisations.  All lists of length up to
    * maxArity of CallEvents by different threads, each paired with their
    * operations. */
  protected def allArgsLists(calls: List[CallEvent1], maxArity: Int)
      : Array[List[(SyncEs, List[Op])]] = {
    // The array holding the result
    val result = new Array[List[(SyncEs, List[Op])]](maxArity+1); 
    result(0) = List( (List(), List()) )
    for(arity <- 1 to maxArity){
      result(arity) = List(); var eoss = result(arity-1)
      while(eoss.nonEmpty){
        val (es,ops) = eoss.head; eoss = eoss.tail; var cs = calls
        // Consider extending es with a member of calls
        while(cs.nonEmpty){
          val e = cs.head; cs = cs.tail
          if(e.ret != null && !contains(es,e) && suffixMatching(e.op::ops) ) 
            result(arity) ::= ((e::es, e.op::ops))
        }
      }
    }
    result
  }

  /** All lists of potential synchronisations including e and other invocations
    * from calls, each paired with their operations. */
  protected def allArgsListsWith(calls: List[CallEvent1], e: CallEvent1)
      : Array[List[(SyncEs, List[Op])]] = {
    require(e.ret != null)
    // require(!calls.contains(e))
    // Possible synchronisations excluding or including e
    val withoutE = new Array[List[(SyncEs, List[Op])]](maxSyncArity+1)
    val withE = new Array[List[(SyncEs, List[Op])]](maxSyncArity+1)
    withoutE(0) = List( (List(), List()) ); withE(0) = List()
    for(arity <- 1 to maxSyncArity){
      withoutE(arity) = List(); withE(arity) = List()
      // Try adding e or a member of calls to each member of withoutE(arity-1)
      var eoss = withoutE(arity-1)
      while(eoss.nonEmpty){
        val (es,ops) = eoss.head; eoss = eoss.tail
        if(arity != maxSyncArity){
          var cs = calls
          while(cs.nonEmpty){
            val e1 = cs.head; cs = cs.tail
            if(e1.ret != null && !contains(es,e1) && suffixMatching(e1.op::ops))
              withoutE(arity) ::= ((e1::es, e1.op::ops))
          }
        }
        if(suffixMatching(e.op::ops)) withE(arity) ::= ((e::es, e.op::ops))
      }
      // Try adding a member of calls to each member of withE(arity-1)
      eoss = withE(arity-1)
      while(eoss.nonEmpty){
        val (es, ops) = eoss.head; eoss = eoss.tail; var cs = calls
        while(cs.nonEmpty){
          val e1 = cs.head; cs = cs.tail
          if(e1.ret != null && !contains(es,e1) && suffixMatching(e1.op::ops))
            withE(arity) ::= ((e1::es, e1.op::ops))
        }
      }
    }
    withE
  }

  /** Array representing invocations that have been matched so far.  A negative
    * value represents an unmatched invocation. */
  type Matching = Array[List[Int]]

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
      case _: CallEvent[Op,_] @unchecked => ""
      case _: ReturnEvent[Op,_] @unchecked => 
        val syncIndices = matching(e.opIndex)
        if(syncIndices != null){
          val others = syncIndices.filter(_ != e.opIndex)
          ( if(others.isEmpty) ":  unary"
            else ":  matched with "+others.mkString(", ") 
          ) + "; linearisation index "+linIndices(e.opIndex)
        }
        else ":  unmatched"
    }))

  /** Is rets in decreasing order of index? */
  private def isSortedByIndex(rets: List[ReturnEvent[Op,_]]): Boolean =
    rets.length <= 1 || 
      rets(0).index > rets(1).index && isSortedByIndex(rets.tail)

  /** Information about a candidate synchronisation: the subsequent state of the
    * specification object, and the call events involved. */
  type SyncInfo = (S, SyncEs)

  /* Base classes provide the following two functions. */

  /** All possible synchronisations between invocations from `calls` given state
    * `spec` of the specification. */
  protected def allSyncs(spec: S, calls: List[CallEvent1]): List[SyncInfo]

  /** All possible synchronisations involving e and other invocations from
    * `calls` given state `spec` of the specification. */
  protected def allSyncsWith(spec: S, calls: List[CallEvent1], e: CallEvent1)
      : List[SyncInfo]

  /** Can the pending invocations in es synchronise given state spec of the
    * specification? */
  private def canSyncPending(spec: S, es: SyncEs): Boolean = {
    val ops = es.map(_.op)
    // Check the invocations can produce the observed result
    specMatching(spec).isDefinedAt(ops) &&
    (try{ specMatching(spec)(ops); true }
    catch{ case _: IllegalArgumentException => false })
  }

  /* Note: 2025-01-08: canReturn was previously held in the reversed order, and
   * I don't know why. */

  /** A configuration in the search. 
    * @param the index in the history reached so far. 
    * @param spec the state of the specification object. 
    * @param canReturn invocations that have been linearised so can return, 
    * sorted in order of index.
    * @param pending invocations that have been called but not yet linearised,
    * sorted in decreasing order of index.
    * @param matching array showing which invocations have been matched so far.
    * matching(ix) gives the list of indices in the synchronisation for ix.
    * @param matchingSize the length of the longest prefix of the calls of the 
    * history that has been matched. 
    * @param linIndices the linearisation indices, giving the order in which
    * invocations synchronised.  If linIndices(ix) >= 0, it gives the
    * linearisation index for the invocation with index ix.
    * @param nextLinIndex the number of synchronisations so far. 
    */
  private class Config(
    index: Int, spec: S, 
    canReturn: Array[ReturnEvent1], pending: List[CallEvent1],
    val matching: Matching, val matchingSize: Int,
    val linIndices: Array[Int], val nextLinIndex: Int) 
      extends Tester.Config(index, spec, canReturn, pending)
  { 
    /** Elements of ps not in es. */
    private def mkNewPending(ps: List[CallEvent1], es: List[CallEvent1])
        : List[CallEvent1] =
      if(ps.isEmpty) List[CallEvent1]()
      else{
        val p = ps.head
        if(contains(es,p)) mkNewPending(ps.tail, es) 
        else p::mkNewPending(ps.tail, es)
      }
      // ps.filter(e => !contains(es,e))

    /** New value for canReturn formed by adding the ReturnEvents from es. */
    private def mkNewCanReturn(es: List[CallEvent1]) = {
      val esLen = es.length; val newRs = new Array[ReturnEvent1](esLen)
      var i = 0
      while(i < esLen){ newRs(i) = es(i).ret; i += 1 }
      merge(newRs.sortBy(e => e.index), canReturn)
    }

    /** The next configuration from this after the synchronisation corresponding
      * to spec1 and es. */
    private def mkNext(spec1: S, es: List[CallEvent1]): Config = {
      // Update pending and canReturn
      val newPending = mkNewPending(pending, es)
      val newCanReturn = mkNewCanReturn(es)
      // merge(es.map(_.ret).sortBy(e => - e.index), canReturn)
      // assert(isSortedByIndex(newCanReturn))
      // Update matching, matchingSize, linIndices
      val newMatching = matching.clone; val indices = es.map(_.opIndex)
      val newLinIndices = linIndices.clone; var j = 0
      while(j < indices.length){
        val i = indices(j); j += 1
        newMatching(i) = indices; newLinIndices(i) = nextLinIndex
      }
      var newMatchingSize = matchingSize
      while(newMatchingSize < invocs && newMatching(newMatchingSize) != null)
        newMatchingSize += 1
      // Is this a new maximum matching?
      if(newMatchingSize > maxMatchingSize){
        maxMatching = newMatching; maxMatchingSize = newMatchingSize
        maxLinIndices = newLinIndices
      }
      new Config(index, spec1, newCanReturn, newPending,
        newMatching, newMatchingSize, newLinIndices, nextLinIndex+1)
    }

    /** Find all configurations reachable by 0 or more synchronisations from
      * this, and add to buffer. */
    private def iterateLinearisations(buffer: List[Config]): List[Config] = {
      if(done) println("done")
      var buff = this::buffer
      for((spec1, es) <- allSyncs(spec,pending)){
        val config1 = mkNext(spec1,es)
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
        var buff = buffer
        for((spec1,es1) <- allSyncsWith(spec,pending,ce)){
          val config1 = mkNext(spec1,es1)
          buff = config1.iterateLinearisations(buff)
        }
        buff
      }

    /** Next configurations in the search graph. */
    def nexts: List[Config] = {
      var result = List[Config]()
      // Consider next event
      if(index < length) events(index) match{
        case ce: CallEvent[Op,Any] @unchecked =>
          // add to pending
          assert(pending.isEmpty || ce.index > pending.head.index)
          val newPending = ce::pending
          val config1 = new Config(index+1, spec, canReturn, newPending,
            matching, matchingSize, linIndices, nextLinIndex)
          result ::= config1
          // Consider linearisations, starting with a linearisation involving ce
          if(doASAP) result = config1.iterateLinearisationsWith(ce, result)

        case re: ReturnEvent[Op,Any] @unchecked =>
          // maybe allow this event to return
          maybeRemoveReturn(re) match{
            case Some(newCanReturn) =>
              result ::= new Config(index+1, spec, newCanReturn, pending,
                matching, matchingSize, linIndices, nextLinIndex)
            case None => {}
          }
      }
      // Consider linearisations
      if(!doASAP) for((spec1, es) <- allSyncs(spec,pending))
        result ::= mkNext(spec1, es)
      result
    } // end of nexts

    /** Does this represent a complete linearisation? */
    def done: Boolean = 
      if(index == length && canReturn.isEmpty){
        for(e <- pending) assert(e.ret == null)
        // Check that no calls in pending can return
        val allPendingArgs = 
          allPotentialPendingSyncs(pending.toArray, maxSyncArity) // in Tester
        for(arity <- arities; arg <- allPendingArgs(arity))
          if(canSyncPending(spec, arg)) return false
        true
      }
      else false

  } // end of Config

  /** Perform a DFS. */
  private def search(): Boolean = {
    length = events.length
    val (calls, pending) = getCalls(events) // from Tester
    invocs = calls.length
    maxMatching = Array.fill[List[Int]](invocs)(null) 
    maxLinIndices = Array.fill(invocs)(-1)
    val config0 = new Config(0, spec0, Array(), List(), 
      maxMatching, 0, maxLinIndices, 0)
    def atEnd(c: Config) = {
      if(verbose) showMatching(c.matching, c.linIndices)
      assert(c.matchingSize == invocs)
      if(arities.length == 1) assert(c.nextLinIndex*arities.head == invocs)
    }
    if(!Tester.search[Op,S,Config](config0, atEnd)){
      println("Failed"); showMatching(maxMatching, maxLinIndices); false
    }
    else true
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
