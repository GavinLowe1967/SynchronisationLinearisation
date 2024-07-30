package synchronisationTesting

import scala.collection.mutable.ArrayBuffer

/** A tester for synchronisation linearisation in the case of binary
  * synchronisations with a stateless synchronisation object.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to.
  * @param matching a PartialFunction describing the results that should be
  * given by a particular pair of operations synchronising. */
class BinaryStatelessTester[Op](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  matching: PartialFunction[(Op,Op), (Any,Any)])
    extends BinaryTester[Op](worker, p, matching){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** Find which invocations within events could synchronise.  
    * @return a pair (isOp1, syncs) such that: isOp1 is a bitmap indicating
    * which invocations corresponded to the first operation of the
    * synchronisation object; and syncs(i) is a list of indices of invocations
    * with which the ith invocation can synchronise (overlapped in time and
    * returned results consistent with matching.  If some invocation can
    * synchronise with no other, or if some pending invocations should have
    * synchronised, then gives an error message and returns null. */
/*
  private def findMatchesX(events: Array[Event])
      : (Array[Boolean], Array[List[Int]]) = {
    val (calls, pending) = getCalls(events) // in Tester
    val numInvs = calls.length 
    // Bitmaps showing whether each of calls is the first or second operation.
    val isOp1, isOp2 = new Array[Boolean](numInvs)
    // Calls with which other calls each call could synchronise 
    val syncs = Array.fill(numInvs)(List[Int]())
    for(i <- 0 until numInvs; j <- 0 until numInvs)
      if(i != j && canSync(calls(i), calls(j))){
        syncs(i)::= j; syncs(j)::= i; isOp1(i) = true; isOp2(j) = true
      }

    // Check that every invocation is an op1 or op2, but not both.  And check
    // that every operation that returned could have synchronised.
    for(i <- 0 until numInvs; if !(isOp1(i) ^ isOp2(i))){
      assert(!isOp1(i) && !isOp2(i), 
        "Requirements of tester not satisfied: there do not appear to be "+
          "two distinct operations.")
      println(); println(events.mkString("\n"))
      println(s"Invocation $i does not synchronise with any other completed "+
        "operation.")
      return null
    }

    // Test if any two pending invocations could synchronise
    canAnyPendingSync(pending) match{ // in BinaryTester
      case (i,j) => 
        println(); println(events.mkString("\n"))
        println(s"Pending invocations "+pending(i).opIndex+" and "+
          pending(j).opIndex+" should have synchronised.")
        null
      case null =>  (isOp1, syncs)
    }
  }
 */
  private def findMatches(events: Array[Event])
      : (Array[Boolean], Array[List[Int]]) = {
    // Call events of returned, non-returned invocations
    val (calls, pendingAtEnd) = getCalls(events) // in Tester
    // Note: It might be better to combine the above with the traversal below.
    // println(events.map(e => 
    //   e.toString+" "+
    //     (e match{ case ce: CallEvent[Op,Any]@unchecked => ce.ret; case _ => })+
    //     " "+e.opIndex
    // ).mkString("\n"))
    // println("pendingAtEnd = \n"+pendingAtEnd.mkString("\n")) 
    val numInvs = calls.length
    // Calls with which each call could synchronise; indexed as in calls
    val syncs = Array.fill(numInvs)(List[Int]())
    // Bitmaps showing whether each of calls is the first or second operation.
    val isOp1, isOp2 = new Array[Boolean](numInvs)
    // We traverse events, to identify possible synchronisations.  `pending`
    // stores CallEvents for invocations that are pending at this point
    var pending = List[CallEvent[Op,_]]()
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,Any] @unchecked  =>
        val i = ce.opIndex
        if(/*i >= 0*/ ce.ret != null){
          // Find synchronisations between ce and pending invocations
          for(ce1 <- pending){
            val i1 = ce1.opIndex
            if(canSync0(ce, ce1)){
              syncs(i) ::= i1; syncs(i1) ::= i; isOp1(i) = true; isOp2(i1) = true
            }
            else if(canSync0(ce1, ce)){
              syncs(i) ::= i1; syncs(i1) ::= i; isOp2(i) = true; isOp1(i1) = true
            }
          }
          pending ::= ce
        }
        // else ce.ret == null, and this op is in pendingAtEnd

      case re: ReturnEvent[Op,_] @unchecked => 
        // Remove the corresponding CallEvent from pending
        val i = re.opIndex; assert(i >= 0)
        val (before, _::after) = pending.span(_.opIndex != i)
        pending = before ++ after
        // Check it synchronised, and has a unique operation type
        if(!(isOp1(i) ^ isOp2(i))){
          assert(!isOp1(i) && !isOp2(i),
            "Requirements of tester not satisfied: there do not appear to be "+
              "two distinct operations.")
          println(); println(events.mkString("\n"))
          println(s"Invocation $i does not synchronise with any other "+
            "completed operation.")
          return null
        }
    } // end of for(...) ... match
    assert(pending.isEmpty, 
      "\n"+events.takeWhile(_ != null).map(e => e.toString+" "+e.opIndex).mkString("\n"))

    // Test if any two pending invocations could synchronise
    canAnyPendingSync(pendingAtEnd) match{ // in BinaryTester
      case (i,j) => 
        println(); println(events.mkString("\n"))
        println(s"Pending invocations "+pendingAtEnd(i).opIndex+" and "+
          pendingAtEnd(j).opIndex+" should have synchronised.")
        null
      case null =>  (isOp1, syncs)
    }
  }

  /** Print history represented by `events`, annotated with matching
    * `matching`. */
  private def debug(events: Array[Event], matching: Array[Int]) = {
    def annotation(e: Event) = {
      if(e.opIndex >= matching.length) " not returned"
      // Note: I'm not certain about the above case.  The condition can arise
      // (every few hundred cases with ChanTester within BugFinderExperiment).
      // But I haven't seen a history including it.
      else{
        // assert(e.opIndex < matching.length,
        //   "Error\n"+events.mkString("\n")+
        // "\nmatching: "+matching.mkString(", "))
        val syncIx = matching(e.opIndex)
        if(syncIx >= 0) s" matched with $syncIx" else " unmatched"
      }
    }
    HistoryLog.showHistoryWith(events, annotation)
  }


  /** Check the log represented by events. */
  private def checkLog(events: Array[Event]): Boolean = 
    findMatches(events) match{
      case (isOp1, syncs) =>
        val (ok,matching) = new FordFulkerson(isOp1, syncs)()
        if(ok) true // succeeded
        else{ debug(events, matching); false }
      case null => false
    }

  /** Run the tester.  Return true if the history produced was synchronisation
    * linearisable. 
    * @param delay if positive, the amount of time after which the testing 
    * system should be interrupted. */
  def apply(delay: Int = -1): Boolean = { 
    // println(delay)
    if(delay > 0){
      val (deadlock, events) = getLogDetectDeadlock(delay) // From Tester
      checkLog(events)
    }
    else{ val events = getLog(); checkLog(events) }
  }

}
