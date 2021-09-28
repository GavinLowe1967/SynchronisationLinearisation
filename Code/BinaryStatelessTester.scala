package synchronisationTesting

/** A tester for synchronisation linearisation in the case of binary
  * synchronisations with a stateless synchronisation object.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to.
  * @matching a PartialFunction describing the results that should be given by
  * a particular pair of operations synchronising. */
class BinaryStatelessTester[Op](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  matching: PartialFunction[(Op,Op), (Any,Any)])
    extends BinaryTester[Op](worker, p, matching){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** Find which invocations within events could synchronise.  
    * @return a pair (isOp1, syncs) such that: isOp1 is a bitmap indicating
    * which invocations corresponded to the first operation of the
    * synchronisation object; and syncs is a bitmap indicating which
    * invocations could synchronise (overlapped in time and returned results
    * consistent with matching.  If some operation can synchronise with no
    * other, then gives an error message and returns null. */
  private def findMatches(events: Array[Event])
      : (Array[Boolean], Array[List[Int]]) = {
    require(events.length%4 == 0); val numInvs = events.length/2
    val calls = getCalls(events) // in Tester

    // Bitmaps showing whether each of calls is the first or second operation.
    val isOp1, isOp2 = new Array[Boolean](numInvs)
    // Calls with which other calls each call could synchronise 
    val syncs = Array.fill(numInvs)(List[Int]())
    for(i <- 0 until numInvs; j <- 0 until numInvs)
      if(i != j && canSync(calls(i), calls(j))){
        syncs(i)::= j; syncs(j)::= i; isOp1(i) = true; isOp2(j) = true
      }
    // Each invocation should be an op1 or op2, but not both
    for(i <- 0 until numInvs; if !(isOp1(i) ^ isOp2(i))){
      assert(!isOp1(i) && !isOp2(i), 
        "Requirements of tester not satisfied: there do not appear to be "+
          "two distinct operations.")
      println; println(events.mkString("\n"))
      println(s"Invocation $i does not synchronise with any other operation.")
      return null
    }

    (isOp1, syncs)
  }

  /** Print history represented by `events`, annotated with matching
    * `matching`. */
  private def debug(events: Array[Event], matching: Array[Int]) = {
    def annotation(e: Event) = {
      val syncIx = matching(e.opIndex)
      if(syncIx >= 0) s" matched with $syncIx" else " unmatched"
    }
    HistoryLog.showHistoryWith(events, annotation)
  }

  /** Run the tester.  Return true if the history produced was synchronisation
    * linearisable. */
  def apply(): Boolean = {
    val events = getLog() // From Tester.class
    findMatches(events) match{
      case (isOp1, syncs) =>
        val (ok,matching) = new FordFulkerson(isOp1, syncs)()
        if(ok) true // succeeded
        else{ debug(events, matching); false }
      case null => false
    }
  }

}
