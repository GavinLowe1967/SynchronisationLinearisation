package synchronisationTesting

class HomogeneousBinaryStatelessTester[Op](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  matching: PartialFunction[(Op,Op), (Any,Any)])
    extends BinaryTester(worker, p, matching){

  import HistoryLog.{Event,CallEvent,ReturnEvent}

  /** A bitmap, showing which invocations are candidate synchronisations. */
  type BitMap = Array[Array[Boolean]]

  /** Find which invocations within events could synchronise.
    * @return a bitmap `syncs` such that `syncs(i)(j)` is true if 
    * invocations `i` and `j` could synchronise, or null if any invocation
    * could synchronise with no other. */
  private def findMatches(events: Array[Event]): BitMap = {
    // require(events.length%4 == 0); val numInvs = events.length/2
    val (calls, pending) = getCalls(events) // in Tester
    val numInvs = calls.length
// FIXME: consider pending
    // Calls with which other calls each call could synchronise 
    val syncs = Array.ofDim[Boolean](numInvs,numInvs) 
    for(i <- 0 until numInvs; j <- 0 until numInvs)
      if(i != j && canSync(calls(i), calls(j))){
        syncs(i)(j) = true; syncs(j)(i) = true
      }

    // Check that every operation that returned could have synchronised.
    for(i <- 0 until numInvs; if syncs(i).forall(_ == false)){
      println(); println(events.mkString("\n"))
      println(s"Invocation $i does not synchronise with any other operation.")
      return null
    }

    // Test if any two pending invocations could synchronise
    canAnyPendingSync(pending) match{ // in BinaryTester
      case (i,j) => 
        println(); println(events.mkString("\n"))
        println(s"Pending invocations "+pending(i).opIndex+" and "+
          pending(j).opIndex+" should have synchronised.")
        null
      case null => syncs
    }
  }

  /** The list of i such that syncs(i). */
  private def syncsOf(syncs: Array[Boolean]): List[Int] = 
    (0 until syncs.length).toList.filter(syncs(_))

  /** Get the connected component that contains n in the graph with edges
    * edges. */
  private def getComponent(n: Int, edges: BitMap): List[Int] = {
    val size = edges.length; var component = List(n)
    val stack = new scala.collection.mutable.Stack[Int]; stack.push(n)
    val seen = new Array[Boolean](size); seen(n) = true
    while(stack.nonEmpty){
      val n1 = stack.pop()
      for(n2 <- 0 until size; if edges(n1)(n2) && !seen(n2)){
        component::= n2; stack.push(n2); seen(n2) = true
      }
    }
    component
  }

  /** Give debugging output corresponding to events.
    * @param syncs bitmap showing which pairs of invocations could synchronise.  
    * @param bestMatching the best matching found; if bestMatching(i) = j >= 0,
    * then it signifies that i and j synchronised.
    * @param stuckOn the index of the first invocation that is not matched.  */
  private def debug(events: Array[Event], syncs: BitMap, 
    bestMatching: Array[Int], stuckOn: Int) 
  = {
    // String representing possible synchronisations of e
    def syncInfo(e: Event) =
      " can synchronise with "+syncsOf(syncs(e.opIndex)).mkString(", ")
    // Test if the connected component for stuckOn is of odd size.
    val component = getComponent(stuckOn, syncs)
    if(component.length%2 == 1){
      println(HistoryLog.showHistoryWith(events, syncInfo))
      println("Invocations "+component.sorted.mkString(", ")+
        " can synchronise only with one another,\n"+
        "but these cannot be paired because there are an odd number.")
    }
    else{     // Show maximal matching found.
      val syncWidth = events.map(e => syncInfo(e).length).max+2
      def annotation(e: Event) = {
        val syncIx = bestMatching(e.opIndex)
        HistoryLog.lJustify(syncInfo(e)+";", syncWidth)+
        (if(syncIx >= 0) s"matched with $syncIx"
        else if(e.opIndex == stuckOn) s"impossible to match"
        else "")
      }
      println(HistoryLog.showHistoryWith(events, annotation))
    }
    // It would be good to improve the feedback in the last case.
  }

  /** Check the log given by events. */
  private def checkLog(events: Array[Event]): Boolean = {
    val syncs = findMatches(events)
    if(syncs == null) false
    else{
      val dfs = new DFS(syncs); 
      val (ok, bestMatching, stuckOn) = dfs()
      if(ok) true
      else{ debug(events, syncs, bestMatching, stuckOn); false }
    }
  }

  /** Run the tester.  Return true if the history produced was synchronisation
    * linearisable. */
  def apply(delay: Int = -1): Boolean = {
    if(delay > 0){
      val (deadlock, events) = getLogDetectDeadlock(delay) // From Tester
      checkLog(events)
    }
    else{
      val events = getLog() // From Tester
      checkLog(events)
    }
  }

}
