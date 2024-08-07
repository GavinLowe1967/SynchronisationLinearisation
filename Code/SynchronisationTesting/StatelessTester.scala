package synchronisationTesting


/** A generic stateless tester.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to. 
  * @param p the number of threads to run.
  * @param arities a list of all possible arities for synchronisations.
  * @param matching a PartialFunction describing the results that should be
  * given by a particular pair of operations synchronising. 
  */
class StatelessTester[Op](  
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int, arities: List[Int],
  matching: PartialFunction[List[Op], List[Any]],
  suffixMatching: List[Op] => Boolean = (es: List[Op]) => true,
  verbose: Boolean = false)
    extends Tester[Op](worker, p){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  // /** The CallEvents used here. */
  //type CallEvent1 = CallEvent[Op,_]

  /** A representation of a synchronisation: the list of CallEvents for the
    * invocations that synchronise. */
  //type SyncEs = List[CallEvent1]

  /** Maximum arity of any synchronisation. */
  private val maxArity = arities.max

  /** Can the events es synchronise? */
  private def canSync(es: SyncEs): Boolean = {
    val ops = es.map(_.op)
    // Check the invocations can produce the observed result
    matching.isDefinedAt(ops) &&
    (try{ es.map(_.ret.result) == matching(ops) }
    catch{ case _: IllegalArgumentException => false })
  }

  /** Can the pending invocations in es synchronise? */
  private def canSyncPending(es: SyncEs): Boolean = {
    val ops = es.map(_.op)
    // Check the invocations can produce the observed result
    matching.isDefinedAt(ops) &&
    (try{ matching(ops); true }
    catch{ case _: IllegalArgumentException => false })
  }

  /** Information about a potential synchronisation: the list of CallEvents, the
    * maximum start index and the minimum end index. */
  private type SyncInfo = (SyncEs, Int, Int)

  /** Get all lists of potential synchronisations.  All lists of length up to
    * arities.max of CallEvents by different threads, together with the
    * maximum start index and minimum end index. */
  private def allArgsLists(calls: Array[CallEvent1])
      : Array[List[SyncInfo]] = {
    val result = new Array[List[SyncInfo]](maxArity+1)
    result(0) = List((List(), Int.MinValue, Int.MaxValue))
    for(arity <- 1 to maxArity){
      result(arity) = List(); var triples = result(arity-1)
      while(triples.nonEmpty){
        // Consider extending es
        val (es,start,end) = triples.head; triples = triples.tail
        var i = 0; var done = false
        while(i < calls.length && !done){
          // Consider e::es as a possible synchronisation
          val e = calls(i); i += 1
          // test if we're not beyond the end of es
          if(e.index < end){
            if(e.ret.index > start && suffixMatching(e.op::es.map(_.op))){
              // Test if es contains an event for e's thread
              var es1 = es
              while(es1.nonEmpty && es1.head.t != e.t) es1 = es1.tail
              if(es1.isEmpty /* && e.ret.index > start*/)
                result(arity) ::= (e::es, start max e.index, end min e.ret.index)
            }
          }
          else done = true // all later elements of calls also beyond end
        }
      }
    }
    result
  }


  /** A representation of a synchronisation: the list of indices of CallEvents
    * for the invocations that synchronise. */
  type SyncIndices = List[Int]

  /** Find which invocations could synchronise.  If any returned invocation has
    * no possible synchronisations, return null.  If any pending invocations
    * could have synchronised, return null.  Otherwise return an array indexed
    * by invocation indices, giving the list of possible synchronisations for
    * each invocation.  */
  private def findMatches(events: Array[Event]): Array[List[SyncIndices]] = {
    val (calls,pending) = getCalls(events) 
    // Identify possible synchronisations
    val matches = Array.fill(calls.length)(List[SyncIndices]())
    val allArgs = allArgsLists(calls)
    for(arity <- arities; (arg,_,_) <- allArgs(arity); 
        if canSync(arg); ce <- arg)
      matches(ce.opIndex) ::= arg.map(_.opIndex)
    // if(verbose){
    //   println("Matches:")
    //   for(i <- 0 until result.length) println(s"$i: "+result(i))
    // }

    // Check every returned invocation could synchronise; otherwise return null
    val nonSyncs = (0 until matches.length).filter(i => matches(i).isEmpty)
    if(nonSyncs.nonEmpty){
      // for(i <- 0 until matches.length) if(matches(i).isEmpty){
      println(events.mkString("\n"))
      println(s"No candidate synchronisation for invocations "+
        nonSyncs.mkString(", ")+".\nPossible synchronisations:")
      for(i <- 0 until matches.length) println(s"$i: "+matches(i))
      return null
    }

    // Check no collection of pending calls could have synchronised
    val allPendingArgs = allPotentialPendingSyncs(pending, maxArity)// in Tester
    for(arity <- arities; arg <- allPendingArgs(arity); if canSyncPending(arg)){
      println(events.mkString("\n"))
      println("Missed synchronisation: "+arg)
      return null
    }

    matches
  }

  /** Are sync1 and sync2 disjoint? */
  private def disjoint(sync1: SyncIndices, sync2: SyncIndices) = {
    //   sync1.forall(i => !sync2.contains(i))
    // done indicates if an element of sync1 is in sync2
    var xs = sync1; var done = false
    while(xs.nonEmpty && !done){
      // Does sync2 contain x?  If so, set done
      var ys = sync2; val x = xs.head; xs = xs.tail
      while(ys.nonEmpty && ys.head != x) ys = ys.tail
      done = ys.nonEmpty
    }
    !done
  }

  /** Seen set.  Each entry gives a set of invocation indices waiting to be
    * linearised.  */
  private val seen = new scala.collection.mutable.HashSet[List[Int]]

  /** Recursive depth-first search.  If a matching is found, returns the list of
    * synchronisations; otherwise returns null. 
    * @param toDo the indices of invocations that have not yet been linearised.
    * @param matches, for each invocation, the list of candidate 
    * synchronisations (where each synchronisation is represented by the list
    * if indices of the synchronising invocations). */
  private def search1(toDo: List[Int], matches: Array[List[SyncIndices]])
      : List[SyncIndices] = {
    if(toDo.isEmpty) List()
    else{
      // Find the invocation with fewest matches
      val index = toDo.minBy(matches(_).length) 
      for(sync <- matches(index)){
        // println("Trying to synchronise "+sync)
        val matches2 = Array.tabulate(matches.length)( i =>
          if(sync.contains(i)) List[SyncIndices]()
          else matches(i).filter(sync1 => disjoint(sync,sync1))
        )
        val toDo2 = toDo.diff(sync)
        if(seen.add(toDo2) && toDo2.forall(i => matches2(i).nonEmpty)){
          val rec = search1(toDo2, matches2)
          if(rec != null) return sync::rec
        }
        // else println("failed")
      } // end of for
      //println("backtracking")
      null
    }
  }

  /** Test whether events represents a valid history. */
  private def checkLog(events: Array[Event]): Boolean = {
    val matches = findMatches(events)
    if(matches == null) false
    else{
      // Try to find matching
      val result = search1((0 until matches.length).toList, matches)
      if(verbose) println(result)
      if(result == null){
        println(events.mkString("\n"))
        println("No matching possible.  Possible synchronisations:")
        for(i <- 0 until matches.length) println(s"$i: "+matches(i))
        false
      }
      else true
    }
  }

  /** Run the tester.  Return true if the history produced was synchronisation
    * linearisable. */
  def apply(delay: Int = -1): Boolean = {
    if(delay > 0){
      val (deadlock, events) = getLogDetectDeadlock(delay) // From Tester
      checkLog(events)
    }
    else{ val events = getLog(); checkLog(events) }
  }

}

// ================================================================== END


/*
  private def search(matches: Array[List[SyncIndices]]) = {
    val numInvocs = matches.length
    // Each entry on the stack represents the list of invocation indices that
    // have not been fixed, and for each such, the list of SyncIndicess it
    // could be involved in.
    type StackEl = (List[Int], Array[List[SyncIndices]])
    val stack = new scala.collection.mutable.Stack[StackEl]
    stack.push(((0 until numInvocs).toList, matches))
    var done = false
    while(!done && stack.nonEmpty){
      val (toDo1, matches1) = stack.pop
      if(toDo1.isEmpty) done = true
      else{
        val index = toDo1.head
        // IMPROVE: the one with fewest matches
        for(sync <- matches1(index)){
          println("Trying to synchronise "+sync)
          val matches2 = Array.tabulate(numInvocs)( i => 
            if(sync.contains(i)) List[SyncIndices]()
            else matches1(i).filter(sync1 => disjoint(sync,sync1))
          )
          val toDo2 = toDo1.diff(sync)
          if(toDo2.forall(i => matches2(i).nonEmpty))
            stack.push((toDo2, matches2))
          else println("backtracking")
        }
      }
    }
    println("done")
  }
 */

/*
  /** Get all lists of potential synchronisations.  All lists of length up to
    * arities.max of CallEvents by different threads.
    * @param calls the CallEvents from the log.  */
  private def allArgLists(calls: Array[CallEvent1]): Array[List[SyncEs]] = {
    val len = calls.length
    // Tabulate which invocations overlap (index of .opIndex values)
    val overlap = Array.ofDim[Boolean](len, len)
    for(i <- 0 until len; j <- i+1 until len){
      overlap(i)(j) = calls(i).index < calls(j).ret.index && 
        calls(j).index < calls(i).ret.index 
      overlap(j)(i) = overlap(i)(j)
    }
    val maxArity = arities.max
    val result = new Array[List[SyncEs]](maxArity+1)
    result(0) = List(List())
    for(arity <- 1 to maxArity){
      result(arity) = List()
      for(es <- result(arity-1); e <- calls){
        // Check: e isn't the same thread as any element of es; e starts
        // before each element of es returns; e doesn't return before any
        // element of es starts
        var es1 = es
        while(es1.nonEmpty && overlap(e.opIndex)(es1.head.opIndex)) 
          es1 = es1.tail
        if(es1.isEmpty)
//        if(es.forall(e1 => overlap(e.opIndex)(e1.opIndex)))
//            e1.t != e.t && e.index < e1.ret.index && e1.index < e.ret.index))
          result(arity) ::= (e::es)
      }
    }
    result
  }
 */
