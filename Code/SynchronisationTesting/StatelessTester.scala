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
      showMatches(matches)
      // for(i <- 0 until matches.length) println(s"$i: "+matches(i))
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

  // /** Are sync1 and sync2 disjoint? */
  // private def disjoint(sync1: SyncIndices, sync2: SyncIndices) = {
  //   //   sync1.forall(i => !sync2.contains(i))
  //   // done indicates if an element of sync1 is in sync2
  //   var xs = sync1; var done = false
  //   while(xs.nonEmpty && !done){
  //     // Does sync2 contain x?  If so, set done
  //     var ys = sync2; val x = xs.head; xs = xs.tail
  //     while(ys.nonEmpty && ys.head != x) ys = ys.tail
  //     done = ys.nonEmpty
  //   }
  //   !done
  // }

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
      val length = matches.length
      // Find the invocation with fewest matches, and consider all ways of
      // synchronising it
      val index = toDo.minBy(matches(_).length) 
      for(sync <- matches(index)){
        // Tabulate sync as a bitmap
        val syncBM = new Array[Boolean](length); var sync1 = sync
        while(sync1.nonEmpty){ syncBM(sync1.head) = true; sync1 = sync1.tail }
        // for(ix <- sync) syncBM(ix) = true
        // Build possible synchronisations after sync: for each i, none if i
        // is in sync; otherwise remove synchronisations that include an
        // element of sync.
        val matches2 = new Array[List[SyncIndices]](length); var i = 0
        while(i < length){
          matches2(i) = 
            // if(sync.contains(i)) List[SyncIndices]()
            if(syncBM(i)) List[SyncIndices]() // needed?????
            //else matches(i).filter(sync1 => disjoint(sync,sync1))
            // else matches(i).filter(sync1 => disjointBM(syncBM,sync1))
            else filterDisjoint(syncBM, matches(i))
          i += 1
        }
        // val matches2 = Array.tabulate(matches.length)( i =>
        //   if(sync.contains(i)) List[SyncIndices]()
        //   else matches(i).filter(sync1 => disjoint(sync,sync1))
        // )
        // toDo2 is invocations still to linearise
        val toDo2 = removeSynced(syncBM, toDo, matches2)
        if(toDo2 != null && seen.add(toDo2)){
        // val toDo2 = toDo.diff(sync)
        // if(seen.add(toDo2) && toDo2.forall(i => matches2(i).nonEmpty)){
          val rec = search1(toDo2, matches2)
          if(rec != null) return sync::rec
        }
        // else println("failed")
      } // end of for
      //println("backtracking")
      null
    }
  }

  /** Remove all elements of bm (those in the current synchronisation) from
    * toDo.  If any remaining element has matches2(i) empty (so cannot be
    * synchronised) then return null.  Otherwise return the remaining elements
    * of toDo. */
  @inline private def removeSynced(
    bm: Array[Boolean], toDo: List[Int], matches2: Array[List[SyncIndices]])
      : List[Int] = {
    if(toDo.isEmpty) toDo
    else{
      val i = toDo.head
      if(bm(i)) removeSynced(bm, toDo.tail, matches2) // current sync
      else if(matches2(i).isEmpty) null // cannot be sync'ed
      else{
        val rec = removeSynced(bm, toDo.tail, matches2)
        if(rec == null) null else i::rec
      }
    }
  }

  /** Those elements of syncs that are disjoint from the set represented by bm. 
    * syncs.filter(sync1 => disjointBM(syncBM,sync1)). */
  @inline private 
  def filterDisjoint(bm: Array[Boolean], syncs: List[SyncIndices])
      : List[SyncIndices] = {
    if(syncs.isEmpty) List[SyncIndices]()
    else{
      val sync1 = syncs.head
      if(disjointBM(bm, sync1)) sync1 :: filterDisjoint(bm, syncs.tail)
      else filterDisjoint(bm, syncs.tail)
    }
  }

  /** Are list (a list of Ints) and bm (a bitmap) disjoint? */
  @inline private def disjointBM(bm: Array[Boolean], list: SyncIndices) = {
    var l = list
    while(l.nonEmpty && !bm(l.head)) l = l.tail
    l.isEmpty
  }

  private def showMatches(matches: Array[List[List[Int]]]) = 
    for(i <- 0 until matches.length)
      println(s"$i: "+matches(i).map(_.mkString("(", ", ", ")")).mkString(", "))

  /** Test whether events represents a valid history. */
  private def checkLog(events: Array[Event]): Boolean = {
    val matches = findMatches(events)
    if(matches == null) false
    else{
      // Try to find matching
      val result = search1((0 until matches.length).toList, matches)
      if(verbose) println(result)
      if(result == null){
        println("\n"+events.mkString("\n"))
        println("No matching possible.  Possible synchronisations:")
        showMatches(matches)
        // for(i <- 0 until matches.length) 
        //   println(s"$i: "+matches(i).mkString(", "))
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
