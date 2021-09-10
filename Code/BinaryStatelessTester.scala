package synchronisationTesting

/** A tester for synchronisation linearisation in the case of binary
  * synchronisations with a stateless synchronisation object. */
class BinaryStatelessTester[Op](
  // syncObj: C, specObj: S, 
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  matching: PartialFunction[(Op,Op), (Any,Any)]){

  import HistoryLog.{Event,CallEvent,ReturnEvent}

  /** Can the invocations corresponding to ce1 and ce2 synchronise? */
  private def canSync(ce1: CallEvent[Op,_], ce2: CallEvent[Op,_]): Boolean = {
    // Each is called before the other returns
    ce1.index < ce2.ret.index && ce2.index < ce1.ret.index &&
    matching.isDefinedAt(ce1.op, ce2.op) &&
    (ce1.ret.result, ce2.ret.result) == matching(ce1.op, ce2.op)
  }

  def findMatches(events: Array[Event])
      : (Array[Boolean], Array[List[Int]]) = {
    require(events.length%4 == 0); val numInvs = events.length/2
    // Find call  events.
    val calls = new Array[CallEvent[Op,_]](numInvs); var i = 0
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,_] @unchecked => calls(i) = ce; i += 1
      case _ => {}
    }
    // Bitmaps showing whether each of calls is the first or second operation.
    val isOp1, isOp2 = new Array[Boolean](numInvs)
    // Array indicating whether calls(i) can synchronise with calls(j)
    // val candidates = Array.ofDim[Boolean](numInvs,numInvs)
    // Calls with which other calls each call could synchronise with
    val syncs = Array.fill(numInvs)(List[Int]())
    for(i <- 0 until numInvs; j <- 0 until numInvs)
      if(i != j && canSync(calls(i), calls(j))){
        // candidates(i)(j) = true; 
        syncs(i)::= j; syncs(j)::= i
        isOp1(i) = true; isOp2(j) = true
      }
    // Each invocation should be an op1 or op2, but not both
    for(i <- 0 until numInvs) assert(isOp1(i) ^ isOp2(i))

    // println
    for(i <- 0 until numInvs) 
      println(s"$i:\t"+calls(i).toString+"; returns "+calls(i).ret.result)
    // for(i <- 0 until numInvs) println(s"$i:\t"+candidates(i).mkString("\t"))
    // for(i <- 0 until numInvs) println(s"$i:\t"+syncs(i))
    (isOp1, syncs)
  }

  def apply() = {
    val log = new HistoryLog[Op](p)
    ThreadUtil.runIndexedSystem(p, i => worker(i, log))
    val events = log.get
    // println(events.mkString("\n"))
    val (isOp1, syncs) = findMatches(events)
    val ff = new FordFulkerson(isOp1, syncs)
    if(ff()) println("Succeeded") else println("Failed")
    ()
  }

}
