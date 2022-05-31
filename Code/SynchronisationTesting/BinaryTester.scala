package synchronisationTesting

/** The base of the binary testers. 
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to.
  * @matching a PartialFunction describing the results that should be given by
  * a particular pair of operations synchronising. */
class BinaryTester[Op](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int,
  matching: PartialFunction[(Op,Op), (Any,Any)])
    extends Tester(worker, p){

  import HistoryLog.{Event,CallEvent,ReturnEvent} 

  /** Can the invocations corresponding to ce1 and ce2 synchronise? */
  protected def canSync(ce1: CallEvent[Op,_], ce2: CallEvent[Op,_]): Boolean = {
    // Each is called before the other returns
    ce1.index < ce2.ret.index && ce2.index < ce1.ret.index &&
    matching.isDefinedAt(ce1.op, ce2.op) &&
    (try{ (ce1.ret.result, ce2.ret.result) == matching(ce1.op, ce2.op) }
     catch{ case _: IllegalArgumentException => false })
  }

  /** Could the pending invocations corresponding to ce1 and ce2 have
    * synchronised? */
  private def canSyncPending(ce1: CallEvent[Op,_], ce2: CallEvent[Op,_])
      : Boolean = {
    matching.isDefinedAt(ce1.op, ce2.op) &&
    (try{ matching(ce1.op, ce2.op); true }
     catch{ case _: IllegalArgumentException => false })
  }

  /** Test if any pending operations could have synchronised.  If so, return
    * their indices.  Otherwise return null. */
  protected def canAnyPendingSync(pending: Array[CallEvent[Op,_]]): (Int,Int) = {
    val numPending = pending.length
    for(i <- 0 until numPending; j <- 0 until numPending)
      if(i != j && canSyncPending(pending(i), pending(j))) 
        return(i,j)
    null
  }

}
