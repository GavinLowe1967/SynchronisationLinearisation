package synchronisationTesting

/** A logger to create a history. 
  * @tparam Op a type describing the operations. 
  * @param p the number of threads. */
class HistoryLog[Op](p: Int){
  /** Log storing the events. */
  private val log = new Log[HistoryLog.Event](p)

  /** Log that t performs the computation comp, described by operation Op. */
  def apply[A](t: Int, comp: => A, op: Op) = {
    val e1 = HistoryLog.CallEvent[Op,A](t, op)
    log.add(t, e1)
    val result = comp
    val e2 = HistoryLog.ReturnEvent[Op,A](t, op, result)
    log.add(t, e2)
    e1.ret = e2
  }

  /** Get the log. */
  def get: Array[HistoryLog.Event] = {
    val es = log.get
    // Set index fields
    for(i <- 0 until es.length) es(i).index = i
    es
  }
}

// ==================================================================

/** Companion object. */
object HistoryLog{
  /** Type of events that will be stored in the log. */
  trait Event{
    /** Place holder for the index of this event within the log. */
    var index: Int = -1
  }

  /** An event representing the call of an operation.
    * @tparam Op the type of operations.
    * @tparam A the return type of the operation.
    * @param t the identity of the thread.
    * @param op a representation of the operation being called. */
  case class CallEvent[Op,A](t: Int, op: Op) extends Event{
    /** The corresponding return event. */
    var ret: ReturnEvent[Op,A] = null
    override def toString = s"$t calls $op"
  }

  /** An event representing an operation returning.
    * @tparam A the return type of the operation.
    * @param t the identity of the thread.
    * @param result the result returned. */
  case class ReturnEvent[Op,A](t: Int, op: Op, result: A) extends Event{

    override def toString = s"$t returns $result from $op"
  }
}
