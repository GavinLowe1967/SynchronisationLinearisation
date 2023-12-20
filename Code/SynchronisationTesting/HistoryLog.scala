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

    /** Place holder for the operation index of this event within the log. */
    var opIndex = -1

    /** Is this a returnEvent? */
    val isReturn : Boolean

    /** String representing opIndex with padding.  This is intended for values of
      * opIndex less than 1000. */
    protected def opIndexString = s"$opIndex:"+" "*(3-opIndex.toString.length)

    override def hashCode = index
  }

  /** An event representing the call of an operation.
    * @tparam Op the type of operations.
    * @tparam A the return type of the operation.
    * @param t the identity of the thread.
    * @param op a representation of the operation being called. */
  case class CallEvent[Op,A](t: Int, op: Op) extends Event{
    /** The corresponding return event. */
    var ret: ReturnEvent[Op,A] = null

    val isReturn = false

    override def toString = s"$opIndexString Call of $op" // Thread $t calls $op"
  }

  /** An event representing an operation returning.
    * @tparam A the return type of the operation.
    * @param t the identity of the thread.
    * @param result the result returned. */
  case class ReturnEvent[Op,A](t: Int, op: Op, result: A) extends Event{
    val isReturn = true

    override def toString = s"$opIndexString Return of $result from $op"
    // Thread $t returns $result from $op"
  }

  /** Left-justify st in a column of width `width`. */
  def lJustify(st: String, width: Int): String = {
    val len = st.length; assert(len <= width); st+" "*(width-len)
  }

  /** A String, representing events, with each event e annotated with
    * annotation(e). */
  def showHistoryWith(events: Array[Event], annotation: Event => String)
      : String = {
    assert(events.nonEmpty)
    val width = events.map(_.toString.length).max
    (for(e <- events) yield lJustify(e.toString, width)+annotation(e)
    ).mkString("\n")
  }

  /** A String, representing events. */
  def showHistory(events: Array[Event]): String = 
    events.map(_.toString).mkString("\n")
}
