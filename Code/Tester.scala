package synchronisationTesting

/** Base class of all testers.
  * @tparam Op the type representing operations on the synchronisation object.
  * @param worker definition of a worker on the synchronisation object,
  * parameterised by its identity and the log it will write to. */
class Tester[Op](worker: (Int, HistoryLog[Op]) => Unit, p: Int){

  import HistoryLog.{Event,CallEvent}

  /** Run a system of `p` `worker` threads.  Return the contents of the log. */
  protected def getLog(): Array[Event] = {
    val log = new HistoryLog[Op](p)
    ThreadUtil.runIndexedSystem(p, i => worker(i, log))
    log.get
  }

  /** Get an array, enumerating the CallEvents.  Each CallEvent and
    * corresponding ReturnEvent is labelled with its index. */
  protected def getCalls(events: Array[Event]):  Array[CallEvent[Op,_]] = {
    require(events.length%4 == 0); val numInvs = events.length/2
    // Find call  events.
    val calls = new Array[CallEvent[Op,_]](numInvs); var i = 0
    for(j <- 0 until events.length) events(j) match{
      case ce: CallEvent[Op,_] @unchecked => 
        calls(i) = ce; ce.opIndex = i; ce.ret.opIndex = i; i += 1
      case _ => {}
    }
    calls
  }

}

// ==================================================================

// object Tester{
// }
