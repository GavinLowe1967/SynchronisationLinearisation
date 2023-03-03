package synchronisationTesting

object TwoStepLinSpec{
  /** Representation of thread IDs. */
  type ThreadID = Int

  /** States used in the encoded state machine. */
  trait AState[A]
  case class Zero[A]() extends AState[A]
  case class One[A](t: ThreadID, x: A) extends AState[A]

  /** Is state an instance of One? */
  private def isOne[A](state: AState[A]) =
    state match{ case Zero() => false; case One(_,_) => true }

  /** A synchronisation specification object. */
  trait SyncSpecObject[A1,A2,B1,B2]{
    /** The results for a synchronisation between operations providing parameters
      * `x1` and `x2`. 
      * @returns a tuple ((y1, y2), newSyncSpec) where y1 and y2 are the results
      * that should be returned by the two operations, and newSyncSpec is the
      * new state of the SyncSpecObject.  */
    def sync(x1: A1, x2: A2): ((B1,B2), SyncSpecObject[A1,A2,B1,B2])
  }

  /** Factory method for a TwoStepLinSpec specification for `numThreads`
    * threads and using synchronisation specification object `syncSpec`. */
  def apply[A1,A2,B1,B2](numThreads: Int, syncSpec: SyncSpecObject[A1,A2,B1,B2])
      : TwoStepLinSpec[A1,A2,B1,B2] =
    new TwoStepLinSpec[A1,A2,B1,B2](
      Zero(), Array.fill(numThreads)(None), syncSpec)

  /** Factory method for a stateless TwoStepLinSpec specification for
    * `numThreads` threads and using synchronisation specification function
    * `sync1`. */
  def apply[A1,A2,B1,B2](numThreads: Int, sync1: (A1,A2) => (B1,B2))
      : TwoStepLinSpec[A1,A2,B1,B2] = {
    val syncSpec = new SyncSpecObject[A1,A2,B1,B2]{ 
      def sync(x1:A1, x2: A2) = (sync1(x1, x2), this) 
    }
    apply(numThreads, syncSpec)
    //new TwoStepLinSpec[A1,A2,B1,B2](Zero(), Array.fill(numThreads)(None), spec)
  }

  /** Log and perform the op_1 in the two-step linearisation scheme. 
    * @tparam A1 The type of parameters to op_1; it should have a suitable 
    * definition of == and hashCode.
    * @tparam B1 The return type of op_1.
    * @tparam A2 The type of parameters of op_2.
    * @tparam B2 The return type of op_2.
    * @tparam C The type of the concrete synchronisation object.
    * @param log The LinearizabilityLog to use.
    * @param me The identity of the thread. 
    * @param cOp The operation on the concrete synchronisation object.
    * @param string A string to use for creating labels for operations.
    * @param x The parameter of the object.
    * Note: `string` should be such that the Strings `s"$me: $string"` and
    * `s"$me: _$string"` are distinct from the Strings used for any other
    * operations.
    */
  def log2[A1,A2,B1,B2,C](
    log: ox.scl.LinearizabilityLog[TwoStepLinSpec[A1,A2,B1,B2], C],
    me: ThreadID, cOp: C => B1, string: String, x: A1) 
  = {
    var y = null.asInstanceOf[B1]
    log(c => {y = cOp(c)}, s"$me: $string", _.op1(me,x))   // Perform op_1
    log(c => y, s"$me: _$string", _.op1X(me))          // Log \overline{op_1}
  }
}

// =======================================================
 
import TwoStepLinSpec.{AState,SyncSpecObject}

/** A specification object for the two-step linearisability technique.  This
  * assumes that we are testing for synchronisation linearisability of a
  * synchronisation object with operations 
  *   def op1(x1: A1): B1
  *   def op2(x2: A2): B2
  * where if two invocations synchronise, they should return y1 and y2,
  * respectively, where (y1,y2) = sync(x1,x2).  Parameters state and returns 
  * are as in the paper. 
  * 
  * @tparam A1 The type of parameters to op_1; it should have a suitable definition of == and hashCode.
  * @tparam B1 The return type of op_1.
  * @tparam A2 The type of parameters of op_2.
  * @tparam B2 The return type of op_2.
  * @param state The state of the automaton.
  * @param returns Array optionally holding return values for threads.
  * @param spec SpecObject whose sync function defines the result of synchronisations, mapping the pair of parameters to the pair of return values.  */
class TwoStepLinSpec[A1,A2,B1,B2](
  private val state: AState[A1], 
  private val returns: Array[Option[B1]],
  private val syncSpec: SyncSpecObject[A1,A2,B1,B2] 
){
  import TwoStepLinSpec.{Zero,One,ThreadID,isOne}

  /** The type of specification object in the linearizability tester. */
  private type LinSpec = TwoStepLinSpec[A1,A2,B1,B2] // shorthand

  /** returns with t set to ob. */
  @inline private 
  def addToReturns(t: ThreadID, ob: Option[B1]): Array[Option[B1]] = {
    val newReturns = returns.clone; newReturns(t) = ob; newReturns
  }

  /** Operation corresponding to op_1. */
  def op1(t: ThreadID, x: A1): (Unit, LinSpec) = {
    require(state == Zero() && returns(t) == None)
    ((), new LinSpec(One(t,x), returns, syncSpec))
  }

  /** Operation corresponding to op_2. */
  def op2(x2: A2): (B2, LinSpec) = {
    require(isOne(state)); val One(t,x1) = state; 
    val ((y1,y2), newSyncSpec) = syncSpec.sync(x1,x2)
    (y2, new LinSpec(Zero(), addToReturns(t, Some(y1)), newSyncSpec))
  }

  /** Operation corresponding to \overline{op_1}. */
  def op1X(t: ThreadID): (B1, LinSpec) = {
    require(state == Zero() && returns(t).nonEmpty); val Some(y1) = returns(t)
    (y1, new LinSpec(state, addToReturns(t, None), syncSpec))
  }

  /* Need to override hashcode and equality. */

  override def hashCode = {
    var h = state.hashCode*73+syncSpec.hashCode
    for(i <- 0 until returns.length) h = h*73 + returns(i).hashCode
    h
  }

  override def equals(that: Any) = that match{
    case lSpec: LinSpec @unchecked =>
      lSpec.state == state && lSpec.syncSpec == syncSpec && 
      lSpec.returns.sameElements(returns)
  }

  override def toString = s"Spec($state, ${returns.mkString("<",",",">")}"
}
