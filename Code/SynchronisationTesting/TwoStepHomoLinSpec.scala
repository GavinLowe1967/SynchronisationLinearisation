package synchronisationTesting

/** The linearisability specification for a binary homogeneous
  * synchronisation.  In this scheme, the synchronisation is between two
  * instances of the same operation, therefore both must be split, and the
  * retrieval of both return values is deferred.  One example of such a
  * synchronisation is an exchanger, in which two threads swap values of the
  * same type.  However it is not necessary for the return type to be the same
  * as the input type in general.
*/

object TwoStepHomoLinSpec{

  /** Representation of thread IDs. */
  type ThreadID = Int

  /** States used in the encoded state machine. 
  Two states suffice, one (Zero) for no synchronisation in progress, 
  and one (One) for the "first" operation having begun and it's input value stored in the state.
  */
  trait SyncState[A]
  case class Zero[A]() extends SyncState[A]
  case class One[A](t: ThreadID, x: A) extends SyncState[A]

  /** Is state an instance of One? */
  private def isOne[A](state: SyncState[A]) =
    state match{ case Zero() => false; case One(_,_) => true }

  /** A synchronisation specification object. */
  trait SyncSpecObject[A,B]{
    /** The results for a synchronisation between operations providing parameters
      * `x1` and `x2`, both of type A.
      * @returns a tuple ((y1, y2), newSyncSpec) where y1 and y2 are the results
      * that should be returned by the two operations, and newSyncSpec is the
      * new state of the SyncSpecObject.  This allows for stateful
      * synchronisation objects by maintaining the current state in the
      * SyncSpecObject.
      */
    def sync(x1: A, x2: A): ((B,B), SyncSpecObject[A,B])
  }

  /** Factory method for a TwoStepHomoLinSpec specification for `numThreads`
    * threads and using synchronisation specification object `syncSpec`. */
  def apply[A,B](numThreads: Int, syncSpec: SyncSpecObject[A,B])
      : TwoStepHomoLinSpec[A,B] =
    new TwoStepHomoLinSpec[A,B](
      Zero(), Array.fill(numThreads)(None), syncSpec)

  /** Factory method for a stateless TwoStepHomoLinSpec specification for
    * `numThreads` threads and using synchronisation specification function
    * `sync1`. */
  def apply[A,B](numThreads: Int, sync1: (A,A) => (B,B))
      : TwoStepHomoLinSpec[A,B] = {
    val syncSpec = new SyncSpecObject[A,B]{ 
      def sync(x1:A, x2: A) = (sync1(x1, x2), this) 
    }
    apply(numThreads, syncSpec)
    //new TwoStepHomoLinSpec[A,B](Zero(), Array.fill(numThreads)(None), syncSpec)
  }

  /** Log and perform the op in the two-step linearisation scheme. 
    * @tparam A The type of parameters to op; it should have a suitable 
    * definition of == and hashCode.
    * @tparam B The return type of op.
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
  def log2[A,B,C](
    log: ox.scl.LinearizabilityLog[TwoStepHomoLinSpec[A,B], C],
    me: ThreadID, cOp: C => B, string: String, x: A) 
  = {
    var y = null.asInstanceOf[B]
    log(c => {y = cOp(c)}, s"$me: $string", _.op(me,x))   // Perform op_1
    log(c => y, s"$me: _$string", _.opX(me))          // Log \overline{op_1}
  }
}

// =======================================================
 
import TwoStepHomoLinSpec.{SyncState,SyncSpecObject}

/** A specification object for the two-step linearisability technique.  This
  * assumes that we are testing for synchronisation linearisability of a
  * synchronisation object with operation 
  *   def op(x: A): B
  */
class TwoStepHomoLinSpec[A,B](
  private val state: SyncState[A], 
  private val returns: Array[Option[B]],
  private val syncSpec: SyncSpecObject[A,B] 
){
  import TwoStepHomoLinSpec.{Zero,One,ThreadID,isOne}

  /** The type of specification object in the linearizability tester. */
  private type LinSpec = TwoStepHomoLinSpec[A,B] // shorthand

  /** returns with t set to ob. */
  @inline private 
  def addToReturns(t: ThreadID, ob: Option[B]): Array[Option[B]] = {
    val newReturns = returns.clone; newReturns(t) = ob; newReturns
  }
  
  @inline private 
  def addTwoToReturns(t1: ThreadID, r1: Option[B], t2: ThreadID, r2: Option[B])
      : Array[Option[B]] = {
    val newReturns = returns.clone; newReturns(t1) = r1; 
    newReturns(t2) = r2; newReturns
  }



  /** Operation corresponding to op. 
  This may be invoked in either state, ie when there is no synchronisation in progress or when 
  another thread has already entered the synchronisation, so state can be Zero or One.
  However there cannot be a return value pending retrieval for the calling thread.
  */
  def op(t: ThreadID, x: A): (Unit, LinSpec) = {
    require(returns(t) == None)
    if (state == Zero()) ((), new LinSpec(One(t,x), returns, syncSpec))
    else { // state == One
      val One(t1,x1) = state
      val ((y1,y), newSyncSpec) = syncSpec.sync(x1,x)
      val newReturns = addTwoToReturns(t1, Some(y1), t, Some(y))
      ((), new LinSpec(Zero(), newReturns, newSyncSpec))
    }
  }


  /** Operation corresponding to \overline{op}. 
  This can only occur when no synchronisation is in progress, ie state == Zero(),
  and there is a value to return, ie a synchronisation has occurred but has not been completed.
  */
  def opX(t: ThreadID): (B, LinSpec) = {
    require(state == Zero() && returns(t).nonEmpty); val Some(y) = returns(t)
    (y, new LinSpec(state, addToReturns(t, None), syncSpec))
  }

  /* Need to override hashcode and equality. 
  Why?
  */

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
