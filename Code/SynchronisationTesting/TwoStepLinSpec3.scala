package synchronisationTesting

/** TwoStepLinSpec3 is an adaptation of TwoStepLinSpec for 3-way
  * synchronisations.  This allows for ternary heterogeneous synchronisation
  * where one each of operations op1, op2 and op3 synchronise.  */

object TwoStepLinSpec3{
  /** Representation of thread IDs. */
  type ThreadID = Int

  /** States used in the encoded state machine. 
  This state machine has 3 states because there are 3 operations synchronising,
  allowing for the states where no synchronisation has started; op1 has occurred; 
  and op2 has occurred but before op3.
  */
  trait SyncState[A1,A2]
  case class Zero[A1,A2]() extends SyncState[A1,A2]  // No synchronisation in progress
  case class One[A1,A2](t1: ThreadID, x1: A1) extends SyncState[A1,A2]  // op1 only has occurred
  case class Two[A1,A2](t1: ThreadID, x1: A1, t2: ThreadID, x2: A2) extends SyncState[A1,A2]  // op1 & op2 both.

/** Class to hold return values for completed split synchronisations.  Each
  * value may be empty (NoRet) or hold either B1 or B2. */
  trait RetVar[B1,B2]  // Optionally able to store values of type B1 or B2.
  case class NoRet[B1,B2]() extends RetVar[B1,B2]  // Nothing stored.
  case class SomeB1[B1,B2](y1: B1) extends RetVar[B1,B2]  // Value of type B1
  case class SomeB2[B1,B2](y2: B2) extends RetVar[B1,B2]  // Value of type B2

  /** Reduce state to value. */
  private def stateOf[A1,A2](state: SyncState[A1,A2]) =
    state match{ case Zero() => 0; case One(_,_) => 1; case Two(_,_,_,_) => 2 }

  /** A synchronisation specification object. */
  trait SyncSpecObject[A1,A2,A3,B1,B2,B3]{
    /** The results for a synchronisation between operations providing parameters
      * `x1` `x2` and `x3`.
      * @returns a tuple ((y1, y2, y3), newSyncSpec) where (y1, y2, y3) are 
      * the results that should be returned by the three operations, and
      * newSyncSpec is the new state of the SyncSpecObject.  */
    def sync(x1: A1, x2: A2, x3: A3)
        : ((B1,B2,B3), SyncSpecObject[A1,A2,A3,B1,B2,B3])
  }

  /** Factory method for a TwoStepLinSpec3 specification for `numThreads`
    * threads and using synchronisation specification object `syncSpec`. */
  def apply[A1,A2,A3,B1,B2,B3](
    numThreads: Int, syncSpec: SyncSpecObject[A1,A2,A3,B1,B2,B3])
      : TwoStepLinSpec3[A1,A2,A3,B1,B2,B3] =
    new TwoStepLinSpec3[A1,A2,A3,B1,B2,B3](
      Zero(), Array.fill(numThreads)(NoRet()), syncSpec)

  /** Factory method for a stateless TwoStepLinSpec3 specification for
    * `numThreads` threads and using synchronisation specification function
    * `sync1`. */
  def apply[A1,A2,A3,B1,B2,B3](numThreads: Int, sync1: (A1,A2,A3) => (B1,B2,B3))
      : TwoStepLinSpec3[A1,A2,A3,B1,B2,B3] = {
    val syncSpec = new SyncSpecObject[A1,A2,A3,B1,B2,B3]{ 
      def sync(x1:A1, x2: A2, x3: A3) = (sync1(x1, x2, x3), this) 
    }
    apply(numThreads, syncSpec)
    //new TwoStepLinSpec3[A1,A2,A3,B1,B2,B3](Zero(), Array.fill(numThreads)(NoRet), spec)
  }

  /** Log and perform the op_1 in the two-step linearisation scheme.  */
  def log1[A1,A2,A3,B1,B2,B3,C](
    log: ox.scl.LinearizabilityLog[TwoStepLinSpec3[A1,A2,A3,B1,B2,B3], C],
    me: ThreadID, cOp: C => B1, string: String, x: A1) 
  = {
    var y = null.asInstanceOf[B1]
    log(c => {y = cOp(c)}, s"$me: $string", _.op1(me,x))   // Perform op_1
    log(c => y, s"$me: _$string", _.op1X(me))          // Log \overline{op_1}
  }

  /** Log and perform the op_2 in the two-step linearisation scheme.  */
  def log2[A1,A2,A3,B1,B2,B3,C](
    log: ox.scl.LinearizabilityLog[TwoStepLinSpec3[A1,A2,A3,B1,B2,B3], C],
    me: ThreadID, cOp: C => B2, string: String, x: A2) 
  = {
    var y = null.asInstanceOf[B2]
    log(c => {y = cOp(c)}, s"$me: $string", _.op2(me,x))   // Perform op_2
    log(c => y, s"$me: _$string", _.op2X(me))          // Log \overline{op_2}
  }
}

// =======================================================
 
import TwoStepLinSpec3.{SyncState,SyncSpecObject,RetVar}



/** A specification object for the two-step linearisability technique.  This
  * assumes that we are testing for synchronisation linearisability of a
  * synchronisation object with operations 
  *   def op1(x1: A1): B1
  *   def op2(x2: A2): B2
  *   def op3(x3: A3): B3
  * where if three invocations synchronise, they should return y1, y2 and y3,
  * respectively, where (y1,y2,y3) = sync(x1,x2,x3).  
  * Parameters state and returns are as in the paper. 
  * 
  * ??? - ToDo : need to update the below.
  * @tparam A1 The type of parameters to op_1; it should have a suitable definition of == and hashCode.
  * @tparam B1 The return type of op_1.
  * @tparam A2 The type of parameters of op_2.
  * @tparam B2 The return type of op_2.
  * @param state The state of the automaton.
  * @param returns Array optionally holding return values for threads.
  * @param spec SpecObject whose sync function defines the result of synchronisations, mapping the pair of parameters to the pair of return values.  */
class TwoStepLinSpec3[A1,A2,A3,B1,B2,B3](
  private val state: SyncState[A1,A2], 
  private val returns: Array[RetVar[B1,B2]],
  private val syncSpec: SyncSpecObject[A1,A2,A3,B1,B2,B3] 
){
  import TwoStepLinSpec3.{Zero,One,Two,NoRet,SomeB1,SomeB2,ThreadID,stateOf}

  /** The type of specification object in the linearizability tester. */
  private type LinSpec = TwoStepLinSpec3[A1,A2,A3,B1,B2,B3] // shorthand

  /** returns with t1 and t2 set to relevant values. */
  @inline private 
  def addTwoToReturns(t1: ThreadID, r1: RetVar[B1,B2], t2: ThreadID, r2: RetVar[B1,B2]): Array[RetVar[B1,B2]] = {
    val newReturns = returns.clone; newReturns(t1) = r1; newReturns(t2) = r2; newReturns
  }

  @inline private 
  def addToReturns(t: ThreadID, r: RetVar[B1,B2]): Array[RetVar[B1,B2]] = {
    val newReturns = returns.clone; newReturns(t) = r; newReturns
  }

  /** Operation corresponding to op_1. */
  def op1(t1: ThreadID, x1: A1): (Unit, LinSpec) = {
    require(state == Zero() && returns(t1) == NoRet())
    ((), new LinSpec(One(t1,x1), returns, syncSpec))
  }

  /** Operation corresponding to op_2. */
  def op2(t2: ThreadID, x2: A2): (Unit, LinSpec) = {
    require(stateOf(state) == 1 && returns(t2) == NoRet())
    val One(t1,x1) = state
    ((), new LinSpec(Two(t1,x1,t2,x2), returns, syncSpec)) 
  }

  /** Operation corresponding to op_3. 
  This is the final and unsplit operation of the triple, which completes the synchronisation.
  */
  def op3(x3: A3): (B3, LinSpec) = {
    require(stateOf(state)==2); val Two(t1, x1, t2, x2) = state; 
    val ((y1,y2,y3), newSyncSpec) = syncSpec.sync(x1,x2,x3)
    (y3, new LinSpec(Zero(), addTwoToReturns(t1, SomeB1(y1), t2, SomeB2(y2)), newSyncSpec))
  }

  /** Operation corresponding to \overline{op_1}. */
  def op1X(t1: ThreadID): (B1, LinSpec) = {
    require(state == Zero() && returns(t1) != NoRet()); val SomeB1(y1) = returns(t1)
    (y1, new LinSpec(state, addToReturns(t1, NoRet()), syncSpec))
  }

  /** Operation corresponding to \overline{op_2}. */
  def op2X(t2: ThreadID): (B2, LinSpec) = {
    require(state == Zero() && returns(t2) != NoRet()); val SomeB2(y2) = returns(t2)
    (y2, new LinSpec(state, addToReturns(t2, NoRet()), syncSpec))
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
