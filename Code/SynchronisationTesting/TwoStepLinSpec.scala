package synchronisationTesting

object TwoStepLinSpec{
  /** Representation of thread IDs. */
  type ThreadID = Int

  /** States used in the encoded state machine. */
  trait State[A]
  case class Zero[A]() extends State[A]
  case class One[A](t: ThreadID, x: A) extends State[A]

  /** Is state an instance of One? */
  private def isOne[A](state: State[A]) =
    state match{ case Zero() => false; case One(_,_) => true }

  /** Factory method for a TwoStepLinSpec specification for `numThreads` threads
    * and using synchronisation function `sync`. */
  def apply[A1,A2,B1,B2](numThreads: Int, sync: (A1,A2) => (B1,B2)) = 
    new TwoStepLinSpec[A1,A2,B1,B2](Zero(), Array.fill(numThreads)(None), sync)

  /** Log and perform the op_1 in the two-step linearisation scheme. 
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
 
import TwoStepLinSpec.{State}

/** A specification object for the two-step linearisability technique.  This
  * assumes that we are testing for synchronisation linearisability of a
  * synchronisation object with operations 
  *   def op1(x1: A1): B1
  *   def op2(x2: A2): B2
  * where if two invocations synchronise, they should return y1 and y2,
  * respectively, where (y1,y2) = sync(x1,x2).  Parameters state and returns 
  * are as in the paper. */
class TwoStepLinSpec[A1,A2,B1,B2](
  private val state: State[A1], 
  private val returns: Array[Option[B1]],
  sync: (A1,A2) => (B1,B2)
){
  import TwoStepLinSpec.{Zero,One,ThreadID,isOne}

  private type Spec = TwoStepLinSpec[A1,A2,B1,B2] // shorthand

  /** returns with t set to ob. */
  @inline private 
  def addToReturns(t: ThreadID, ob: Option[B1]): Array[Option[B1]] = {
    val newReturns = returns.clone; newReturns(t) = ob; newReturns
  }

  /** Operation corresponding to op_1. */
  def op1(t: ThreadID, x: A1): (Unit, Spec) = {
    require(state == Zero() && returns(t) == None)
    ((), new Spec(One(t,x), returns, sync))
  }

  /** Operation corresponding to op_2. */
  def op2(x2: A2): (B2, Spec) = {
    require(isOne(state)); val One(t,x1) = state; val (y1,y2) = sync(x1,x2)
    (y2, new Spec(Zero(), addToReturns(t, Some(y1)), sync))
  }

  /** Operation corresponding to \overline{op_1}. */
  def op1X(t: ThreadID): (B1, Spec) = {
    require(state == Zero() && returns(t).nonEmpty); val Some(y1) = returns(t)
    (y1, new Spec(state, addToReturns(t, None), sync))
  }

  /* Need to override hashcode and equality. */

  override def hashCode = {
    var h = state.hashCode
    for(i <- 0 until returns.length) h = h*73 + returns(i).hashCode
    h
  }

  override def equals(that: Any) = that match{
    case spec: Spec @unchecked =>
      spec.state == state && spec.returns.sameElements(returns)
  }

  override def toString = s"Spec($state, ${returns.mkString("<",",",">")}"
}
