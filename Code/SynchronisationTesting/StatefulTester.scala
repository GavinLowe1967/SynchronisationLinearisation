package synchronisationTesting

import scala.collection.mutable.ArrayBuffer

/** A tester for synchronisation linearisation in the case of synchronisations
  * of arbitrary arity with a stateful synchronisation object.
  * 
  * @tparam Op the type representing operations on the synchronisation object.
  * @tparam S the type of the specification object.
  * 
  * @param worker definition of a worker on the synchronisation object,
  *  parameterised by its identity and the log it will write to.
  * @param p the number of threads to run.
  * @param arities the list of arities of synchronisations. 
  * @param specMatching a description of the results that should be given by a 
  *  particular list of operations synchronising and the corresponding state
  *  of the specification object.
  * @param suffixMatching a predicate saying whether a list of invocations could
  *  form a suffix of a synchronisation.
  * @param spec0 the initial state of the specification object.
  * @param doASAP should the ASAP optimisation be used?
  * @param verbose flag to give verbose output. */
class StatefulTester[Op,S](
  worker: (Int, HistoryLog[Op]) => Unit,
  p: Int, arities: List[Int],
  specMatching: S => PartialFunction[List[Op], (S,List[Any])],
  suffixMatching: List[Op] => Boolean = (es: List[Op]) => true,
  spec0: S, doASAP: Boolean = false, verbose: Boolean = false)
    extends StatefulTesterBase[Op,S](worker, p, arities, 
      specMatching, suffixMatching, spec0, doASAP, verbose){

  /** Test if the invocations represented by es can synchronise given state
    * `spec` of the specification.  `ops` is the `Op`s associated with `es`.
    * Optionally return the resulting state of the specification. */
  private def canSync(spec: S, es: List[CallEvent1], ops: List[Op])
      : Option[S] = {
    if(specMatching(spec).isDefinedAt(ops)){
      try{
        val (spec1, expected) = specMatching(spec)(ops)
        if(equalResults(expected, es)){
          if(verbose) println(s"Sync of $es with $spec")
          Some(spec1)
        }
        else None
      }
      catch{ case _: IllegalArgumentException => None }
    }
    else None
  }

  /** All possible synchronisations between invocations from `calls` given state
    * `spec` of the specification. */
  protected def allSyncs(spec: S, calls: List[CallEvent1]): List[SyncInfo] = {
    var result = List[SyncInfo]()
    val candidates = allArgsLists(calls, maxSyncArity)
    for(arity <- arities; (es,ops) <- candidates(arity))
      canSync(spec, es, ops) match{
        case Some(spec1) => result ::= (spec1, es)
        case None => {}
      }
    result
  }

  /** All possible synchronisations involving e and other invocations from
    * `calls` given state `spec` of the specification. */
  protected def allSyncsWith(spec: S, calls: List[CallEvent1], e: CallEvent1)
      : List[SyncInfo] = {
    require(doASAP && e.ret != null)
    var result = List[SyncInfo]()
    val candidates = allArgsListsWith(calls.filter(_ != e), e)
    for(arity <- arities; (es1, ops1) <- candidates(arity)) 
      canSync(spec, es1, ops1) match{ 
        case Some(spec1) => result ::= (spec1, es1)
        case None => {}
    }
    result
  }
}
