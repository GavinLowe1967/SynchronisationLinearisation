package synchronisationTester

import scala.util.Random
import synchronisationTesting.{HistoryLog,StatelessTester}
import synchronisationObject.{
  AtomicBroadcastT, AtomicBroadcast, FaultyAtomicBroadcast}

object AtomicBroadcastTester extends Tester{
  type AB = AtomicBroadcastT[Int]

  /** The number of receivers involved in each synchronisation. */
  var n = 3

  trait LogEvent
  case class Send(x: Int) extends LogEvent
  case class Receive(id: Int) extends LogEvent

  /* A synchronisation will correspond to a Send event followed by n Receive
   * events, in increasing order of identities. */

  /** Is es a valid prefix of a synchronisation? */
  def isValid(es: List[LogEvent]) =
    es.head.isInstanceOf[Send] && isIncreasing(es.tail, 0)

  /** Is es a list of Receives with contiguous identities starting from id? */
  def isIncreasing(es: List[LogEvent], id: Int): Boolean = 
    es.isEmpty || es.head == Receive(id) && isIncreasing(es.tail, id+1)

  def matching: PartialFunction[List[LogEvent], List[Any]] = {
    case es if es.length == n+1 && isValid(es) =>
      val Send(x) = es.head; () :: List.fill(n)(x)
  } 

  /** A worker. */
  def worker(ab: AB)(me: Int, log: HistoryLog[LogEvent]) = {
    for(i <- 0 until iters)
      if(me == n){ val x = Random.nextInt(100); log(me, ab.send(x), Send(x)) }
      else log(me, ab.receive(), Receive(me))
  }

  /** Is es a suffix of a possible synchronisation?  I.e. either a complete
    * synchronisation or Receives where are the ids fields of the form [id..n)
    * for some id? */
  def suffixMatching(es: List[LogEvent]) : Boolean = es.head match{
    case Send(_) => es.length == n+1 && isIncreasing(es.tail, 0)
    case Receive(id) => es.length == n-id && isIncreasing(es.tail, id+1)
  }


  val Default = 0; val Faulty = 1
  var choice = Default
  var timeout = -1

  /** Do a single test. */
  def doTest = {
    val ab: AB = choice match{
      case Default => new AtomicBroadcast[Int](n)
      case Faulty => new FaultyAtomicBroadcast[Int](n)
    }
    val tester = new StatelessTester[LogEvent](
      worker(ab), n+1, List(n+1), matching, suffixMatching)
    if(progressCheck) tester(timeout) else tester()
  }

  def main(args: Array[String]) = {
    var reps = 1000; var i = 0
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--faulty" => choice = Faulty; i += 1
      case "--progressCheck" => 
        progressCheck = true; timeout = args(i+1).toInt; i += 2
     // case "--timing" => timing = true; i += 1
    }

    runTests(reps, false)
  }


}
