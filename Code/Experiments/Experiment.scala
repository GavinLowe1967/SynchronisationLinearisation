package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to test speed on a number of examples. */
object Experiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Basic command */
  val cmd0 = s"scala -cp $cp "

  /** Number of threads. */
  val p = 4
  // Note: Tester sets p = 4 as default; but ABC sets this to 6.

  /** Number of invocations per thread. */
  val iters = 6

  /** Number of repeats. */
  var reps = 10000

  /** Waiting time in progress checks, of a negative value if not doing progress
    * checks. */
  var progress = -1

  var doASAP = false


  // def options = s"  --reps $reps --iters $iters --timing"
  /** Options for most testers. */
  def opts0 =  s"--reps $reps --iters $iters "

  /** Are we doing a progress check? */
  def progressCheck = progress > 0

  /** Flags to produce a progress check, if appropriate. */
  def maybeProgressCheck =
    if(progressCheck) s"--progressCheck $progress" else ""

  /** Make command to run file. 
    * @param asap does this tester allow the "--doASAP" option? */
  def mkCmd(file: String, asap: Boolean) = {
    val maybeASAP = if(doASAP && asap) "--doASAP" else ""
    s"$cmd0 $file $maybeProgressCheck $maybeASAP --timing" // options
  }

  val Million = 1000000

  /** Number of samples for statistical analysis. */ 
  var samples = 10

  /** Round x to nearest Int. */
  def round1(x: Double): Int = (x+0.5).toInt

  /** List of names of examples, testers to check, and with extra flags to
    * include. */
  def linFiles = Array[(String,String,String,Boolean)](
    ("Synchronous channel", "ChanTester", opts0, false), 
    ("Filter channel", "FilterChanTester", opts0, false),
    ("Men and women", "MenWomenTester", opts0, false),
    { // Each thread does one op, so run more threads
      val p1 = p*iters; val reps1 = p*iters*reps/p1 
      ("Exchanger", "ExchangerTester", s"-p $p1 --reps $reps1", false) },
    { val m = Math.sqrt(p*iters/2).toInt; val n = m+1; 
      // Expected number of invocations per run
      val expInvs = if(progressCheck) (m-0.5)*n+m*(n-0.5) else 2*m*n
      val reps1 = (reps*iters*p/expInvs + 0.5).toInt
      ("Two families", "TwoFamiliesTester", s"-m $m -n $n --reps $reps1", true)},
    { val n = Math.sqrt(p*iters).toInt+1; 
      val expInvs = if(progressCheck) n*(n-1.5) else n*(n-1)
      val reps1 = (reps*iters*p/expInvs + 0.5).toInt
      ("One family", "OneFamilyTester", s"-n $n --reps $reps1 ", true) },
    { val p1 = 6; val iters1 = iters*p/p1; 
      ("ABC", "ABCTester", s"-p $p1 --iters $iters1 --reps $reps", false) },
    { val expInvs = if(progressCheck) p*iters-1 else p*iters
      val reps1 = round1(reps*iters*p/expInvs)
      ("Barrier", "BarrierTester", s"--iters $iters --reps $reps", false) },
    ("Timeout channel", "TimeoutChannelTester", opts0, false),
    ("Timeout exchanger", "TimeoutExchangerTester", opts0, false),
    ("Closeable channel", "CloseableChanTester", opts0, true),
    { val bound = p*iters*reps
      ("Terminating queue", "TerminatingQueueTester", s"--untilIters $bound", true) }
  ) 

  /* Notes.
   * 
   * In OneFamilyTester, each thread does n-1 iterations per run, so we set n
   * so that the total number of iterations per run is roughly the same as for
   * other testers, and adjust the number of runs. 
   * 
   * In TwoFamiliesTester, each thread does p/2-1 iterations per run
   * 
   * The TerminatingQueueTester doesn't do a fixed number of iterations per
   * run.  So we arrange for it to do roughly the same number of operations as
   * other testers.
   * 
   * For TimeoutChannelTester, the object itself is slower, so we omit this.
   */


  /** Flag to indicate to produce LaTeX output. */
  var latex = false

  /** Round x, representing a time in nanos, to the nearest millisecond. */
  def round(x: Double): Int = (x/Million+0.5).toInt

  def doMeasurement = {
    // Parameters for statistical analysis: fixed number of samples. 
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val files = linFiles
    val length = files.length; val results = new Array[(Double,Double)](length)

    for(ix <- 0 until length){
      val (name, file, opts, hasASAP) = files(ix)
      val cmd = mkCmd(s"synchronisationTester.$file $opts", hasASAP)
      println(cmd)
      def measure : Double = { // returns time in nanos
        val t = Experiments.timeProc(cmd, verbose=true)
        print(s"${t/Million} ms ") // print time in millis
        t.toDouble
      }
      val (m, s) =
        if(samples > 1) Experiments.iterateMeasurement(measure, params)
        else (measure, 0.0)
      println("\n("+m/Million+"ms, "+s/Million+"ms)")
      results(ix) = (m, s)  // nanos
    }

    for(ix <- 0 until length){
      val (name,file,ops,_) = files(ix); results(ix) match{
        case (m,s) =>
          print(name+"  \t")
          if(latex) print("& "+round(m)+"\t & ")
          else print(s"${round(m)}\t +- ")
          println(round(s))
          // println(name+s"  \t$m +- $s")
        case null => {}
      }
    }
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--progressCheck" => progress = args(i+1).toInt; i += 2
      case "--doASAP" => doASAP = true; i += 2
      // case "--progress" => progressCheck = true; i += 1
      case "--latex" => latex = true; i += 1
    }

    doMeasurement
  }

}
