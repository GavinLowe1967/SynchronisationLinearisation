package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to test how varying parameters affects the time taken to find a
  * bug. */
object BugParametersExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Number of repeats. */
  val reps = Int.MaxValue // 1000000 

  var countReps = false

  def timing = !countReps

  /** Tester to use. */
  var tester = "ABCTester"

  var flags = "--faulty"

  /** Max # threads to run. */
  var maxP = Int.MaxValue

  /** Max iters per thread on each run. */
  var maxIters = Int.MaxValue
    
  var maxItersPerRun = 256

  /** Basic command */
  def cmd0 = 
    s"scala -cp $cp synchronisationTester.$tester $flags --reps $reps "

  val Million = 1000000
  val Billion = 1_000_000_000

  /* The raw results are in nanoseconds.  We scale these by dividing by
   * `scalar`, so that they are in units given by `units`. */

  def scaler = if(false && tester == "ABCTester") Billion else Million

  def units = if(false && tester == "ABCTester") "s" else "ms"

  /** Numbers of threads to use. */
  def ps = (
    if(tester == "ABCTester") Array(6, 9, 15, 24) // 12, 18, 24)
    else if(tester == "CloseableChanTester") Array(2, 4, 8, 12)
    else Array(2, 4, 8, 16) // 16 -- ClosableChan slow here
  ).filter(_ <= maxP)

  def itersList = Array(2, 4, 8, 16, 32).filter(_ <= maxIters)

  /** Number of samples for statistical analysis. */
  var samples = 5

  def options = Array[String](
    "title = Experiment on the "+
      (if(timing) "time" else "number of invocations")+" taken to find a bug",
    "ylabel = "+(if(timing) s"Time ($units)" else "Invocations"), 
    "xlabel = Number of iterations per thread",
    // "ymin = 0", 
    if(countReps) "% ymax = 50000" else "% ymax = 4",
    "log basis x=2", "scaled ticks = false",
    "legend pos = north east", 
    "height = 0.5\\textheight", "width = 0.6\\textwidth"
  )

  /** Perform the experiment.  Return a string representing the graph. */
  def doMeasurement: String = {
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val numPs = ps.length; val numIters = itersList.length
    // results will hold the results: if timing, then the time in seconds;
    // otherwise the total number of invocations by all threads.
    val results = Array.ofDim[(Double, Double)](numPs, numIters) 
    val cmd1 = cmd0+(if(countReps) "--countReps " else "--timing ")

    // Run the experiments
    for(ixp <- 0 until numPs; ixi <- 0 until numIters){
      val p = ps(ixp); val iters = itersList(ixi)
      val cmd = s"$cmd1 -p $p --iters $iters"; println(cmd)
      def measure : Double = { // returns time in nanos
        val t = Experiments.timeProc(cmd, verbose=false)
        if(timing) print(s"${t/Million}ms ") // print time in millis
        else print(s"$t ")  // print # runs to find bug
        t.toDouble
      }
      val itersPerRun = p*iters
      // Testing is very slow outside the following range
      if(itersPerRun > 2 && itersPerRun <= maxItersPerRun){
        val (m, s) = Experiments.iterateMeasurement(measure, params)
        if(timing) println("\n("+m/Million+"ms, "+s/Million+"ms)")
        else println(s"\n($m, $s)")
        if(timing) results(ixp)(ixi) = (m/scaler, s/scaler) // units of `units`
        else results(ixp)(ixi) = (m*itersPerRun, s*itersPerRun)
      }
      else{ println("Skipped"); results(ixp)(ixi) = null }
    }

    // Print results
    for(ixp <- 0 until numPs; ixi <- 0 until numIters)
      if(results(ixp)(ixi) != null){
        val (m,s) = results(ixp)(ixi)
        println(s"(${ps(ixp)}, ${itersList(ixi)}): \t"+m+" +- "+s)
      }

    // Produce graphs
    val labels = ps.map(p => s"p = $p")
    Graphs.makeLogXGraph(options, labels, itersList, results)
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
      case "--countReps" => countReps = true; i += 1
      case "--tester" => tester = args(i+1); i += 2
      case "--flags" => flags = args(i+1); i += 2
      case "--maxP" => maxP = args(i+1).toInt; i += 2
      case "--maxIters" => maxIters = args(i+1).toInt; i += 2 
      case "--maxItersPerRun" => maxItersPerRun = args(i+1).toInt; i += 2 
    }

    val graphString = doMeasurement
    val fname = if(timing) s"BPN-$tester$flags.tex" else s"BPNCount-$tester.tex"
    val argsString = "% "+args.mkString(" ")+"\n"
    Graphs.writeStandAloneFile(fname, graphString+"\n"+argsString)
    println(s"Results written to $fname")
  }

}
