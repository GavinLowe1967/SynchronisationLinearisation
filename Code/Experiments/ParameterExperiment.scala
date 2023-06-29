package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to test how varying parameters affects the time taken to find a
  * bug. */
object ParameterExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Number of repeats. */
  var reps = 10_000

  /** Tester to use. */
  var tester = "ChanTester"

  var flags = ""

  /** Basic command */
  def cmd0 = 
    s"scala -cp $cp synchronisationTester.$tester $flags --reps $reps --timing "

  val Million = 1_000_000
  val Billion = 1_000_000_000

  /** Number of samples for statistical analysis. */
  var samples = 5

  /** Numbers of threads to use. */
  def ps = 
    if(tester == "ABCTester") Array(6, 9, 12, 15, 30) 
    else Array(2, 4, 8, 16) // 16 -- ClosableChan slow here

  val itersList = Array(2, 4, 8, 16, 32) // 5, 10, 20, 30)

  def options = Array[String](
    "title = Experiment on the running time of "+tester,
    "ylabel = Time (ms)",
    "xlabel = Number of iterations per thread",
    "ymin = 0", 
    // if(countReps) "% ymax = 50000" else "% ymax = 4",
    "log basis x=2", "scaled ticks = false",
    "legend pos = north west", 
    "height = 0.5\\textheight", "width = 0.6\\textwidth"
  )

  /** Perform the experiment.  Return a string representing the graph. */
  def doMeasurement: String = {
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val numPs = ps.length; val numIters = itersList.length
    // results will hold the results: if timing, then the time in seconds;
    // otherwise the total number of invocations by all threads.
    val results = Array.ofDim[(Double, Double)](numPs, numIters) 

    // Run the experiments
    for(ixp <- 0 until numPs; ixi <- 0 until numIters){
      val p = ps(ixp); val iters = itersList(ixi)
      val cmd = s"$cmd0 -p $p --iters $iters"; println(cmd)
      def measure : Double = { // returns time in nanos
        val t = Experiments.timeProc(cmd, verbose=false)
        print(s"${t/Million} ms ") // print time in millis
        t.toDouble
      }
      val itersPerRun = p*iters
      // Testing is very slow outside the following range
      if(itersPerRun > 2 && itersPerRun < 200){
        val (m, s) = Experiments.iterateMeasurement(measure, params)
        println("\n("+m/Million+"ms, "+s/Million+"ms)")
        results(ixp)(ixi) = (m/Million, s/Million) // ms
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
      case "--reps" => reps = args(i+1).toInt; i += 2
      // case "--countReps" => countReps = true; i += 1
      case "--tester" => tester = args(i+1); i += 2
      case "--flags" => flags = args(i+1); i += 2
    }

    val graphString = doMeasurement
    val fname = s"paramExp-$tester.tex"
    val argsString = "%"+args.mkString(" ")
    Graphs.writeStandAloneFile(fname, graphString+"\n"+argsString)
    println(s"Results written to $fname")
  }
}
