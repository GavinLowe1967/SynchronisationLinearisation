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
    else if(tester == "ChanTwoStepTester") Array(2, 4, 8)
    else if(tester == "ABCTwoStepTester") Array(6, 9, 12)
    else if(tester == "CloseableChannelTwoStepTester") Array(2, 4, 8, 10)
    else if(tester == "BarrierTester") Array(2, 4, 8, 16)
    else if(tester == "BarrierTwoStepTester") Array(4, 8, 10)
    else if(tester == "ExchangerTester") Array(4, 8, 16, 32)
    else if(tester == "ExchangerTwoStepTester") Array(4, 8, 10)
    else Array(2, 4, 8, 16) 
  ).filter(_ <= maxP)

  def itersList = Array(2, 4, 8, 16, 32).filter(_ <= maxIters)

  /** Number of samples for statistical analysis. */
  var samples = 5

  def options = Array[String](
    "title = Experiment on the "+
      (if(timing) "time" else "number of invocations")+" taken to find a bug "+
      s"using $tester $flags",
    "ylabel = "+(if(timing) s"Time ($units)" else "Invocations"), 
    "xlabel = Number of iterations per thread",
    "ymin = 0", // "ymax = 6000",
    //if(countReps) "% ymax = 50000" else "% ymax = 4",
    "log basis x=2", "scaled ticks = false",
    "legend pos = north east", 
    "height = 0.8\\textheight", "width = 0.6\\textwidth"
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

  /** Definition of a set of experiments.  Each tuple represents the name of the
    * tester to run, flags to use, and the maximum number of iterations per
    * run. */
  val allExperiments = Array(
    ("ChanTester", "--faulty", 256),
    ("ChanTwoStepTester", "--faulty", 256),
    ("ABCTester", "--faulty", 256), 
    ("ABCTwoStepTester", "--faulty", 100), 
    ("BarrierTester", "--faulty4", 256),
    ("BarrierTwoStepTester", "--faulty4", 256),
      // Note: above two largely independent of the number of iterations,
      // since bug nearly always found on first run, second iteration.
    ("CloseableChanTester", "--faultyWrapped", 256),
    ("CloseableChannelTwoStepTester", "--faultyWrapped", 128)
  )
    // ("ExchangerTester", "--faulty", 512),
    // ("ExchangerTwoStepTester", "--faulty", 512),
    // The exchanger testers do one iteration per thread, so we don't want these. 

  /** Run all the experiments represented by `allExperiments`. */
  def doAllExperiments = {
    var graphString = ""
    for((t,f,mi) <- allExperiments){
      tester = t; flags = f; maxItersPerRun = mi
      graphString += "\n"+doMeasurement
    }
    val fname = "Results/BPN-all.tex"
    Graphs.writeStandAloneFile(fname, graphString)
    println(s"Results written to $fname")
  }

  /** Options with scaling experiments. */
  val options1 = options ++ Array(
    "xlabel = Number of threads",
    "title = Scaling experiment"
  )

  /** Run a single scaling experiment. */
  def doScalingExperiment(testers: Array[(String, Int)], flags: String, ps: Array[Int])
      : String = {
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val numTesters = testers.length
    val results = Array.ofDim[(Double,Double)](numTesters, ps.length)
    val options1 = options :+ "xlabel = Number of threads"
    // Run experiments
    for(ix <- 0 until numTesters){
      val (tester, pMax) = testers(ix)
      val cmd0 = 
        s"scala -cp $cp synchronisationTester.$tester $flags --iters 4 "+
          s"--timing --reps ${Int.MaxValue} "
      results(ix) = new Array[(Double,Double)](ps.length)
      for(pix <- 0 until ps.length){
        val p = ps(pix); val cmd = cmd0+s"-p $p"; println(cmd)
        def measure : Double = { // returns time in nanos
          val t = Experiments.timeProc(cmd, verbose=false)
          print(s"${t/Million}ms ") // print time in millis
          t.toDouble
        }
        if(p <= pMax){
          val (m,s) = Experiments.iterateMeasurement(measure, params)
          println("\n("+m/Million+"ms, "+s/Million+"ms)")
          results(ix)(pix) = (m/Million, s/Million)
        }
        else println("Omitted")
      }
    }
    // Print results
    for(ix <- 0 until numTesters){
      val (tester,_) = testers(ix)
      for(pix <- 0 until ps.length; if results(ix)(pix) != null){
        val (m,s) = results(ix)(pix)
        println(s"($tester, ${ps(pix)}): \t"+m+" +- "+s)
      }
    }
    // Produce graphs
    val labels = testers.map(_._1) // labels for plots
    // val pmax = testers.map(_._2.max).max
    val pLabels = ps.map(_.toString) // labels for x-axis
    Graphs.makeLinearGraph(options1, labels, pLabels, results)
  }

  /** An entry for the scaling experiments.  Each element of the array is the
    * name of a tester and the max number of threads to run. */
  type ScalingEntry = Array[(String, Int)]

  /** Data about the scaling experiments to do.  (1) An array holding names of
    * testers and the max number of threads to run; (2) flags; (3) the numbers
    * of threads to run. */
  val scalingExperiments = Array[(ScalingEntry, String, Array[Int])](
    (Array(("ChanTester", 32), ("ChanTwoStepTester", 20)),
      "--faulty", Array(2,4,8,12,16,20,22,24,28,32)),
    (Array(("ABCTester", 30), ("ABCTwoStepTester", 21)),
      "--faulty", Array(6,9,12,18,21,24,30)),
    (Array(("ExchangerTester", 32), ("ExchangerTwoStepTester", 10)),
      "--faulty", Array(4,6,8,10,12,16,20,24,28,32)),
    (Array(("BarrierTester", 32), ("BarrierTwoStepTester", 10)),
      "--faulty4", Array(4, 6, 8, 10, 12, 16, 24, 32)),
    (Array(("ChanCounterTester", 32), ("ChanCounterTwoStepTester", 16)),
      "--faulty", Array(4, 8, 12, 16, 18, 20, 24, 28, 32)) // maybe 18 for 2-step
  )

  // (Array(("TerminatingQueueTester", 14), ("TerminatingQueueTwoStepTester", 11)),
  //   "--faulty", Array(4, 8, 10, 11, 12, 14))
  // Note: Terminating Queue somewhat explodes with both testers.  I think
  // this is to do with the *queue* nature, rather than the termination.
  // However, it is very hard to profile for this reason.

  // (Array(("CloseableChanTester", 22), ("CloseableChannelTwoStepTester", 12)),
  //   "--faultyWrapped", Array(4, 8, 12, 16, 18, 20, 22)),
  // The Closeable Channel tester gives results that fluctuate wildly.  This
  // makes it hard to get reliable averages.  However, the direct tester is
  // more robust than the two-step tester.

  def doScalingExperiments = {
    var graphString = ""
    for((testers, flags, ps) <- scalingExperiments){
      val st = doScalingExperiment(testers, flags, ps)
      graphString += "\n"+st
    }
    val fname = "Results/scalingExperiment.tex"
    Graphs.writeStandAloneFile(fname, graphString)
    println(s"Results written to $fname")
  }

  def main(args: Array[String]) = {
    var i = 0; var doAll = false; var doSE = false
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
      case "--countReps" => countReps = true; i += 1
      case "--tester" => tester = args(i+1); i += 2
      case "--flags" => flags = args(i+1); i += 2
      case "--maxP" => maxP = args(i+1).toInt; i += 2
      case "--maxIters" => maxIters = args(i+1).toInt; i += 2 
      case "--maxItersPerRun" => maxItersPerRun = args(i+1).toInt; i += 2 
      case "--doAll" => doAll = true; i += 1
      case "--scalingExperiments" => doSE = true; i += 1
    }

    if(doAll) doAllExperiments
    else if(doSE) doScalingExperiments
    else{
      val graphString = doMeasurement
      val fname = "Results/"+
        (if(timing) s"BPN-$tester$flags.tex" else s"BPNCount-$tester.tex")
      val argsString = "% "+args.mkString(" ")+"\n"
      Graphs.writeStandAloneFile(fname, graphString+"\n"+argsString)
      println(s"Results written to $fname")
    }
  }

}
