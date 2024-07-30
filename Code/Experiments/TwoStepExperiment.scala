package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to compare the two-step and special-purpose testers. */
object TwoStepExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Basic command */
  val cmd0 = s"scala -cp $cp "

  /** Number of invocations per thread. */
  // val iters = 6

  /** Number of repeats. */
  var reps = 1000

  /** Options for most testers. */
  // def opts0 =  s"--reps $reps --iters $iters "

  /** Make command to run file.  */
  def mkCmd(file: String) = s"$cmd0 $file --timing"

  val Million = 1000000

  /** Number of samples for statistical analysis. */ 
  var samples = 200

  /** List of: names of example, direct tester, two-step tester. */
  def linFiles = Array[(String,String,String)](
    // ("Synchronous channel", "ChanTester", "ChanTwoStepTester"),
// FIXME: uncomment
    ("Counter channel", "ChanCounterTester", "ChanCounterTwoStepTester")
  )

  /** Numbers of iterations to consider. */
  val iterss = Array(2,4,8,16,32)

  /** Base for number of repetitions: each thread will do this number of
    * iterations in total per observation. */
  val repsBase = 1024

  val options0 = Array[String](
    "xlabel = Number of iterations per thread",
    "ylabel = Time (ms)",
    "log basis x=2", "scaled ticks = false"
  )

  def doMeasurement() = {
    // Parameters for statistical analysis: fixed number of samples. 
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val files = linFiles; val length = files.length

    for(ix <- 0 until length){
      val (name, file1, file2) = files(ix)
      println(name)
      val results = Array.ofDim[(Double,Double)](2, iterss.length)
      for(fi <- List(0,1); ri <- 0 until iterss.length){
        val file = if(fi == 0) file1 else file2
        val iters = iterss(ri); val reps = repsBase/iters
        val cmd = 
          mkCmd(s"synchronisationTester.$file --iters $iters --reps $reps")
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
        results(fi)(ri) = (m/Million, s/Million)  // ms
      }

      // Print data
      for(fi <- List(0,1)){
        if(fi == 0) println("Direct:") else println("Two-step:")
        for(ri <- 0 until iterss.length)
          println(s"${iterss(ri)} \t "+results(fi)(ri))
      }

      // Create graphs
      val options = s"title = $name" +: options0
      val labels = Array("Direct", "Two-step")
      val fname = "twoStepExperiment"+name.filter(_ != ' ')+".tex"
      val graph = Graphs.makeLogXGraph(options, labels, iterss, results)
      Graphs.writeStandAloneFile(fname, graph)
      println(s"Output written to $fname")
    }
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
    }

    doMeasurement()
  }

}
