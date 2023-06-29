package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to investigate false positives with progress checks. */
object ProgressExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Basic command */
  val cmd0 = s"scala -cp $cp "

  var file = "ChanTester"

  /** Number of threads. */
  val p = 4

  var iters = 4

  /** Number of repeats. */
  var reps = 1000

  /** Number of samples for statistical analysis. */ 
  var samples = 10

  var delay = 30

  //var runs = 100

  val Million = 1_000_000

  def options = 
    s" -p $p --iters $iters --reps $reps --progressCheck $delay --timing"

  def doTest = { 
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val cmd = cmd0+s"synchronisationTester.$file $options"
    println(cmd)
    def measure: Double = {
      val res = Experiments.timeProc(cmd, verbose=true)
      println(s"${res/Million} ms")
      res.toDouble
    }
    val (m, s) =
      if(samples > 1) Experiments.iterateMeasurement(measure, params)
      else (measure, 0.0)
    println("\n("+m/Million+"ms, "+s/Million+"ms)")

  }

  def main(args: Array[String]): Unit = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      //case "--runs" => runs = args(i+1).toInt; i += 2
      case "--delay" => delay = args(i+1).toInt; i += 2
      case "--file" => file = args(i+1); i += 2
      case arg => println(s"Illegal argument: $arg"); sys.exit()
    }

    doTest
  }

}
