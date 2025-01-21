package experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to test speed on a number of examples. */
object BugFinderExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Basic command */
  val cmd0 = s"scala -cp $cp "

  /** Number of threads. */
  val p = 4

  /** Number of invocations per thread. */
  val iters = 4

  /** Number of repeats. */
  val reps = Int.MaxValue // 1000000 

  /** Waiting time in progress checks, of a negative value if not doing progress
    * checks. */
  var progress = -1

  def options = 
    s" --reps $reps --iters $iters --timing "+
      (if(progress > 0) s"--progressCheck $progress" else "") 

  /** Make command to run file. */
  def mkCmd(file: String) = cmd0+file+options

  val Million = 1000000 

  /** Number of samples for statistical analysis. */
  var samples = 5

  /** Files for synchronisation linearisation testing. */
  val linFiles = Array(
    ("Synchronous channel", "ChanTester --faulty"),    
    ("Synchronous channel two-step", "ChanTwoStepTester --faulty"),    

    ("Men and women", "MenWomenTester --faulty"),     
    ("Men and women two-step", "MenWomenTwoStepTester --faulty"),  
 
    // With following two, each thread does one op, so run more threads
    { val p1 =  p*2; ("Exchanger", s"ExchangerTester --faulty -p $p1") },  
    // This explodes for p1 more than about 10!
    { val p1 =  p*2;
      ("Exchanger two-step", s"ExchangerTwoStepTester --faulty -p $p1") },

    ("Counter channel", "ChanCounterTester --faulty"),    
    ("Counter channel two-step", "ChanCounterTwoStepTester --faulty"),

    ("Two families", s"TwoFamiliesTester --faulty -m ${p/2} -n ${p/2}"),
    ("Two families two-step", 
      s"TwoFamiliesTwoStepTester --faulty -m ${p/2} -n ${p/2}"),

    ("One family", "OneFamilyTester --faulty"),     
    // ("One family two-step", "OneFamilyTwoStepTester --faulty"),   
     
    ("ABC", "ABCTester --faulty -p 6"),         
    ("ABC two-step", "ABCTwoStepTester --faulty -p 6"), 
        
    ("Timeout channel", "TimeoutChannelTester --faulty"), 
    ("Timeout channel two-step", "TimeoutChannelTwoStepTester --faulty"), 

    ("Timeout exchanger", "TimeoutExchangerTester --faulty2"), 
    ("Timeout exchanger two-step", "TimeoutExchangerTwoStepTester --faulty2"),
 
    ("Closeable channel", "CloseableChanTester --faultyWrapped"), 
    ("Closeable channel two-step", 
      "CloseableChannelTwoStepTester --faultyWrapped"),

    ("Barrier", "BarrierTester --faulty4"),
    ("Barrier two-step", "BarrierTwoStepTester --faulty4"),

    ("Resignable Barrier", "ResignableBarrierTester --faulty"),
    ("Resignable Barrier two-step", "ResignableBarrierTwoStepTester --faulty"),

    ("Terminating Queue", "TerminatingQueueTester --faulty"),
    ("Terminating Queue two-step", "TerminatingQueueTwoStepTester --faulty")
  )

  // Missing for two-step: OneFamily.

  /** Flag to indicate to produce LaTeX output. */
  var latex = false

  /** Files for progress checking. */
  val progressFiles = Array(
    ("Synchronous channel", "ChanTester --deadlock"),
    ("Filter channel", "FilterChanTester --nonProgressing"),
    ("Men and women", "MenWomenTester --deadlock"),
    ("One family", "OneFamilyTester --deadlock"),
    ("ABC", "ABCTester --deadlock"),             
    ("Barrier", "BarrierTester --faulty3"),            
  )


  /** Round x, representing a time in nanos, to the nearest millisecond. */
  def round(x: Double): Int = (x/Million+0.5).toInt

  def doMeasurement = {
    val files = if(progress > 0) progressFiles else linFiles
    val len = files.length
    val results = new Array[(Double, Double)](len) // will hold results
    // Parameters for statistical analysis: fixed number of samples. 
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)

    // Run the experiments
    for(ix <- 0 until len){
      val (_, file) = files(ix)
      val cmd = mkCmd("synchronisationTester."+file); println(cmd)
      def measure : Double = { // returns time in nanos
        val t = Experiments.timeProc(cmd, verbose=false)
        print(s"${t/Million} ms ") // print time in millis
        t.toDouble
      }
      val (m, s) = Experiments.iterateMeasurement(measure, params)
      println("\n("+m/Million+"ms, "+s/Million+"ms)")
      results(ix) = (m, s)
    }

    // Print results
    for(ix <- 0 until len){
      val (m,s) = results(ix); val (name,_) = files(ix)
      print(name+"  \t")
      if(latex) println("& "+round(m)+"\t & "+round(s)+"\\\\")
      else println(s"${round(m)}\t +- "+round(s))
    }
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
      case "--progressCheck" => progress = args(i+1).toInt; i += 2
      case "--latex" => latex = true; i += 1
    }

    doMeasurement
  }

}
