package synchronisationTester

/** Base class for testers, combining common code. */
trait Tester{
  /** Number of worker threads to run. */
  var p = 4

  /** Number of iterations per worker thread. */
  var iters = 20

  /** Do a single test.  Return true if it passes.  Defined in subclasses.  */
  def doTest: Boolean

  /** Do we check the progress condition? */
  var progressCheck = false

  /** Run `reps` tests.
    * @param timing are we doing timing experiments?
    * @param countReps are we doing an experiment counting the number of 
    * repetitions?  */
  def runTests(reps: Int, timing: Boolean, countReps: Boolean = false) = {
    val start = java.lang.System.nanoTime; var i = 0
    while(i < reps && doTest){ 
      i += 1; if(i%100 == 0 || progressCheck) print(".")
      // if(timing && i%100 == 0) println(".")
    }
    if(timing){
      val duration = java.lang.System.nanoTime - start // nanos
      println(); println(duration)
    }
    else if(countReps){ println(); println(i+1) }
    else{
      val duration = (java.lang.System.nanoTime - start)/1_000_000 // ms
      println(); println(s"$duration ms")
    }  
  }

  /** Run `reps` tests.  Print 1 if all successful else print Int.MaxValue. */
  def expectTrue(reps: Int) = {
    var i = 0
    while(i < reps && doTest){ 
      i += 1; if(i%100 == 0 || progressCheck) print(".")
    }
    println()
    if(i == reps) println(1) else println(Int.MaxValue)
  }

  // def parseArgs(args: Array[String], specials: PartialFunction[String, (Unit, Int)]) = {
  //   var i = 0

  //   while(i < args.length) args(i) match{
  //     case "-p" => p = args(i+1).toInt; i += 2
  //     case "--iters" => iters = args(i+1).toInt; i += 2
  //     // case "--reps" => reps = args(i+1).toInt; i += 2
  //     case arg => 
  //       println(arg)
  //       if(specials.isDefinedAt(arg)){ val (_, inc) = specials(arg); i += inc; println(inc) }
  //       else println(s"Illegal argument: $arg"); sys.exit()
  //   }

  // }

}
