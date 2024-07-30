ackage experiments

import ox.gavin.experiments.{Experiments,Graphs}

/** Experiment to compare the two-step and special-purpose testers. */
object TwoStepExperiment{
  /** Classpath. */
  val cp = ".:/home/gavin/Scala/SCL:/home/gavin/Scala/Util"

  /** Basic command */
  val cmd0 = s"scala -cp $cp "

  /** Number of invocations per thread. */
  val iters = 6

  /** Number of repeats. */
  var reps = 10000

  /** Options for most testers. */
  def opts0 =  s"--reps $reps --iters $iters "

  /** Make command to run file.  */
  def mkCmd(file: String) = s"$cmd0 $file $maybeProgressCheck --timing"



}
