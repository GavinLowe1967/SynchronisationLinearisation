package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

// Common signature between concurrent and sequential implementations.
trait LinMenWomenT{  
  // This version is asymmetric between men and women. A symmetric form is also possible.
  def manSync(id:Int,me: Int): Unit  // id is thread number, me is "name".
  def manExit(id:Int):Int            // id is needed in the log to check the man gets the correct woman.
  def womanSync(me: Int): Int
  def womanExit(): Unit  // Dummy (no-op) for precondition checking, and to match log sizes.
}

class ConcMW(faulty:Boolean) extends LinMenWomenT {

val mw = if (faulty) new FaultyMenWomen else new MenWomen  // Datatype to test.  Note - it is unmodified.
val names = Array.ofDim[Int](2)  // alternatively could use ThreadLocal.
  
  // names array does not need to be protected from concurrent access, 
  // because elements are only written and read by the owning thread.
  def manSync(id:Int,me: Int): Unit = {names(id/2) = mw.manSync(me);} 
  def manExit(id:Int):Int = names(id/2)         
  def womanSync(me: Int): Int = mw.womanSync(me)
  def womanExit(): Unit = {}

}

object MenWomenLinTest{
  var iters = 10 // Number of iterations by each worker
  val MaxVal = 50 // Maximum value placed in the queue

   // We need an immutable representation for the sequential state.
// This could be a class, but here we use the common form of defining a tuple type.
  // Type representing state of sequential MW system.
  type SeqMW = (Boolean,Int,Array[Int])  // (set,manid,names)

  def seqManSync(id:Int,me: Int)(mw:SeqMW): (Unit, SeqMW) = {  // id will be even for men.
val (set,manid,names) = mw
require(!set)
val newnames = names.clone()  // Must not alter original names.
newnames(id/2) = me
((),(true,id,newnames))
}

  def seqManExit(id:Int)(mw:SeqMW):(Int,SeqMW) = {  
val (set,manid,names) = mw
require(!set)
(names(id/2),mw) // woman name.
}

  def seqWomanSync(me: Int)(mw:SeqMW): (Int,SeqMW) = {  
val (set,manid,names) = mw
require(set)
val newnames = names.clone()  // Must not alter original names.
newnames(manid/2) = me        // Save my name in new array.
(names(manid/2),(false,manid,newnames))
}

  def seqWomanExit()(mw:SeqMW): (Unit, SeqMW) = {
val (set,manid,names) = mw
require(!set)
((),mw)
} 


  /** A worker for the MW LinTesters */
  def worker(id: Int, log: GenericThreadLog[SeqMW, ConcMW]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt()+id*45207)
    for(i <- 0 until iters){
        val x = random.nextInt(MaxVal)  // man/woman name.
      if(id%2 == 0){  // Man (me = thread id)
        log.log(_.manSync(id,x), "mansync("+id+","+x+")", seqManSync(id,x))
        log.log(_.manExit(id), "manexit("+id+")", seqManExit(id))
      }
      else {  // Woman
        log.log(_.womanSync(x), "womansync("+x+")", seqWomanSync(x))
        log.log(_.womanExit(), "womanexit()", seqWomanExit())
}
    }
  }

  def main(args: Array[String]) = {
    val reps = 100  // Number of repetitions
     val p = 4      // Number of workers, must be even (to ensure termination of test system)

   val faulty = false

    for(r <- 0 until reps){
      val seqMW = (false,0,Array.ofDim[Int](p/2))
      val concMW = new ConcMW(faulty)

      val tester = LinearizabilityTester.JITGraph[SeqMW, ConcMW](
        seqMW, concMW, p, worker _)
      assert(tester() > 0)

      if(r%50 == 0) print(".")
    } // end of for loop
    println()
  }
}
