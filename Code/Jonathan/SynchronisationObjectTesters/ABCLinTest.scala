package synchronisationTester

import ox.cads.testing._
import scala.util.Random
import synchronisationObject._

package object global {
val p = 12  // No of threads.
}


// Common signature between concurrent and sequential implementations.
trait LinABCT{  
  def aSync(id:Int,value: Int): Unit  // id is thread number, value is "result".
  def aExit(id:Int):(Int,Int)        // id is needed in the log to check the a gets the correct result.
  def bSync(id:Int,value: Int): Unit  // id is thread number, value is "result".
  def bExit(id:Int):(Int,Int)        // id is needed in the log to check the b gets the correct result.
  def cSync(value: Int): (Int,Int)
}

class ConcABC(faulty:Boolean) extends LinABCT {

val abc = if (faulty) new FaultyABC[Int,Int,Int] else new ABC[Int,Int,Int]  // Datatype to test.  Note - it is unmodified.
val results = Array.ofDim[(Int,Int)](global.p)  // alternatively could use ThreadLocal.
  
  // results array does not need to be protected from concurrent access, 
  // because elements are only written and read by the owning thread.
  def aSync(id:Int,value: Int): Unit = {results(id) = abc.syncA(value);} 
  def aExit(id:Int):(Int,Int) = results(id)         
  def bSync(id:Int,value: Int): Unit = {results(id) = abc.syncB(value);} 
  def bExit(id:Int):(Int,Int) = results(id)         
  def cSync(value: Int): (Int,Int) = abc.syncC(value)
}

object ABCLinTest{
  var iters = 20 // Number of iterations by each worker
  val MaxVal = 10 // Maximum value placed in the queue
  val reps = 100

  // Type representing state of sequential ABC system.
  // This could be a class, but here we use the common idiom of defining a tuple type.
  type SeqABC = (Int,Int,Int,Int,Int,Array[(Int,Int)])  // (state,aid,aval,bid,bval,results)

// cSync() must be the last of 3 operations so must occur in state 2. 
// The other 2 may occur in any order.  
// We define bSync() to occur first, just to be different from the concurrent monitor implemention.

  def seqASync(id:Int,value: Int)(abc:SeqABC): (Unit, SeqABC) = {
val (state,aid,aval,bid,bval,results) = abc
require(state==1)
((),(2,id,value,bid,bval,results))
}

  def seqBSync(id:Int,value: Int)(abc:SeqABC): (Unit, SeqABC) = {
val (state,aid,aval,bid,bval,results) = abc
require(state==0)
((),(1,aid,aval,id,value,results))
}

def seqAExit(id:Int)(abc:SeqABC):((Int,Int),SeqABC) = {  
val (state,aid,aval,bid,bval,results) = abc
require(state==0)
(results(id),abc)
}

def seqBExit(id:Int)(abc:SeqABC):((Int,Int),SeqABC) = seqAExit(id)(abc)  

  def seqCSync(value: Int)(abc:SeqABC): ((Int,Int), SeqABC) = {
val (state,aid,aval,bid,bval,results) = abc
require(state==2)
val newresults = results.clone()
newresults(aid) = (bval,value)
newresults(bid) = (aval,value)
((aval,bval),(0,aid,aval,bid,bval,newresults))
}


  /** A worker for the ABC LinTesters */
  def worker(id: Int, log: GenericThreadLog[SeqABC, ConcABC]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt()+id*45207)
    for(i <- 0 until iters){
        val v = random.nextInt(MaxVal)  // a/b/c result.
      id%3 match {
case 0 => {        
        log.log(_.aSync(id,v), "async("+id+","+v+")", seqASync(id,v))
        log.log(_.aExit(id), "aexit("+id+")", seqAExit(id))
}
case 1 => {        
        log.log(_.bSync(id,v), "bsync("+id+","+v+")", seqBSync(id,v))
        log.log(_.bExit(id), "bexit("+id+")", seqBExit(id))
}
case 2 => {        
        log.log(_.cSync(v), "csync("+v+")", seqCSync(v))
}

}  // match {}
    }
  }

  def main(args: Array[String]) = {

   val faulty = false

    for(r <- 0 until reps){
      val seqABC = (0,0,0,0,0,Array.ofDim[(Int,Int)](global.p))
      val concABC = new ConcABC(faulty)

      val tester = LinearizabilityTester.JITGraph[SeqABC, ConcABC](
        seqABC, concABC, global.p, worker _)
      assert(tester() > 0)

      if(r%50 == 0) print(".")
    } // end of for loop
    println()
  }
}
