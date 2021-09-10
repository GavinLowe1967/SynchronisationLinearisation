package synchronisationTesting

import scala.collection.mutable.Queue

/** Run the Ford Fulkerson Algorithm on a bipartite graph with nodes defined
  * by nodeInfo and edges defined by edges.
  * 
  * More precisely, the nodes are {source,target} U [0..N) where N =
  * inX.length.  The left nodes X are those i in [0..N) such that inX(i); and
  * the right nodes Y are those i in [0..N) such that !inX(i).  The edges are
  * (1) (i,j) s.t. i in X and j in edges(i); (2) (source,i) for i in X; (3)
  * (j,target) for j in Y.  For each (i,j) in case (1), edges(j) contains
  * corresponding i.  */
class FordFulkerson(inX: Array[Boolean], edges: Array[List[Int]] ){
  val N = inX.length
  require(edges.length == N)
  // All edges between X and Y, and in agreement.
  require((0 until N).forall(i =>
    if(inX(i)) edges(i).forall(j => !inX(j) && edges(j).contains(i)) 
    else edges(i).forall(j => inX(j) && edges(i).contains(j))
  ))

  type Node = Int

  // The sets X and Y, as Lists. 
  private val X = (0 until N).toList.filter(i => inX(i))
  private val Y = (0 until N).toList.filter(i => !inX(i))

  def inY(n: Node) = !inX(n)

  /* We represent a flow by
   * - An N by N bit map `between` indicating flow from X to Y node
   * - A size N bit map `toEnds` showing the flow from source to nodes in X,
   *   or from nodes in Y to the target.
   */

  // Representation of a path; stored in reverse order, and implicitly
  // starting at source node.
  type Path = List[Node]

  /** Try to form an augmenting path of the flow represented by between and
    * toEnds. */
  private def augment(between: Array[Array[Boolean]], toEnds: Array[Boolean])
      : Path = {
    val seen = new Array[Boolean](N); val queue = new Queue[(Node, Path)]
    // Start at nodes in X with no flow
    for(i <- X; if !toEnds(i)){ seen(i) = true; queue += ((i, List[Node](i))) }
    while(queue.nonEmpty){
      val (n,path) = queue.dequeue
      if(inX(n)) // edges to a node in Y with no flow
        for(j <- edges(n); if !between(n)(j) && !seen(j)){
          seen(j) = true; queue += ((j, j::path))
        }
      else if(!toEnds(n)) return(path.reverse) // IMPROVE
      else // edges back to an X node, against the flow
        for(i <- edges(n); if between(i)(n) && !seen(i)){
          seen(i) = true; queue += ((i, i::path))
        }
    }
    null
  }

  /** Run the Ford-Fulkerson algorithm.
    * @return true if a complete matching is found. */
  def apply(): Boolean = {
    val between = Array.ofDim[Boolean](N,N); val toEnds = Array.ofDim[Boolean](N)
    var done = false; var flow = 0
    while(!done){
      var path = augment(between, toEnds)
      if(path != null){
        println(s"Augmenting with $path")
        // Augment the flow based on path
        val start = path.head; assert(inX(start) && !toEnds(start))
        toEnds(start) = true
        while(path.length > 1){
          val n1 = path.head; val n2 = path.tail.head; path = path.tail
          if(inX(n1)){
            assert(inY(n2) && !between(n1)(n2)); between(n1)(n2) = true
          }
          else{ assert(inX(n2) && between(n2)(n1)); between(n2)(n1) = false }
        }
        val n = path.head; assert(inY(n)); toEnds(n) = true; flow += 1
      }
      else done = true // failed to augment
    }
    println(s"flow = $flow")
    for(i <- X; j <- Y) if(between(i)(j)){ 
      assert(toEnds(i) && toEnds(j)); print(s"${(i,j)} ")
    }
    println
    2*flow == N
  }

}

// ==================================================================

/** Companion object. */
object FordFulkerson{
  // Test of algorithm
  def main(args: Array[String]) = {
    // Test graph: X = [0..N/2), Y = [N/2..N), edges (i,i+N/2) for i in X
    val N = 10; assert(N%2 == 0)
    val inX = Array.tabulate(N)(i => 2*i < N)
    val edges = Array.tabulate(N)(i => List((i+N/2)%N))
    new FordFulkerson(inX, edges)()

    // Test graph: X = [0..N/2), Y = [N/2..N), edges (i,i+N/2), (i,i+1+N/2)
    // for i in [0..N/2-1) and (N/2-1,N/2).  For N = 10: (0,5), (0,6), (1,6),
    // (1,7),... (3,8), (3,9), (4,5).
    val edges2 = Array.tabulate(N)(i => (
      if(i < N/2-1) List(i+N/2, i+1+N/2)
      else if(i == N/2-1) List(N/2)
      else if(i == N/2) List(0, N/2-1)
      else if(i < N-1) List(i-N/2, i-N/2-1)
      else List(i-N/2-1)
    ))
    if(new FordFulkerson(inX, edges2)()) println("Succeeded") 
    else println("Failed")

  }

}
