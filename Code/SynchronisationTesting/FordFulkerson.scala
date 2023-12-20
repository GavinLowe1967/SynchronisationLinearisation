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

  // The sets X and Y, as Arrays. 
  private val X = (0 until N).toList.filter(i => inX(i)).toArray
  private val Y = (0 until N).toList.filter(i => !inX(i)).toArray

  def inY(n: Node) = !inX(n)

  /* We represent a flow by the following two variables.  */
  /** Array showing whether there is a flow between a pair of nodes. */
  private val between = Array.ofDim[Boolean](N,N)
  /** Array showing whether there is flow from the source node to a node in X,
    * or from a node in Y to the target. */
  private val toEnds = Array.ofDim[Boolean](N)

  // Representation of a path; stored in reverse order, and implicitly
  // starting at source node.
  type Path = List[Node]

  /** Try to find  an augmenting path of the flow starting from x in X. */
  private def findPath(x: Node): Path = {
    val seen = new Array[Boolean](N); val queue = new Queue[Path]
    queue += List[Node](x) 
    var result: Path = null // holds augmenting path if non-null
    while(result == null && queue.nonEmpty){
      val path = queue.dequeue(); val n = path.head
      if(inX(n)){ // edges to a node in Y with no flow
        for(j <- edges(n); if !between(n)(j) && !seen(j)){
          seen(j) = true; queue += j::path
        }
      }
      else if(!toEnds(n)) result = path.reverse 
      else // edges back to an X node, against the flow
        for(i <- edges(n); if between(i)(n) && !seen(i)){
          seen(i) = true; queue += i::path
        }
    }
    result 
  }

  /** Augment the flow based on path. */
  private def augment(path0: Path) = {
    var path = path0; val start = path.head; assert(inX(start) && !toEnds(start))
    toEnds(start) = true
    while(path.length > 1){
      val n1 = path.head; val n2 = path.tail.head; path = path.tail
      if(inX(n1)){ assert(inY(n2) && !between(n1)(n2)); between(n1)(n2) = true }
      else{ assert(inX(n2) && between(n2)(n1)); between(n2)(n1) = false }
    }
    val n = path.head; assert(inY(n)); toEnds(n) = true;
  }

  /** Run the Ford-Fulkerson algorithm.
    * @return true if a complete matching is found.  Also return the maximal 
    * matching found (complete or not). */
  def apply(): (Boolean, Array[Int]) = {
    var done = false; var flow = 0
    for(x <- X){ // IMPROVE
      val path = findPath(x)
      if(path != null){ augment(path); flow += 1 }
    }
    // Build the matching
    val matching = Array.fill(N)(-1)
    for(i <- X; j <- edges(i)) if(between(i)(j)){ 
      assert(toEnds(i) && toEnds(j)); matching(i) = j; matching(j) = i
    }
    (2*flow == N, matching) // Has this covered all the X nodes? 
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
    val (ok,matching) = new FordFulkerson(inX, edges)()
    assert(ok)

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
    val (ok2,matching2) = new FordFulkerson(inX, edges2)()
    assert(ok2)
  }

}
