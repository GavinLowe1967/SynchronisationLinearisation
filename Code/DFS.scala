package synchronisationTesting

/** Try to find a total matching on the graph whose nodes are
  * [0..edges.length), and such that node i has an edge to node j for each j
  * in edges(i).  Pre: the graph is symmetric (undirected). */
class DFS(edges: Array[Array[Boolean]]){

  type Node = Int

  type Matching = Array[Node]

  /** Number of nodes. */
  private val size = edges.length

  /** Perform a depth-first search.  Return a triple (ok, bestMatching, stuckOn)
    * such that: ok is true if we found a complete matching; bestMatching is
    * the best matching we found; stuckOn is the node we got stuck on, if
    * any. */
  def apply(): (Boolean, Matching, Node) = rec(0, false)

  /** Which nodes have been matched. */
  private val matched = new Array[Boolean](size)

  /** A heuristic for which node to expand next, with a lower value being
    * better: the number of edges from n to unmatched nodes; or size if n is
    * already matched. */
  private def score(n: Node) = 
    if(matched(n)) size
    else{
      var c = 0; 
      for(n1 <- 0 until size) if(edges(n)(n1) && !matched(n1)) c += 1
      c
    }

  /** The current matching. */
  private var matching: Matching = Array.fill[Node](size)(-1)

  /** What was the deepest level reached in the search so far? */
  private var bestDepth = 0

  /** The best matching found. */
  private var bestMatching: Matching = null

  /** Which node did we get stuck on (if any)? */
  private var stuckOn: Node = -1

  /** Perform a recursive depth-first search.  At each stage we expand a node
    * with the minimum score (minimum number of edges to unmatched nodes). */
  private def rec(depth: Int, isBest: Boolean): (Boolean, Matching, Node) = {
    if(matched.forall(_ == true)) (true, bestMatching, -1)
    else{
      // Unmatched node with fewest edges
      val node = (0 until size).minBy(score); assert(!matched(node))
      var matchable = false // have we found a possible match for node?
      for(node2 <- 0 until size;
          if node2 != node && !matched(node2) && edges(node)(node2)){
        // Try matching node with node2
        matchable = true
        assert(edges(node2)(node) && !matched(node) && !matched(node2) && 
          matching(node) == -1 && matching(node2) == -1)
        matched(node) = true; matched(node2) = true
        matching(node) = node2; matching(node2) = node
        val newBest = (depth+1 > bestDepth)
        if(newBest){ bestDepth = depth+1; bestMatching = matching.clone }
        val recResult @ (ok,_,_) = rec(depth+1, newBest)
        if(ok) return recResult
        else{  // backtrack
          matched(node) = false; matched(node2) = false
          matching(node) = -1; matching(node2) = -1
        }
      } // end of for loop
      if(!matchable && isBest) stuckOn = node 
      (false, bestMatching, stuckOn)
    }
  }

}
