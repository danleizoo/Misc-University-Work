/** A trait representing an unlabelled graph with nodes of type N. */
trait Graph[N]{
  /** The successors of node n. */
  def succs(n: N): List[N]
}
//==========================================================================

import io.threadcso._
import scala.collection.mutable.Stack

/** A partial stack that terminates if all worker threads are attempting to
  * pop, and the stack is empty.
  * @param numWorkers the number of worker threads. */
class TerminatingPartialStack[A](numWorkers: Int){
  /** Channel for enqueueing. */
  private val pushChan = ManyOne[A]

  private type ReplyChan = Chan[A]

  /** Channel for popping. */
  private val popChan = OneMany[ReplyChan]

  /** Channel for shutting down the stack. */
  private val shutdownChan = ManyOne[Unit]

  /** Push x.
    * @throws StopException if the stack has been shutdown. */
  def push(x: A): Unit = pushChan!x

  /** Attempt to pop a value.
    * @throws StopException if the stack has been shutdown. */
  def pop: A = {
    val reply = OneOne[A]
    popChan!reply
    reply?()
  }

  /** Shut down this stack. */
  def shutdown = attempt{ shutdownChan!(()) }{ }
  // Note: it's possible that the server has already terminated, in which case
  // we catch the StopException.

  /** The server process. */
  private def server = proc("server"){
    // Currently held values
    val stack = new Stack[A]()
    // Stack holding reply channels for current pop attempt.
    val waiters = new Stack[ReplyChan]()
    // Inv: stack.isEmpty or waiters.isEmpty
    // Termination: signal to all waiting workers
    def close = {
      for(c <- waiters) c.close
      pushChan.close; popChan.close; shutdownChan.close
    }

    serve(
      pushChan =?=> { x => 
        if(waiters.nonEmpty){ // pass x to a waiting dequeue
          assert(stack.isEmpty); waiters.pop!x
        }
        else stack.push(x)
      }
      |  
      popChan =?=> { reply =>
        if(stack.nonEmpty) reply!(stack.pop) // service request immediately
        else{
          waiters.push(reply)
          if(waiters.length == numWorkers) close
        }
      }
      |
      shutdownChan =?=> { _ => close }
    )
  }

  server.fork
}


// A simple implementation of partial solutions
class Partial{
  /** Array holding the digits played so far, with 0 representing a blank
    * square. */
  private val contents = Array.ofDim[Int](9,9)

  /** Initialise from a file. */
  def init(fname: String) = {
    val lines = scala.io.Source.fromFile(fname).getLines
    for(i <- 0 until 9){
      val line = lines.next
      for(j <- 0 until 9){
	val c = line.charAt(j)
	if(c.isDigit) contents(i)(j) = c.asDigit
	else { assert(c=='.'); contents(i)(j) = 0 }
      }
    }
  }

  /** Print. */
  def printPartial = {
    for(i <- 0 until 9){
      for(j <- 0 until 9) print(contents(i)(j))
      println 
    }
    println
  }

  /** Is the partial solution complete? */
  def complete : Boolean = {
    for(i <- 0 until 9; j <- 0 until 9) if(contents(i)(j) == 0) return false
    true
  }

  /** Find a blank position; precondition: complete returns false. */
  def nextPos: (Int,Int) = {
    for(i <- 0 until 9; j <- 0 until 9) if(contents(i)(j) == 0) return (i,j)
    throw new RuntimeException("No blank position")
  }

  /** Can we play value d in position (i,j); precondition: (i,j) is blank. */
  def canPlay(i:Int, j:Int, d:Int): Boolean = {
    // Check if d appears in row i
    for(j1 <- 0 until 9) if(contents(i)(j1) == d) return false
    // Check if d appears in column j
    for(i1 <- 0 until 9) if(contents(i1)(j) == d) return false
    // Check if d appears in this 3x3 block
    val basei = i/3*3; val basej = j/3*3
    for(i1 <- basei until basei+3; j1 <- basej until basej+3)
      if(contents(i1)(j1) == d) return false
    // All checks passed
    true
  }

  /** Create a new partial solution, extending this one by playing d in
    * position (i,j). */
  def play(i:Int, j:Int, d:Int) : Partial = {
    val p = new Partial
    for(i1 <- 0 until 9; j1 <- 0 until 9) 
      p.contents(i1)(j1) = contents(i1)(j1)
    p.contents(i)(j) = d
    p
  }
}


import scala.collection.mutable.{Set,Stack}

/** A trait representing an unlabelled graph with nodes of type N. */
//trait Graph[N]{
  /** The successors of node n. */
//  def succs(n: N): List[N]
//}

// -------------------------------------------------------

/** Abstract class representing graph search problems. */
abstract class GraphSearch[N](g: Graph[N]){
  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[N]
}

// -------------------------------------------------------

/** Sequential depth-first search of graph g. */
class SeqGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget.  This performs a tree-search, not storing the set of
    * nodes seen previously. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    // Stack storing nodes
    val stack = new Stack[N](); stack.push(start)

    while(stack.nonEmpty){
      val n = stack.pop
      for(n1 <- g.succs(n)){
        if(isTarget(n1)) return Some(n1) else stack.push(n1)
      }
    }
    None
  }
}


/** A graph corresponding to Sudoku problems.  From each node p of the graph,
  * there is a path to every solution of p.  Further, there is at most one
  * path between any pair of nodes p1 and p2. */
object SudokuGraph extends Graph[Partial]{
  /** The successors of a particular partial solution.
    * 
    * It is guaranteed that any solution of p is also a solution of a member
    * of succs(p), and vice versa.  Further, each element of succs(p) has
    * fewer blank squares than p.
    * 
    * Pre: !p.complete. */
  def succs(p: Partial): List[Partial] = {
    val (i,j) = p.nextPos
    (for(d <- 1 to 9; if p.canPlay(i, j, d)) yield p.play(i, j, d)).toList
  }
}


class ConcGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  /**The number of workers. */
  val numWorkers = 8

  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    val stack = new TerminatingPartialStack[N](numWorkers); stack.push(start)

    // Channel for worker telling the coordinator that it has found a solution
    val pathFound = ManyOne[N]
    var result: Option[N] = None

    // A single worker
    def worker = proc("worker"){
      repeat{
        val n = stack.pop
        for (n1 <- g.succs(n)) {
          if (isTarget(n1)) pathFound!(n1)
          else stack.push(n1)
        }
      }
      pathFound.close // tells coordinator to shutdown
    }

    def coordinator = proc("coordinator"){
      attempt{result = Some(pathFound?())}{ }
      stack.shutdown // close stack, causing workers to terminate
      pathFound.close // close pathFound in case another thread has found solution
    }

    val workers = || (for(_ <- 0 until numWorkers) yield worker)
    (workers || coordinator)()
    result
  }
}


// -------------------------------------------------------

/** A program for solving Sudoku problems, based on a GraphSearch object.  
  * 
  * This expects to find the filename for the starting position as the first
  * argument on the command line.  */
object Sudoku{
  def main(args: Array[String]) = {
    val fname = args(0)
    val useConc = args.length > 1 && args(1) == "--conc"
    val p = new Partial; p.init(fname)
    val g = SudokuGraph
    val solver: GraphSearch[Partial] = if(useConc) new ConcGraphSearch(g) else new SeqGraphSearch(g)
    solver(p, _.complete) match{
      case Some(p1) => p1.printPartial
      case None => println("No solution found")
    }
    io.threadcso.exit
  }
}
