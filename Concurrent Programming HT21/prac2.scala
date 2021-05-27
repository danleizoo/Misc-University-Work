import io.threadcso._
import scala.language.postfixOps
import scala.util.Random

/** Simulation of the Dining Philosophers example. */
object Prac2 {
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def Eat = Thread.sleep(500)
  def Think = Thread.sleep(scala.util.Random.nextInt(900))
  def Pause = Thread.sleep(500)

  // Philosophers send "pick" and "drop" commands to their forks.
  type Command = Boolean
  val Pick = true; val Drop = false
 
  /** A left-handed philosopher. */
  def phil(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      Think
      println(me+" sits"); Pause
      left!Pick; println(me+" picks up left fork"); Pause
      right!Pick; println(me+" picks up right fork"); Pause
      println(me+" eats"); Eat
      left!Drop; Pause; right!Drop; Pause
      println(me+" leaves")
    }
  }

   /** A right-handed philosopher. */
  def philRight(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      Think
      println(me+" sits"); Pause
      // We pick up the right fork first
      right!Pick; println(me+" picks up right fork"); Pause
      left!Pick; println(me+" picks up left fork"); Pause
      println(me+" eats"); Eat
      right!Drop; Pause; left!Drop; Pause
      println(me+" leaves")
    }
  }
  
  val sits, leaves = ManyOne[Int]
   /** A philosopher that sends info to butler. */
  def philBut(me: Int, left: ![Command], right: ![Command]) = proc("Phil"+me){
    repeat{
      sits!(me);
      Think; 
      println(me+" sits"); Pause
      left!Pick; println(me+" picks up left fork"); Pause
      right!Pick; println(me+" picks up right fork"); Pause
      println(me+" eats"); Eat
      left!Drop; Pause; right!Drop; Pause
      println(me+" leaves"); leaves!(me);
    }
  }

  /** A  butler checking that less than N philosophers are seated.*/
  def butler():PROC = proc {
    var numSitting = 0; // Receiving on sits and leaves channels
    serve(
          (numSitting < N-1 && sits ) =?=> {x => numSitting += 1 }
          | (numSitting == N-1 && leaves) =?=> {x => numSitting -= 1}
      ) 
  }

  /** A single fork. */
  def fork(me: Int, left: ?[Command], right: ?[Command]) = proc("Fork"+me){
    serve(
      left =?=> {
        x => assert(x == Pick); val y = left?; assert(y == Drop)
      }
      |
      right =?=> {
        x => assert(x == Pick); val y = right?; assert(y == Drop)
      }
    )
  }

  /** Run the system. */
  def main(args : Array[String]) = { 
  	println("0. Unchanged problem")
  	println("1. With a right handed philosopher")
  	println("2. With a butler")
    val numSelected = scala.io.StdIn.readInt();

    numSelected match {
      case 0 => {
            /** The complete system for N philosophers. */
      		def system = {
        		val philToLeftFork, philToRightFork = Array.fill(5)(OneOne[Command])
        		// philToLeftFork(i) is from Phil(i) to Fork(i);
        		// philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
        		val allPhils = || ( 
          			for (i <- 0 until N)
          				yield phil(i, philToLeftFork(i), philToRightFork(i))
        			) 
        		val allForks = || ( 
          			for (i <- 0 until N) 
            			yield fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
        			)
        		allPhils || allForks
     		}

        system ();
      }

      case 1 => {
          /** When we have a right handed philosopher */
      		def system = {
          val philToLeftFork, philToRightFork = Array.fill(5)(OneOne[Command])
          // philToLeftFork(i) is from Phil(i) to Fork(i);
          // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
          val allPhils = || ( 
              for (i <- 0 until N-1)
                yield phil(i, philToLeftFork(i), philToRightFork(i))
        ) || philRight(N-1, philToLeftFork(N-1), philToRightFork(N-1))
          val allForks = || ( 
              for (i <- 0 until N) 
                yield fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
          )
          allPhils || allForks 
        }

        system ();
      }

      case 2 => {
        /** When we have a butler */
    		def system = {
      		val philToLeftFork, philToRightFork = Array.fill(5)(OneOne[Command])
      		// philToLeftFork(i) is from Phil(i) to Fork(i);
      		// philToRightFork(i) is from Phil(i) to Fork((i-1)%N)
      		val allPhils = || ( 
        		for (i <- 0 until N)
        			yield philBut(i, philToLeftFork(i), philToRightFork(i))
      		) 
      		val allForks = || ( 
        			for (i <- 0 until N)
          			yield fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
      		)
      		allPhils || allForks || butler
    		}
        system ();
      }
    }
    exit();
  }
}