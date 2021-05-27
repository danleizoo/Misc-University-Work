import io.threadcso._

object Prac1 {

  /** A single comparator, inputting on in0 and in1, and outputting on out0 ∗ (smaller value) and out1 (larger value). */
   def comparator(in0: ?[Int], in1: ?[Int], out0: ![Int], out1: ![Int ]): PROC = proc{
    var x, y = 0;
    
    repeat {
      (proc {x = in0?()} || proc {y = in1?()} ) ();
      // Compare the two numbers and output on correct channel
      if(x < y) 
        (proc {out0!(x)} || proc {out1!(y)}) ();
      else 
        (proc {out0!(y)} || proc {out1!(x)}) ();
    }
    out0.closeOut();
    out1.closeOut();
  }

  /** A sorting network for four values. */
  def sort4(ins: List[?[Int ]], outs: List [![ Int ]]): PROC = proc {
    require(ins.length == 4 && outs.length == 4)

    //Create new channels for communicating results of intermediate comparators
    var min02, max02, min13, max13, uppermid, lowermid = OneOne [Int]
    //Run the comparators in parallel
    val first = comparator(ins(0), ins(2), min02, max02) || comparator (ins(1), ins(3), min13, max13);
    val second = comparator (min02, min13, outs(0), uppermid) || comparator (max02, max13, lowermid, outs(3));
    val third = comparator (uppermid, lowermid, outs(1), outs(2));

    repeat { run (first || second || third); }
    
    //Close channels
    for(out <- outs) out.closeOut();
    for(in <- ins) in.closeIn();
  }

  /** Insert a value input on in into a sorted sequence input on ins.
  ∗ Pre: ins.length = n && outs.length = n+1, for some n >= 1.
  ∗ If the values xs input on ins are sorted, and x is input on in, then a
  ∗ sorted permutation of x::xs is output on ys. **/
  def insert(ins: List[?[Int]], in: ?[Int], outs: List[![Int]]): PROC =
  {
    val n = ins.length; require(n >= 1 && outs.length == n+1)
    //Recursive insertion
    ins match {
      case in1::Nil => comparator(in1, in, outs(0), outs(1))
      case in1::in_rest => {
        val larger_channel = OneOne[Int]
        comparator(in1, in, outs.head, larger_channel) || insert(in_rest, larger_channel, outs.tail)
      }
    }
    
  }

  /** Insertion sort. **/
  def insertionSort(ins: List [?[Int]], outs: List [![Int]]): PROC = {
    val n = ins.length; require(n >= 2 && outs.length == n)
    //Recursive pattern matching on the list of input channels
    ins match {
      case in1::in2::Nil => comparator(in1, in2, outs(0), outs(1))
      case in1::ins_rest =>
        {
          val rest_sorted = List.fill(outs.length-1)(OneOne[Int])
          insertionSort(ins_rest, rest_sorted) || insert(rest_sorted, in1, outs)
        }
    }
  }


  /** Input random numbers to outs **/
  def inputs(outs: List[![Int]], num_of_tests:Int): PROC =
    proc {
      for (t <- 1 to num_of_tests)
      {
        if(t > 1) channel?()
        val values = List.fill(outs.length)(scala.util.Random.nextInt(100))
        print("Input: ")
        for (i <- values.indices) print(values(i) + " ")
        print(" ... Output: ")
        for (i <- outs.indices) outs(i) ! values(i)
      }
      channel?()
      for (ch <- outs) ch.closeOut()
    }

  /** Input sorted random numbers to  outs **/
  def inputs_sorted(outs: List[![Int]], num_of_tests:Int): PROC =
    proc {
      for (t <- 1 to num_of_tests) {
        if(t > 1) channel?()
        val values = List.fill(outs.length)(scala.util.Random.nextInt(100)).sorted
        print("Input: ")
        for (i <- values.indices) print(values(i) + " ")
        print(" ... Output: ")
        for (i <- outs.indices) outs(i) ! values(i)
      }
      for (ch <- outs) ch.closeOut()
    }

  /** Prints everything from ins **/
  def outputs(ins: List[?[Int]]): PROC =
    proc {
      repeat {
        var arr = new Array[Int](ins.size)
        for (i <- ins.indices) arr(i) = ins(i)?()
        for (i <- ins.indices) print(arr(i) + " ")
        println()
        channel!()
      }
      for(in <- ins) in.closeIn()
    }

  val channel = OneOne[Unit]

  def main(args : Array[String]): Unit =
  {
    print("Input number of tests: ");
    var num_of_tests = scala.io.StdIn.readInt()

    println("1. Comparator test")
    println("2. Sort4 test")
    println("3. Insert test")
    println("4. Insertion sort test")
    print("Input test: ")
    var testnum = scala.io.StdIn.readInt()

    testnum match
    {
      case 1 => {
        println("1: Comparator test")
        val ins = List.fill(2)(OneOne[Int])
        val outs = List.fill(2)(OneOne[Int])
        val system = inputs(ins, num_of_tests) || comparator(ins(0), ins(1), outs(0), outs(1)) || outputs(outs)
        run(system)
      }
      case 2 => {
        println("2: Sort4 test")
        val ins = List.fill(4)(OneOne[Int])
        val outs = List.fill(4)(OneOne[Int])
        val system = inputs(ins, num_of_tests) || sort4(ins, outs) || outputs(outs)
        run(system)
      }
      case 3 => {
        println("3: Insert test")
        val ins = List.fill(9)(OneOne[Int])
        val outs = List.fill(10)(OneOne[Int])
        val chan_for_x = OneOne[Int]
        val x = 25 // The test value we are inserting
        val system = inputs_sorted(ins, num_of_tests) || proc{while(true) chan_for_x!x} || insert(ins, chan_for_x, outs) || outputs(outs)
        run(system)
      }
      case 4 => {
        println("Test question 4: Insertion Sort")
        val ins = List.fill(10)(OneOne[Int])
        val outs = List.fill(10)(OneOne[Int])
        val system = inputs(ins, num_of_tests) || insertionSort(ins, outs) || outputs(outs)
        run(system)
      }
    }
    sys.exit();
  }
}


