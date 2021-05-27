// Template for the Sleeping Tutor practical

import io.threadcso._
import io.threadcso.debug.Log
import scala.util.Random

/** The trait for a Sleeping Tutor protocol. */
trait SleepingTutor{
  /** A tutor waits for students to arrive. */
  def tutorWait

  /** A student arrives and waits for the tutorial. */
  def arrive
  
  /** A student receives a tutorial. */
  def receiveTute

  /** A tutor ends the tutorial. */
  def endTeach
}

// =======================================================

import scala.util.Random

class SleepingTutorMonitor extends SleepingTutor{
  private var tutorAvailable = false
  private var tutorDone = false
  private var numOfStudents = 0 
  private val monitor = new Monitor

  private val tutorAvailableC, bothHereC, tutorDoneC, studentLeftC = monitor.newCondition

  def tutorWait = monitor.withLock{
  	tutorAvailable = true
  	tutorAvailableC.signalAll() // signal to the students
  	bothHereC.await()	// wait for both students to arrive
  }

  def arrive = monitor.withLock {
  	numOfStudents = numOfStudents + 1
  	tutorAvailableC.await(tutorAvailable) // wait for tutor to become available
    	if (numOfStudents == 2 ){
    		bothHereC.signalAll() // wake tutor up once both students arrived
    	}
  }

  def receiveTute = monitor.withLock {
  	if(!tutorDone) tutorDoneC.await() // wait for tutor to finish  
  	tutorDone = false // prepare for next tutorial
  	studentLeftC.signalAll() // signal students to leave
  }

  def endTeach = monitor.withLock {
  	tutorDone = true
  	tutorDoneC.signalAll() // wake up students
  	studentLeftC.await() // wait for tutor to finish teaching
  	numOfStudents = 0	// reset for next tutorial
  	tutorAvailable = false
  }
}

object SleepingTutorSimulation{
  private val st: SleepingTutor = new SleepingTutorMonitor

  def student(me: String) = proc("Student"+me){
    while(true){
      Thread.sleep(Random.nextInt(2000))
      println("Student "+me+" arrives")
      st.arrive
      println("Student "+me+" ready for tutorial")
      st.receiveTute
      println("Student "+me+" leaves")
    }
  }

  def tutor = proc("Tutor"){
    while(true){
      println("Tutor waiting for students")
      st.tutorWait
      println("Tutor starts to teach")
      Thread.sleep(1000)
      println("Tutor ends tutorial")
      st.endTeach
      Thread.sleep(1000)
    }
  }

  def system = tutor || student("Alice") || student("Bob")


}



object SleepingTutorTest {
    private val st: SleepingTutor = new SleepingTutorMonitor
    private val log = new debug.Log[LogEvent](3)
    private var ended = false

    private def student(me: String) = proc("Student"+me){
        // Give Alice and Bob meInt values 1 and 2 for logging
        val meInt = if (me == "Alice") 1 else 2 
        while (!ended) {
            Thread.sleep(Random.nextInt(1000))
            log.add(meInt, Arriving(me))
            st.arrive
            st.receiveTute
            log.add(meInt, DidLeave(me))
        }
    }

    private def tutor = proc("Tutor"){
        while (!ended) {
            st.tutorWait
            log.add(0, StartedTeaching)
            Thread.sleep(Random.nextInt(1000))
            log.add(0, FinishedTeaching)
            st.endTeach
            Thread.sleep(Random.nextInt(1000))
        }
    }

    private def system = tutor || student("Alice") || student("Bob")

    private def isValidSequence(events: Array[LogEvent]): Boolean = {
        var studentsPresent: Int = 0
        var tutorTeaching = false

        var error = false
        var i = 0
        while (i < events.length && !error) {
            events(i) match {
                case Arriving(student) => {
                    if (tutorTeaching) error = true // Violated condition 1
                    studentsPresent += 1
                }
                case StartedTeaching => {
                    if (studentsPresent < 2) error = true // Violated condition 1
                    tutorTeaching = true
                }
                case FinishedTeaching => {
                    if (studentsPresent < 2) error = true // Violated condition 2
                    tutorTeaching = false
                }
                case DidLeave(student) => {
                    if (tutorTeaching) error = true // Violated conditon 2
                    studentsPresent -= 1
                }
            }
            i += 1
        }
        !error
    }

    /** Runs the test for timeInSecs given as a command line argument */
    def main(args: Array[String]) = {
        require(args.length == 1)
        val timeInSecs: Int = args(0).toInt

        ended = false
        system.fork
        println("Started test, waiting for " +timeInSecs+ " seconds")
        Thread.sleep(timeInSecs*1000)
        ended = true

        val ranCorrectly = isValidSequence(log.get)
        if (ranCorrectly) println("Test passed")
        else println("Test failed")
    }
}

abstract class LogEvent
case class Arriving(student: String) extends LogEvent
case object StartedTeaching extends LogEvent
case object FinishedTeaching extends LogEvent
case class DidLeave(student: String) extends LogEvent