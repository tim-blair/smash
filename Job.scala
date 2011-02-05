import scala.actors._
import scala.actors.Actor._

class Job(pb: ProcessBuilder) extends Actor {
	def act() {
		pb.redirectErrorStream(true)
		val proc = pb.start
		val procInputHandler = new ProcessInputHandler(proc)
		val procOutputHandler = new ProcessOutputHandler(proc)
		val jobDoneAlerter = new ProcessWaiter(proc, this)

		procInputHandler.start()
		procOutputHandler.start()
		jobDoneAlerter.start()

		loop {
			println("Job is ready for a command")
			react {
				case Stop => {
					println("Job got a stop")
					proc.destroy()
					procInputHandler ! Stop
					MainActor ! Next
					exit()
				}
				case Done => {
					println("Job got a done")
					procInputHandler ! Stop
					MainActor ! Next
					exit()
				}
			}
		}
	}
}

//TODO: rumour has it that this may need to be in cooked mode...
//Victory? vim/top don't work because they need a tty... so I'll have to figure
//that one out
class ProcessInputHandler(proc: Process) extends Actor {
	def act() = {
		val out = proc.getOutputStream()
		//def write(Array[Byte): Unit
		loop {
			InputReader ! ReadIfAvail(this)
			reactWithin(100) {
				case Character(c) => {
					out.write(Array(c.toByte))
					//This is probably going to absolutely destroy performance
					out.flush
				}
				case Backspace => {
					out.write(Array('\177'.toByte))
					//This is probably going to absolutely destroy performance
					out.flush
				}
				case Stop => exit()
			}
			reactWithin(0) {
				case Stop => exit()
			}
		}
	}
}

class ProcessOutputHandler(proc: Process) extends Actor {
	def act() = {
		val in = proc.getInputStream()
		val buffer = new Array[Byte](1024)
		Stream.continually(in.read(buffer))
			.takeWhile(_ != -1)
			.foreach(x => Printer ! Output(new String(buffer)))
	}
}

class ProcessWaiter(proc: Process, parent: Actor) extends Actor {
	def act() = {
		//proc.exitValue will return the exit code of proc
		Printer ! "Ready to wait"
		loop {
			try {
				proc.exitValue
				Printer ! "Job done"
				parent ! Done
				exit()
			} catch {
				case e: Exception => Thread.sleep(500)
			}
		}
	}
}
