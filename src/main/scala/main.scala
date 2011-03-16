import scala.actors._

object ParseLine {
	def main(args: Array[String]) = {
		InputReader.start()
		Printer.start()
		InputBuilder.start()
		MainActor.start()

		MainActor ! Next
		
		//TODO: have a custom exception type
		//TODO: this doesn't work for things that run in the terminal (vim)
		//BUG: ctl-C kills the shell
	}
}

object MainActor extends Actor {
	val exec = new Executor
	def act() {
		loop {
			react {
				case Next => {
					InputReader ! Raw
					Printer ! Prompt
					InputBuilder ! ReadLine(this)
				}
				case Stop => {
					Printer ! Stop
					InputReader ! Stop
					InputBuilder ! Stop
					exit()
				}
				case Line(line) => {
					InputReader ! Cook
					if(line != "") {
						try {
							val (cmd, remainder) = CommandParser.parseCommand(line)
							if(BuiltinManager.contains(cmd)) {
								BuiltinManager.handle(cmd, remainder) match {
									case Some(msg) => this ! msg
									case None => this ! Next
								}
							} else
								//TODO: make this a message
								exec.execute(cmd, remainder)
						} catch {
							case e: Exception => Printer ! Message(e.getMessage)
							this ! Next
						}
					} else { //empty line
						Printer ! Message("")
						this ! Next
					}
				}
			}
		}
	}
}
