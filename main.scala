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
		//TODO: have a printing obj (actor?)
		//BUG: ctl-C kills the shell
	}
}

object MainActor extends Actor {
	val parser = new LineParser
	val exec = new Executor
	def act() {
		loop {
			react {
				case Next => {
					Printer ! Prompt
					InputBuilder ! ReadLine(this)
				}
				case Line(line) => {
					if(line == "exit") {
						Printer ! Stop
						InputReader ! Stop
						InputBuilder ! Stop
						exit()
					} else if(line != "") {
						try {
							val (cmd, args) = parser.parse(line)
							if(BuiltinManager.contains(cmd))
								BuiltinManager.handle(cmd, args)
							else
								//TODO: make this a message
								exec.execute(cmd, args)
						} catch {
							case e: Exception => Printer ! Message(e.getMessage)
						}
						//TODO: this should come from executor
						this ! Next
					}
				}
			}
		}
	}
}
