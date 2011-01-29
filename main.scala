object ParseLine {
	def main(args: Array[String]) = {
		val parser = new LineParser
		val exec = new Executor
		Printer.start()
		
		def process():Unit = {
			Printer ! Prompt
			val line = InputReader.read
			if(line == "exit") {
				Printer ! Stop
			} else {
				try {
					val (cmd, args) = parser.parse(line)
					if(BuiltinManager.contains(cmd))
						BuiltinManager.handle(cmd, args)
					else
						exec.execute(cmd, args)
				} catch {
					case e: Exception => Printer ! Message(e.getMessage)
				}
				process()
			}
		}

		process()
		//TODO: have a custom exception type
		//TODO: this doesn't work for things that run in the terminal (vim)
		//TODO: have a printing obj (actor?)
		//BUG: ctl-C kills the shell
	}
}
