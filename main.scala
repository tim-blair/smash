object ParseLine {
	def main(args: Array[String]) = {
		val reader = new InputReader
		val parser = new LineParser
		val exec = new Executor
		
		var keepGoing = true
		while(keepGoing) {
			val line = reader.read
			if(line == "exit")
				keepGoing = false
			else {
				val (cmd, args) = parser.parse(line)
				//println("Command: " + cmd + " args: " + args.mkString)
				exec.execute(cmd, args)
			}
		}
		//TODO: parser should be here
		//TODO: catch the exceptions thrown by the parser
		//TODO: have a custom exception type
	}
}
