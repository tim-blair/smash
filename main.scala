object ParseLine {
	def main(args: Array[String]) = {
		val reader = new InputReader
		val parser = new LineParser
		val exec = new Executor
		
		//TODO: need to handle /path/to/cmd without adding . to the path
		//TODO: tail recursion this
		var keepGoing = true
		while(keepGoing) {
			print("> ")
			val line = reader.read
			if(line == "exit")
				keepGoing = false
			else {
				val (cmd, args) = parser.parse(line)
				exec.execute(cmd, args)
			}
		}
		//TODO: catch the exceptions thrown by the parser
		//TODO: have a custom exception type
	}
}
