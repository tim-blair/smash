object ParseLine {
	def main(args: Array[String]) = {
		val reader = new InputReader
		val parser = new LineParser
		val exec = new Executor
		
		def process():Unit = {
			print("> ")
			val line = reader.read
			if(line != "exit") {
				try {
					val (cmd, args) = parser.parse(line)
					if(BuiltinManager.contains(cmd))
						BuiltinManager.handle(cmd, args)
					else
						exec.execute(cmd, args)
				} catch {
					case e: Exception => println(e.getMessage)
				}
				process()
			}
		}

		process()
		//TODO: have a custom exception type
		//TODO: this doesn't work for things that run in the terminal (vim)
	}
}
