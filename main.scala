object ParseLine {
	def main(args: Array[String]) = {
		val reader = new InputReader
		val parser = new LineParser
		val exec = new Executor
		val builtinMgr = new BuiltinManager
		
		def process():Unit = {
			print("> ")
			val line = reader.read
			if(line != "exit") {
				val (cmd, args) = parser.parse(line)
				if(!builtinMgr.handle(cmd, args))
					exec.execute(cmd, args)
				process()
			}
		}

		process()
		//TODO: catch the exceptions thrown by the parser
		//TODO: have a custom exception type
	}
}
