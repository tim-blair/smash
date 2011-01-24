class which extends Builtin {
	override val name = "which"

	override def execute(args: List[String]) = {
		args.foreach(arg => {
			if(BuiltinManager.contains(arg))
				println(arg + ": shell built-in command")
			else {
				//there is a linux command which, so it might make more sense
				//to just run that here instead...
				Executor.findOnPath(arg) match {
					case Some(s) => println(s)
					case None => println(arg + " not found")
				}
			}
		})
	}
}
