object which extends Builtin {
	override val name = "which"

	override def execute(args: List[String]) = {
		args.foreach(arg => {
			if(BuiltinManager.contains(arg))
				Printer ! Message(arg + ": shell built-in command")
			else {
				//there is a linux command which, so it might make more sense
				//to just run that here instead...
				Executor.findOnPath(arg) match {
					case Some(s) => Printer ! Message(s)
					case None => Printer ! Message(arg + " not found")
				}
			}
		})
	}
}
