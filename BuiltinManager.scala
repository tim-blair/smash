object BuiltinManager {
	//This should really be a set/map, and we can just do a get...
	private val builtins: List[Builtin] = List(new cd, new which)

	def names =
		for(b <- builtins) yield b.name

	def handle(cmd: String, args: List[String]) = {
		builtins.filter(b => b.name == cmd) match {
			case x :: xs => { x.execute(args); true }
			case Nil => false
		}
	}

	def contains(cmd: String): Boolean =
		!builtins.filter(b => b.name == cmd).isEmpty
}
