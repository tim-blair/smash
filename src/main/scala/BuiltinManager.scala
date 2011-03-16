object BuiltinManager {
	//This should really be a set/map, and we can just do a get...
	private val builtins: List[Builtin] = List(cd, which, tag, exit)

	lazy val names = for(b <- builtins) yield b.name

	def handle(cmd: String, args: String): Option[MainMessage] = {
		builtins.filter(b => b.name == cmd) match {
			case x :: xs => x.execute(args)
			case Nil => None
		}
	}

	def contains(cmd: String): Boolean =
		!builtins.filter(b => b.name == cmd).isEmpty
}
