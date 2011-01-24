class BuiltinManager {
	//This should really be a set/map, and we can just do a get...
	val builtins = List(new cd)

	def handle(cmd: String, args: List[String]) = {
		builtins.filter(b => b.name == cmd) match {
			case x :: xs => { x.execute(args); true }
			case Nil => false
		}
	}
}
