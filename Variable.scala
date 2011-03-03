class Variable(name: String) extends Token {
	//TODO: strip $
	def apply(): String = {
		Environment.env.getOrElse(name, "")
	}
}
