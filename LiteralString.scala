class LiteralString(str: String) extends Token {
	def apply(): String = {
		str
	}
}
