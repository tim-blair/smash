trait Token {
	def apply(): String = toString
	val isStringToken = false
}
case class Variable(name: String) extends Token {
	override def toString = Environment.env.getOrElse(name, "")
	override val isStringToken = true 
}
case class WhiteSpace(space: String) extends Token {
	override def toString = " "
}
case class SingleString(str: String) extends Token {
	override def toString = str
	override val isStringToken = true 
}
case class DoubleString(str: String) extends Token {
	//TODO: strip quotes, and interpret variables inside
	override def toString = str
	override val isStringToken = true 
	//TODO: probably extend regexparsing so I can interpret strings correctly
}
case class LiteralString(str: String) extends Token {
	override def toString = str
	override val isStringToken = true 
}
