import scala.util.parsing.combinator._
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
case class DoubleString(str: String) extends Token with RegexParsers {
	override def toString = out
	override val isStringToken = true 
	private lazy val out =
		parse(str)
	//I don't like numbers in variables, so for now they're illegal
	//TODO: these 2 are the same as LineParsing
	override def skipWhitespace = false
	def variable: Parser[Token] = "$" ~> """[_a-zA-Z]+""".r ^^ (x => new Variable(x))
	def literal: Parser[Token] = """[^$]""".r ^^ (x => new LiteralString(x))
	def tokens: Parser[List[Token]] = rep(variable | literal)

	def parse(arg: String): String = {
		parseAll(tokens, arg) match {
			case Success(list, in) => list.map(_()).mkString
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
case class LiteralString(str: String) extends Token {
	override def toString = str
	override val isStringToken = true 
}
