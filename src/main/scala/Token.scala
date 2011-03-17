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
case class OpenSingleString(str: String) extends Token {
	override def toString = str
	override val isStringToken = true 
}
class DoubleStrToken(str: String) extends Token with ItemParsing {
	override def toString = out
	override val isStringToken = true 
	private lazy val out =
		parse(str)

	def literal: Parser[Token] = """[^$]+""".r ^^ (x => new LiteralString(x))
	def stringToken: Parser[List[Token]] = rep(variable | literal)

	def parse(arg: String): String = {
		parseAll(stringToken, arg) match {
			case Success(list, in) => list.map(_()).mkString
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
case class DoubleString(private val str: String) extends DoubleStrToken(str) {
}
case class OpenDoubleString(private val str: String) extends DoubleStrToken(str) {
	def tokenize(): List[Token] = {
		parseAll(stringToken, str) match {
			case Success(list, in) => list
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
case class LiteralString(str: String) extends Token {
	override def toString = str
	override val isStringToken = true 
}
case class EqualsSign(str: String) extends Token {
	override def toString = "="
}
