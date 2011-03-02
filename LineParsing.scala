import scala.util.parsing.combinator._

//For now, no backslash, or any fanciness, really
trait LineParsing extends RegexParsers {
//Give me my whitespace!
//thanks to http://oldfashionedsoftware.com/2008/08/16/easy-parsing-in-scala/
//Prog in Scala says we can do: override val whiteSpace = "".r, too
	override def skipWhitespace = false

	// Not sure if '=' is valid for a cmd, but it is for an arg
	def chars: Parser[String] = """[-\w./+=]+""".r
	def ws: Parser[Unit] = """\s+""".r ^^ (x => Unit)
	//I don't like numbers in variables, so for now they're illegal
	def variable: Parser[String] = "$" ~> """[_a-zA-Z]+""".r ^^ (x => handleVariable(x))
	def str: Parser[String] = "'" ~> "[^']+".r <~ "'"
	def string: Parser[String] = "\"" ~> """[^"]+""".r <~ "\""
	def command: Parser[String] = variable | chars
	def argument: Parser[String] = (string | str | variable | chars)
	def line: Parser[String ~ Option[List[String]]] = 
		(command | failure("Could not parse command")) ~ 
			opt(rep(ws ~> argument))

	def handleVariable(x: String) =
		Environment.env.getOrElse(x, "")
}
