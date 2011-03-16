import scala.util.parsing.combinator._

trait ItemParsing extends RegexParsers {
//Give me my whitespace!
//thanks to http://oldfashionedsoftware.com/2008/08/16/easy-parsing-in-scala/
//Prog in Scala says we can do: override val whiteSpace = "".r, too
	override def skipWhitespace = false

	// Not sure if '=' is valid for a cmd, but it is for an arg
	def chars: Parser[Token] = """[-\w./+=]+""".r ^^ (x => new LiteralString(x))
	def ws: Parser[Token] = """\s+""".r ^^ (x => new WhiteSpace(x))
	//I don't like numbers in variables, so for now they're illegal
	def variable: Parser[Token] = "$" ~> """[_a-zA-Z]+""".r ^^ (x => new Variable(x))
	def str: Parser[Token] = "'" ~> "[^']+".r <~ "'" ^^ (x => new SingleString(x))
	def string: Parser[Token] = "\"" ~> """[^"]+""".r <~ "\"" ^^ (x => new DoubleString(x))

	def token: Parser[List[Token]] = opt(ws) ~> rep(variable | str | string | chars)
}
