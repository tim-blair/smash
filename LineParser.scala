import scala.util.parsing.combinator._

//For now, no backslash, or any fanciness, really
class LineParser extends RegexParsers {
//Give me my whitespace!
//thanks to http://oldfashionedsoftware.com/2008/08/16/easy-parsing-in-scala/
//Prog in Scala says we can do: override val whiteSpace = "".r, too
	override def skipWhitespace = false

	// Not sure if '=' is valid for a cmd, but it is for an arg
	def char: Parser[String] = """[-\w./+=]""".r
	def ws: Parser[Unit] = """\s+""".r ^^ (x => Unit)
	//TODO: single quotes
	def string: Parser[List[String]] = "\"" ~> rep("""[^"]""".r) <~ "\""
	def command: Parser[String] = rep(char) ^^ (_.mkString)
	def argument: Parser[String] = (string | rep(char)) ^^ {
		case x :: xs => (x :: xs).mkString 
		case Nil => "" //empty list => empty string
	}
	def line: Parser[String ~ Option[List[String]]] = 
		(command | failure("Could not parse command")) ~ 
			opt(rep(ws ~> argument))

	def parse(arg: String): (String, List[String]) = {
		val parsed = parseAll(line, arg.trim)
		val cmd = parsed match {
			case Success((x ~ Some(list)), in) => (x, list)
			case Success((x ~ None), in) => (x, Nil)
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
		cmd
	}
}
