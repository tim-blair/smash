import scala.util.parsing.combinator._

//For now, no backslash...

// \s means whitespace
// \w is word (letter, digit, underscore)
//Probably could be another parser...
class LineParser extends RegexParsers {

//Give me my whitespace!
//thanks to http://oldfashionedsoftware.com/2008/08/16/easy-parsing-in-scala/
	override def skipWhitespace = false

	def char: Parser[String] = """[-\w./+]""".r
	def ws: Parser[Unit] = """\s+""".r ^^ (x => Unit)
	//TODO: single quotes
	def string: Parser[String] = "\"" ~> rep("""[^"]""".r) <~ "\"" ^^ (_.mkString)
	def command: Parser[String] = rep(char) ^^ (_.mkString)
	def argument: Parser[String] = (rep(char) | string) ^^ {
		case x: String => x 
		case x :: xs => (x :: xs).mkString 
	}
	//If I trim I can ignore the opt(ws) at the start and end...
	def line: Parser[String ~ Option[List[String]]] = opt(ws) ~> command ~ 
		opt(rep(ws ~> argument)) <~ opt(ws)

	//repsep is rep with a separator
	//javatokenparsers has a stringliteral that matches "this" or "that"
	def parse(arg: String): (String, List[String]) = {
		val parsed = parseAll(line, arg.trim)
		val cmd = parsed match {
			//case ~(x:String, Some(list)) => (x, list)
			//case ~(x:String, None) => (x, Nil)
			case Success((x ~ Some(list)), in) => (x, list)
			case Success((x ~ None), in) => (x, Nil)
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
		cmd
	}
}
