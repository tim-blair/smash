import scala.util.parsing.combinator._

object LineParser extends RegexParsers with LineParsing {
	def parse(arg: String): (String, List[String]) = {
		parseAll(line, arg.trim) match {
			case Success((x ~ Some(list)), in) => (x, list)
			case Success((x ~ None), in) => (x, Nil)
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
