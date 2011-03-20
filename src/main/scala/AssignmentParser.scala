import scala.util.parsing.combinator._

object AssignmentParser extends ItemParsing {
	override def chars: Parser[Token] = """[-\w./+]+""".r ^^ (x => new LiteralString(x))
	def equalsSign: Parser[Token] = "=" ^^ (x => new EqualsSign(x))
	def name: Parser[List[Token] ~ Option[List[Token]]] = token ~ opt(opt(ws) ~> (equalsSign ~> token))
	def args: Parser[List[List[Token] ~ Option[List[Token]]]] = rep1(name) <~ opt(ws)
	
	def parse(arg: String): List[(String, Option[String])] = {
		parseAll(args, arg) match {
			case Success(list, in) => list.map(_ match {
				case x ~ None => (x.mkString, None)
				case x ~ Some(y) => (x.mkString, Some(y.mkString))
			})
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}

