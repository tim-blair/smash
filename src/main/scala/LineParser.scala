import scala.util.parsing.combinator._

object LineParser extends LineParsing {
	def process(arg: String): (String, List[String]) = {
		parse(arg) match {
			case Nil => ("", Nil)
			case x => {
				val resp = buildResponse(x)
				(resp.head, resp.tail)
			}
		}
	}

	def buildResponse(x: List[Token]): List[String] = {
		if( x == Nil )
			Nil
		else
			x.takeWhile(_.isStringToken).mkString :: buildResponse(x.dropWhile(_.isStringToken).drop(1))
	}
}
